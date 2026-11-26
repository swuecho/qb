(* [%sql "SELECT ..."]: SQL checked against the schema when the file is
   compiled, as a Qb.stmt with its types worked out. *)

open Ppxlib
open Qb_pg_parse

let schema_paths = ref []
let tables = ref None
let bigint = ref `Int

let () =
  Driver.add_arg "-qb-schema"
    (Arg.String (fun p -> schema_paths := !schema_paths @ [ p ]))
    ~doc:
      "PATH A schema file, or a directory whose *.sql files are replayed in \
       name order (migrations). A relative PATH is looked for in the folder of \
       the file being compiled, then in each folder above it up to the project \
       root. Repeat for several.";
  Driver.add_arg "-qb-tables"
    (Arg.String (fun m -> tables := Some m))
    ~doc:
      "MODULE The module holding the tables qb-pg-parse gen wrote for the same \
       schema; t.* then reads that table's row type.";
  Driver.add_arg "-qb-bigint"
    (Arg.Symbol
       ( [ "int"; "int64" ],
         fun s -> bigint := if s = "int64" then `Int64 else `Int ))
    ~doc:"TYPE The OCaml type BIGINT is read as: int (the default) or int64."

(* Where a relative path is looked for: the folder of the file being
   compiled, then each one above it, up to the project root. That root is the
   first folder holding a dune-project or, in a build, the build context
   (_build/default): dune copies only preprocessor_deps there, so the ppx
   never reads a schema file dune doesn't know about.

   Tools run the rewriter from different places. Dune runs it from the build
   context with the file's path from there (src/db/users.ml); merlin, for the
   editor, runs it from the nearest folder with a dune file and gives the
   file's name alone. Searching up from the file finds the same schema in
   both. *)
let search_dirs dir =
  let dir =
    if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
    else dir
  in
  let is_root d =
    Sys.file_exists (Filename.concat d "dune-project")
    || Filename.basename (Filename.dirname d) = "_build"
  in
  let rec up d =
    let parent = Filename.dirname d in
    if is_root d || parent = d then [ d ] else d :: up parent
  in
  up dir

let resolve ~dir path =
  if not (Filename.is_relative path) then path
  else
    match
      List.find_opt
        (fun d -> Sys.file_exists (Filename.concat d path))
        (search_dirs dir)
    with
    | Some d -> Filename.concat d path
    | None -> path

let sql_files ~dir path =
  let path = resolve ~dir path in
  if Sys.file_exists path && Sys.is_directory path then
    Sys.readdir path |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".sql")
    |> List.sort compare
    |> List.map (Filename.concat path)
  else [ path ]

let load ~dir =
  if !schema_paths = [] then
    Error
      "no schema to check against: give the migrations in dune, e.g. \
       (preprocess (pps qb-ppx -- -qb-schema migrations)) with \
       (preprocessor_deps (source_tree migrations))"
  else
    match List.concat_map (sql_files ~dir) !schema_paths with
    | [] ->
        Error
          (Printf.sprintf "schema: no *.sql files found in %s"
             (String.concat ", " !schema_paths))
    | files ->
        List.fold_left
          (fun st file ->
            Result.bind st (fun st ->
                match In_channel.with_open_bin file In_channel.input_all with
                | exception Sys_error m ->
                    Error
                      (Printf.sprintf "schema: %s (is it in preprocessor_deps?)"
                         m)
                | sql -> (
                    let sql = Pg.strip_psql_meta sql in
                    match Schema.apply_sql ~init:st sql with
                    | Ok st -> Ok st
                    | Error e -> Error (file ^ ": " ^ Pg.error_to_string ~sql e)
                    )))
          (Ok Schema.empty) files

(* Read once per directory; a run of the rewriter is usually one file. *)
let schemas = Hashtbl.create 1

let schema ~dir =
  match Hashtbl.find_opt schemas dir with
  | Some s -> s
  | None ->
      let s = load ~dir in
      Hashtbl.replace schemas dir s;
      s

(* Where the [len] bytes at [off] of the string's contents are in the source
   file. Exact for {|quoted|} strings and for plain ones without escapes. *)
let position ?(len = 1) (loc : Location.t) delim s off =
  let open Lexing in
  let skip = match delim with None -> 1 | Some d -> String.length d + 2 in
  let start = { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + skip } in
  let pos = ref start in
  String.iteri
    (fun i c ->
      if i < off then
        let p = !pos in
        pos :=
          if c = '\n' then
            {
              p with
              pos_lnum = p.pos_lnum + 1;
              pos_bol = p.pos_cnum + 1;
              pos_cnum = p.pos_cnum + 1;
            }
          else { p with pos_cnum = p.pos_cnum + 1 })
    s;
  let p = !pos in
  {
    loc_start = p;
    loc_end = { p with pos_cnum = p.pos_cnum + len };
    loc_ghost = false;
  }

let error ~loc fmt =
  Printf.ksprintf
    (fun m ->
      Ast_builder.Default.pexp_extension ~loc
        (Location.error_extensionf ~loc "%s" m))
    fmt

(* Generated code is blamed on the whole [%sql ...], except the variable a
   parameter reads, which is blamed on its :name in the SQL: that is where an
   unbound or wrongly typed one is reported. *)
let relocate loc params =
  object
    inherit Ast_traverse.map as super
    method! location _ = { loc with loc_ghost = true }

    method! expression e =
      let e = super#expression e in
      match e.pexp_desc with
      | Pexp_ident { txt = Lident n; _ } -> (
          match List.assoc_opt n params with
          | Some loc ->
              {
                e with
                pexp_loc = loc;
                pexp_desc = Pexp_ident { txt = Lident n; loc };
              }
          | None -> e)
      | _ -> e
  end

let expand rows ~ctxt sql str_loc delim =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match schema ~dir:(Filename.dirname loc.loc_start.pos_fname) with
  | Error m -> error ~loc "qb: %s" m
  | Ok st -> (
      match Query.one st sql with
      | Error e ->
          let loc =
            match e.position with
            | Some p -> position str_loc delim sql p
            | None -> str_loc
          in
          error ~loc "%s" e.message
      | Ok q -> (
          match
            Codegen.query_expr ~bigint:!bigint ?tables:!tables ~rows st q
          with
          | exception Invalid_argument m -> error ~loc "qb: %s" m
          | src ->
              let params =
                List.map
                  (fun (p : Query.param) ->
                    ( Codegen.ident p.name,
                      position
                        ~len:(1 + String.length p.name)
                        str_loc delim sql p.at ))
                  q.params
              in
              (relocate loc params)#expression
                (Parse.expression (Lexing.from_string src))))

let payload =
  Ast_pattern.(single_expr_payload (pexp_constant (pconst_string __ __ __)))

(* [%sql]: rows are tuples. [%sql.obj]: rows are objects, a method per
   column. *)
let extensions =
  [
    Extension.V3.declare "qb.sql" Extension.Context.expression payload
      (expand `Tuple);
    Extension.V3.declare "qb.sql.obj" Extension.Context.expression payload
      (expand `Object);
  ]

let () =
  Driver.register_transformation "qb"
    ~rules:(List.map Context_free.Rule.extension extensions)
