(* qb-pg-parse init           The dune stanzas that set a project up
   qb-pg-parse gen FILE...    OCaml qb modules for the tables FILEs create
   qb-pg-parse queries FILE... -q QUERIES   typed statements for SQL queries
   qb-pg-parse check FILE...  Checks that Postgres can parse each FILE *)

open Qb_pg_parse
open Cmdliner

(* Files may come straight from pg_dump, psql meta-commands and all. *)
let read file =
  Pg.strip_psql_meta (In_channel.with_open_bin file In_channel.input_all)

let report file sql e =
  Printf.eprintf "%s: %s\n" file (Pg.error_to_string ~sql e)

(* The schema the files build, replayed in order. *)
let replay files =
  let rec go st = function
    | [] -> Ok st
    | file :: rest -> (
        let sql = read file in
        match Schema.apply_sql ~init:st sql with
        | Ok st -> go st rest
        | Error e ->
            report file sql e;
            Error ())
  in
  go Schema.empty files

let write output src =
  match output with
  | None -> print_string src
  | Some f -> Out_channel.with_open_bin f (fun oc -> output_string oc src)

let queries files query_files tables bigint output =
  match replay files with
  | Error () -> 1
  | Ok st -> (
      let rec read_all acc = function
        | [] -> Ok (List.rev acc)
        | file :: rest -> (
            let sql = In_channel.with_open_bin file In_channel.input_all in
            match Query.of_sql st sql with
            | Ok qs -> read_all (List.rev_append qs acc) rest
            | Error e ->
                report file sql e;
                Error ())
      in
      match read_all [] query_files with
      | Error () -> 1
      | Ok qs -> (
          match Codegen.queries ~bigint ?tables st qs with
          | Ok src ->
              write output src;
              0
          | Error m ->
              Printf.eprintf "qb-pg-parse: %s\n" m;
              1))

let gen files only bigint output =
  match replay files with
  | Error () -> 1
  | Ok st ->
      List.iter (Printf.eprintf "qb-pg-parse: warning: %s\n") st.warnings;
      List.iter
        (fun name ->
          let found =
            match String.index_opt name '.' with
            | Some i ->
                Schema.find st ~schema:(String.sub name 0 i)
                  (String.sub name (i + 1) (String.length name - i - 1))
            | None -> Schema.find st name
          in
          if found = None then
            Printf.eprintf "qb-pg-parse: warning: no table %s\n" name)
        only;
      write output (Codegen.ocaml ~bigint ~only st);
      0

let check files caqti =
  List.fold_left
    (fun code file ->
      let sql = read file in
      match Pg.check ~caqti sql with
      | Ok () -> code
      | Error e ->
          report file sql e;
          1)
    0 files

let files =
  Arg.(non_empty & pos_all file [] & info [] ~docv:"FILE" ~doc:"SQL files.")

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "o"; "output" ] ~docv:"FILE" ~doc:"Write to FILE, not stdout.")

let bigint =
  Arg.(
    value
    & opt (enum [ ("int", `Int); ("int64", `Int64) ]) `Int
    & info [ "bigint" ] ~docv:"TYPE"
        ~doc:
          "The OCaml type $(b,BIGINT) is read as: $(b,int) (the default) or \
           $(b,int64).")

let queries_cmd =
  let query_files =
    Arg.(
      non_empty & opt_all file []
      & info [ "q"; "queries" ] ~docv:"FILE"
          ~doc:
            "A file of queries, each started by a line $(b,-- name: \
             query_name). Repeat for several.")
  in
  let tables =
    Arg.(
      value
      & opt (some string) None
      & info [ "tables" ] ~docv:"MODULE"
          ~doc:
            "The module holding $(b,gen)'s output for the same schema. Queries \
             selecting whole tables ($(b,t.*)) then return its row types.")
  in
  Cmd.v
    (Cmd.info "queries"
       ~doc:
         "Generate a typed qb statement for each query in the $(b,-q) files, \
          checked against the schema the FILEs build.")
    Term.(const queries $ files $ query_files $ tables $ bigint $ output)

let gen_cmd =
  let only =
    Arg.(
      value & opt_all string []
      & info [ "t"; "table" ] ~docv:"TABLE"
          ~doc:"Only generate TABLE. Repeat for several; default is all.")
  in
  Cmd.v
    (Cmd.info "gen"
       ~doc:
         "Generate qb table modules from schema files, replayed in the order \
          given (e.g. migrations).")
    Term.(const gen $ files $ only $ bigint $ output)

let check_cmd =
  let caqti =
    Arg.(
      value & flag
      & info [ "caqti" ] ~doc:"Read ? placeholders as \\$1, \\$2, ... first.")
  in
  Cmd.v
    (Cmd.info "check" ~doc:"Check that Postgres can parse each file.")
    Term.(const check $ files $ caqti)

(* The dune setup has three parts that must name the same folder and
   module; this writes them out together. *)
let init dir modname =
  let file = String.uncapitalize_ascii modname ^ ".ml" in
  Printf.printf
    {|; For the dune file of the folder holding %s/.

; Table modules from the migrations: %s, the module %s.
(rule
 (targets %s)
 (deps
  (glob_files %s/*.sql))
 (action
  (run qb-pg-parse gen %%{deps} -o %%{targets})))

; [%%sql "..."] checked against the same migrations: add the preprocess and
; preprocessor_deps fields to your own executable or library.
(executable
 (name main)
 (libraries qb caqti-driver-postgresql)
 (preprocess
  (pps qb-ppx -- -qb-schema %s -qb-tables %s))
 (preprocessor_deps
  (source_tree %s)))
|}
    dir file modname file dir dir modname dir;
  0

let init_cmd =
  let dir =
    Arg.(
      value & opt string "migrations"
      & info [ "m"; "migrations" ] ~docv:"DIR"
          ~doc:"The folder of *.sql migrations, from the dune file's folder.")
  in
  let modname =
    Arg.(
      value & opt string "Schema"
      & info [ "module" ] ~docv:"MODULE"
          ~doc:"The module the table modules are generated into.")
  in
  Cmd.v
    (Cmd.info "init"
       ~doc:
         "Print the dune stanzas for a project: the rule that generates the \
          table modules, and the fields that turn on [%sql].")
    Term.(const init $ dir $ modname)

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "qb-pg-parse" ~doc:"Postgres's parser, for qb.")
          [ init_cmd; gen_cmd; queries_cmd; check_cmd ]))
