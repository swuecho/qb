(* OCaml source for qb table modules, from a Schema. *)

open Schema

let keywords =
  [
    "and";
    "as";
    "assert";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "lazy";
    "let";
    "match";
    "method";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "effect";
    "mod";
    "land";
    "lor";
    "lxor";
    "lsl";
    "lsr";
    "asr";
  ]

(* A lowercase OCaml identifier for a SQL name. *)
let ident name =
  let b = Buffer.create (String.length name) in
  String.iteri
    (fun i c ->
      match c with
      | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> Buffer.add_char b c
      | 'A' .. 'Z' ->
          Buffer.add_char b (if i = 0 then Char.lowercase_ascii c else c)
      | _ -> Buffer.add_char b '_')
    name;
  let s = Buffer.contents b in
  let s =
    match s with
    | "" -> "_"
    | s when (s.[0] >= '0' && s.[0] <= '9') || s.[0] = '\'' -> "_" ^ s
    | s -> s
  in
  if List.mem s keywords then s ^ "_" else s

(* Module names must start with a letter: __drizzle_migrations becomes
   Drizzle_migrations, 2fa becomes T_2fa. *)
let module_name name =
  let s = ident name in
  let n = String.length s in
  let rec skip i = if i < n && s.[i] = '_' then skip (i + 1) else i in
  let s = String.sub s (skip 0) (n - skip 0) in
  let s =
    match s with
    | "" -> "T"
    | s when s.[0] >= 'a' && s.[0] <= 'z' -> s
    | s -> "T_" ^ s
  in
  (* ident's keyword escape (type_) isn't needed for a module. *)
  String.capitalize_ascii s

(* Makes each name in [names] unique by adding _2, _3, ... *)
let uniq ?(taken = []) names =
  let seen = Hashtbl.create 16 in
  List.iter (fun n -> Hashtbl.replace seen n ()) taken;
  List.map
    (fun n ->
      let rec pick k =
        let c = if k = 1 then n else Printf.sprintf "%s_%d" n k in
        if Hashtbl.mem seen c then pick (k + 1) else c
      in
      let c = pick 1 in
      Hashtbl.replace seen c ();
      c)
    names

(* [BIGINT] is read as [int64] instead of [int] while [with_bigint `Int64]
   runs, for code whose ids are already [int64]. *)
let bigint_int64 = ref false

let with_bigint bigint f =
  let saved = !bigint_int64 in
  bigint_int64 := bigint = `Int64;
  Fun.protect ~finally:(fun () -> bigint_int64 := saved) f

(* The qb type expression for a column type, and a note when it is only an
   approximation. *)
let qb_type (ty : pg_type) =
  let custom caqti =
    Printf.sprintf "(Qb.ty Caqti_type.%s %S)" caqti (type_sql ty)
  in
  if ty.array_dims > 0 then
    (custom "string", Some "array, read as its text form")
  else
    match (ty.name, ty.mods) with
    | "int8", [] -> ((if !bigint_int64 then "Qb.int64" else "Qb.int"), None)
    | ("int4" | "int2"), [] -> (custom "int", None)
    | "float8", [] -> ("Qb.float", None)
    | "float4", [] -> (custom "float", None)
    | "text", [] -> ("Qb.text", None)
    | ("varchar" | "bpchar" | "citext" | "name"), _ -> (custom "string", None)
    | "bool", [] -> ("Qb.bool", None)
    | "bytea", [] -> ("Qb.octets", None)
    | "timestamptz", [] -> ("Qb.ptime", None)
    | ("timestamptz" | "timestamp"), _ -> (custom "ptime", None)
    | "date", [] -> ("Qb.date", None)
    | "uuid", [] -> ("Qb.uuid", None)
    | "jsonb", [] -> ("Qb.json", None)
    | "json", [] -> (custom "string", None)
    | "numeric", _ ->
        (custom "string", Some "numeric, read as text to keep its precision")
    | _ -> (custom "string", Some (ty.name ^ ", read as text"))

let ocaml_type (ty : pg_type) =
  if ty.array_dims > 0 then "string"
  else
    match ty.name with
    | "int8" -> if !bigint_int64 then "int64" else "int"
    | "int4" | "int2" -> "int"
    | "float8" | "float4" -> "float"
    | "bool" -> "bool"
    | "timestamptz" | "timestamp" | "date" -> "Ptime.t"
    | _ -> "string"

(* The database fills these in when an insert leaves them out. *)
let optional (c : column) = c.generated || c.has_default || not c.not_null

(* [values ~required ?optional ... ()]: a new row for Qb's insert_values. The
   columns and table are aliased first, since the labels shadow them. *)
let values_fn b cols =
  let p fmt = Printf.bprintf b fmt in
  let alias v = "c__" ^ v in
  p
    "\n\
    \  (* A new row, for [Qb.Blocking.insert_values db (values ...)]. Columns\n\
    \     the database can fill in (generated, defaulted or nullable) are\n\
    \     optional. *)\n\
    \  let values =\n\
    \    let t__ = table";
  List.iter (fun (_, (_, v)) -> p "\n    and %s = %s" (alias v) v) cols;
  p " in\n    fun";
  List.iter
    (fun (c, (f, _)) -> p " %s%s" (if optional c then "?" else "~") f)
    cols;
  p
    " () ->\n\
    \      Qb.values t__\n\
    \        (List.filter_map Fun.id\n\
    \           [\n";
  List.iter
    (fun ((c : column), (f, v)) ->
      let set =
        if c.not_null then Printf.sprintf "Qb.set %s" (alias v)
        else Printf.sprintf "fun v -> Qb.set %s (Some v)" (alias v)
      in
      if optional c then p "             Option.map (%s) %s;\n" set f
      else p "             Some (Qb.set %s %s);\n" (alias v) f)
    cols;
  p "           ])\n"

let table_module ~module_name:m (t : table) =
  let b = Buffer.create 1024 in
  let p fmt = Printf.bprintf b fmt in
  let fields = uniq (List.map (fun (c : column) -> ident c.name) t.columns) in
  (* Column values live next to [table], [values] and [by_pk], so a field
     with one of those names gets a column value called e.g. [table_col]. *)
  let reserved = [ "table"; "values"; "by_pk" ] in
  let values =
    uniq ~taken:reserved
      (List.map (fun f -> if List.mem f reserved then f ^ "_col" else f) fields)
  in
  let cols = List.combine t.columns (List.combine fields values) in
  let single_pk = match t.primary_key with [ k ] -> Some k | _ -> None in
  p "module %s = struct\n" m;
  if List.length t.primary_key > 1 then
    p
      "  (* Composite primary key (%s): qb only marks single columns, so no\n\
      \     column here is ~primary_key. *)\n"
      (String.concat ", " t.primary_key);
  p "  type t = {\n";
  List.iter
    (fun ((c : column), (f, _)) ->
      let nullable = not c.not_null in
      p "    %s : %s%s;\n" f (ocaml_type c.ty)
        (if nullable then " option" else ""))
    cols;
  p "  }\n";
  List.iter
    (fun ((c : column), (f, v)) ->
      let ty, note = qb_type c.ty in
      let ty =
        if c.not_null then ty else Printf.sprintf "(Qb.nullable %s)" ty
      in
      p "\n";
      Option.iter (fun n -> p "  (* %s *)\n" n) note;
      p "  let %s =\n    Qb.column %S %s (fun (r : t) -> r.%s)" v c.name ty f;
      if single_pk = Some c.name then p " ~primary_key:true";
      if c.generated then p " ~auto_increment:true";
      p "\n")
    cols;
  p "\n  let table =\n";
  p "    let ( let+ ), ( and+ ) = Qb.(( let+ ), ( and+ )) in\n";
  (match t.schema with
  | Some s when s <> "public" -> p "    Qb.table ~schema:%S %S\n" s t.name
  | _ -> p "    Qb.table %S\n" t.name);
  List.iteri
    (fun i (_, (f, v)) ->
      p "      %s %s = Qb.col %s\n" (if i = 0 then "(let+" else " and+") f v)
    cols;
  p "       in\n       { %s })\n"
    (String.concat "; " (List.map (fun (_, (f, _)) -> f) cols));
  values_fn b cols;
  (match single_pk with
  | Some k -> (
      match List.find_opt (fun ((c : column), _) -> c.name = k) cols with
      | Some (_, (_, v)) ->
          p
            "\n\
            \  (* The row with this primary key, as a query to pass to first,\n\
            \     update or delete: [Qb.Blocking.first db (by_pk k)]. *)\n\
            \  let by_pk =\n\
            \    let c__ = %s in\n\
            \    fun k -> Qb.where (Qb.Ops.( = ) c__ k) (Qb.from table)\n"
            v
      | None -> ())
  | None -> ());
  p "end\n";
  Buffer.contents b

let public (t : table) =
  match t.schema with None | Some "public" -> true | Some _ -> false

let indent src =
  String.split_on_char '\n' src
  |> List.map (fun l -> if l = "" then l else "  " ^ l)
  |> String.concat "\n"

(* Where each table's module goes: public tables at the top, the others in
   a module named after their schema (gea.reports is Gea.Reports). Names are
   made unique. *)
let layout tables =
  let schemas =
    List.fold_left
      (fun acc (t : table) ->
        match t.schema with
        | Some sc when (not (public t)) && not (List.mem sc acc) -> acc @ [ sc ]
        | _ -> acc)
      [] tables
  in
  let schema_modules = uniq (List.map module_name schemas) in
  let named ?taken ts =
    List.combine ts
      (uniq ?taken (List.map (fun (t : table) -> module_name t.name) ts))
  in
  ( named ~taken:schema_modules (List.filter public tables),
    List.map2
      (fun sc m ->
        (m, named (List.filter (fun (t : table) -> t.schema = Some sc) tables)))
      schemas schema_modules )

let modules named =
  String.concat ""
    (List.map
       (fun ((t : table), m) ->
         if t.columns = [] then
           Printf.sprintf "\n(* %s has no columns, so it has no module. *)\n"
             t.name
         else "\n" ^ table_module ~module_name:m t)
       named)

let rec ocaml ?(bigint = `Int) ?only s =
  with_bigint bigint (fun () -> ocaml_ ?only s)

and ocaml_ ?(only = []) (s : Schema.t) =
  let wanted (t : table) =
    only = [] || List.mem t.name only
    || List.mem (Option.value t.schema ~default:"public" ^ "." ^ t.name) only
  in
  let top, nested = layout (List.filter wanted s.tables) in
  let header =
    "(* Generated by qb-pg-parse from a Postgres schema. Do not edit. *)\n"
  in
  header ^ modules top
  ^ String.concat ""
      (List.map
         (fun (m, inner) ->
           Printf.sprintf "\nmodule %s = struct%send\n" m
             (indent (modules inner)))
         nested)

(* ---------- queries ---------- *)

exception Bad_query of string

(* The module [ocaml] gives a table, as a path under [root]. *)
let table_path root (s : Schema.t) (t : table) =
  let top, nested = layout s.tables in
  let find named =
    List.find_map (fun (x, m) -> if x == t then Some m else None) named
  in
  match find top with
  | Some m -> root ^ "." ^ m
  | None -> (
      match
        List.find_map
          (fun (sm, inner) -> Option.map (fun m -> sm ^ "." ^ m) (find inner))
          nested
      with
      | Some m -> root ^ "." ^ m
      | None -> raise (Bad_query ("no module for table " ^ t.name)))

(* [a; b; c] as (a, (b, c)): how Qb.Stmt.both nests. *)
let rec nest = function
  | [] -> "()"
  | [ x ] -> x
  | x :: rest -> Printf.sprintf "(%s, %s)" x (nest rest)

let rec both = function
  | [] -> "Qb.Stmt.unit"
  | [ x ] -> x
  | x :: rest -> Printf.sprintf "Qb.Stmt.both (%s) (%s)" x (both rest)

let quoted sql =
  let rec pick k =
    let id = if k = 0 then "sql" else Printf.sprintf "sql%d" k in
    let close = "|" ^ id ^ "}" in
    let n = String.length close in
    let rec has i =
      i + n <= String.length sql && (String.sub sql i n = close || has (i + 1))
    in
    if has 0 then pick (k + 1) else id
  in
  let id = pick 0 in
  Printf.sprintf "{%s|%s|%s}" id sql id

let qb_ty ty nullable =
  let t = fst (qb_type ty) in
  if nullable then Printf.sprintf "(Qb.nullable %s)" t else t

let ocaml_ty ty nullable = ocaml_type ty ^ if nullable then " option" else ""

(* [shared]: the record type already made for each list of fields, so that
   queries with the same columns share one row type, as sqlc's do. *)
let query_fn ?(shared = Hashtbl.create 0) ?tables (s : Schema.t) (q : Query.t) =
  let fn = ident q.name in
  let all_whole =
    tables <> None
    && List.for_all (function Query.Whole _ -> true | Column _ -> false) q.row
  in
  (* The row's type declaration (if it needs one), type and reader. *)
  let decl, row_type, reader =
    match tables with
    | Some root when all_whole ->
        let parts =
          List.filter_map
            (function
              | Query.Whole { table; nullable } ->
                  let m = table_path root s table in
                  Some
                    (if nullable then
                       ( m ^ ".t option",
                         Printf.sprintf "Qb.Stmt.opt (Qb.Stmt.whole %s.table)" m
                       )
                     else (m ^ ".t", Printf.sprintf "Qb.Stmt.whole %s.table" m))
              | Column _ -> None)
            q.row
        in
        let types = List.map fst parts and readers = List.map snd parts in
        let vars = List.mapi (fun i _ -> Printf.sprintf "x%d" (i + 1)) parts in
        ( "",
          (match types with
          | [ t ] -> t
          | ts -> "(" ^ String.concat " * " ts ^ ")"),
          if List.length parts <= 2 then both readers
          else
            Printf.sprintf "Qb.Stmt.map\n       (fun %s -> (%s))\n       (%s)"
              (nest vars) (String.concat ", " vars) (both readers) )
    | _ -> (
        let cols =
          List.concat_map
            (function
              | Query.Column c -> [ c ]
              | Whole { table; nullable } ->
                  List.map
                    (fun (c : column) : Query.column ->
                      {
                        name = c.name;
                        ty = c.ty;
                        nullable = nullable || not c.not_null;
                      })
                    table.columns)
            q.row
        in
        let fields = List.map (fun (c : Query.column) -> ident c.name) cols in
        let rec dup = function
          | [] -> ()
          | f :: rest ->
              if List.mem f rest then
                raise
                  (Bad_query
                     (Printf.sprintf
                        "query %s: two columns are named %s: rename one with AS"
                        q.name f));
              dup rest
        in
        dup fields;
        let ret (c : Query.column) =
          Printf.sprintf "Qb.Stmt.ret %s" (qb_ty c.ty c.nullable)
        in
        match cols with
        | [] -> ("", "unit", "Qb.Stmt.unit")
        | [ c ] -> ("", ocaml_ty c.ty c.nullable, ret c)
        | _ ->
            let field_decls =
              String.concat ""
                (List.map2
                   (fun f (c : Query.column) ->
                     Printf.sprintf "  %s : %s;\n" f (ocaml_ty c.ty c.nullable))
                   fields cols)
            in
            let decl, ty =
              match Hashtbl.find_opt shared field_decls with
              | Some ty -> ("", ty)
              | None ->
                  let ty = fn ^ "_row" in
                  Hashtbl.add shared field_decls ty;
                  (Printf.sprintf "type %s = {\n%s}\n\n" ty field_decls, ty)
            in
            ( decl,
              ty,
              Printf.sprintf
                "Qb.Stmt.map\n       (fun %s -> ({ %s } : %s))\n       (%s)"
                (nest fields)
                (String.concat "; " fields)
                ty
                (both (List.map ret cols)) ))
  in
  let labels =
    uniq (List.map (fun (p : Query.param) -> ident p.name) q.params)
  in
  let args =
    List.map2
      (fun l (p : Query.param) ->
        Printf.sprintf "Qb.Stmt.%s %s %s"
          (if p.list then "arg_list" else "arg")
          (qb_ty p.ty p.nullable) l)
      labels q.params
  in
  Printf.sprintf
    "%slet %s%s : %s Qb.stmt =\n  Qb.Stmt.make ~name:%S\n    %s\n    [%s]\n    (%s)\n"
    decl fn
    (String.concat "" (List.map (fun l -> " ~" ^ l) labels))
    row_type q.name (quoted q.sql)
    (if args = [] then "" else " " ^ String.concat ";\n      " args ^ " ")
    reader

let query_expr ?tables ?(rows = `Tuple) (s : Schema.t) (q : Query.t) =
  let col (c : Query.column) =
    ( ident c.name,
      ocaml_ty c.ty c.nullable,
      Printf.sprintf "Qb.Stmt.ret %s" (qb_ty c.ty c.nullable) )
  in
  (* Each element of the row: its name, type and reader. *)
  let parts =
    List.concat_map
      (function
        | Query.Column c -> [ col c ]
        | Whole { table; nullable } -> (
            match tables with
            | Some root ->
                let m =
                  try table_path root s table
                  with Bad_query m -> invalid_arg m
                in
                [
                  (if nullable then
                     ( ident table.name,
                       m ^ ".t option",
                       Printf.sprintf "Qb.Stmt.opt (Qb.Stmt.whole %s.table)" m
                     )
                   else
                     ( ident table.name,
                       m ^ ".t",
                       Printf.sprintf "Qb.Stmt.whole %s.table" m ));
                ]
            | None ->
                List.map
                  (fun (c : column) ->
                    col
                      {
                        name = c.name;
                        ty = c.ty;
                        nullable = nullable || not c.not_null;
                      })
                  table.columns))
      q.row
  in
  let names = List.map (fun (n, _, _) -> n) parts
  and types = List.map (fun (_, t, _) -> t) parts
  and readers = List.map (fun (_, _, r) -> r) parts in
  let vars = List.mapi (fun i _ -> Printf.sprintf "x%d" (i + 1)) parts in
  let reader, row_type =
    match (rows, parts) with
    (* An object with a method per column; one column is still the bare
       value. *)
    | `Object, _ :: _ :: _ ->
        let rec dup = function
          | [] -> ()
          | n :: rest ->
              if List.mem n rest then
                invalid_arg
                  (Printf.sprintf "two columns are named %s: rename one with AS"
                     n);
              dup rest
        in
        dup names;
        ( Printf.sprintf "Qb.Stmt.map (fun %s -> object %s end) (%s)" (nest vars)
            (String.concat " "
               (List.map2 (Printf.sprintf "method %s = %s") names vars))
            (both readers),
          Printf.sprintf "< %s >"
            (String.concat "; "
               (List.map2 (Printf.sprintf "%s : %s") names types)) )
    | _ ->
        ( (if List.length parts <= 2 then both readers
           else
             Printf.sprintf "Qb.Stmt.map (fun %s -> (%s)) (%s)" (nest vars)
               (String.concat ", " vars) (both readers)),
          match types with [] -> "unit" | ts -> String.concat " * " ts )
  in
  let args =
    List.map
      (fun (p : Query.param) ->
        Printf.sprintf "Qb.Stmt.%s %s %s"
          (if p.list then "arg_list" else "arg")
          (qb_ty p.ty p.nullable) (ident p.name))
      q.params
  in
  Printf.sprintf "(Qb.Stmt.make %s [%s] (%s) : (%s) Qb.stmt)" (quoted q.sql)
    (String.concat "; " args) reader row_type

let queries ?(bigint = `Int) ?tables (s : Schema.t) qs =
  with_bigint bigint @@ fun () ->
  try
    Ok
      ("(* Generated by qb-pg-parse from SQL queries. Do not edit. *)\n"
      ^
      let shared = Hashtbl.create 16 in
      String.concat ""
        (List.map (fun q -> "\n" ^ query_fn ~shared ?tables s q) qs))
  with Bad_query m -> Error m
