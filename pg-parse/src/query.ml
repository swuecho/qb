(* Queries written as SQL: what goes in and what comes out, worked out from
   the parse tree and a Schema. *)

open Schema
open Query_ast
open Query_lex
open Query_type

type nonrec param = param = {
  name : string;
  ty : pg_type;
  nullable : bool;
  list : bool;
  at : int;
}

type nonrec column = column = { name : string; ty : pg_type; nullable : bool }

type nonrec item = item =
  | Column of column
  | Whole of { table : table; nullable : bool }

type nonrec t = t = {
  name : string;
  sql : string;
  params : param list;
  row : item list;
}

(* ---------- SELECT ---------- *)

let target_name rt =
  match str "name" rt with
  | Some n -> Some n
  | None ->
      let rec of_expr j =
        match node j with
        | Some ("ColumnRef", cr) -> (
            match List.rev (List.map sval (list "fields" cr)) with
            | Some c :: _ -> Some c
            | _ -> None)
        | Some ("FuncCall", f) -> Some (func_name f)
        | Some ("CoalesceExpr", _) -> Some "coalesce"
        | Some ("TypeCast", tc) ->
            of_expr (Option.value (field "arg" tc) ~default:`Null)
        | _ -> None
      in
      of_expr (Option.value (field "val" rt) ~default:`Null)

(* Why a result column has no type, and the cast that gives it one. *)
let untyped env rt v =
  let arg_type f =
    match list "args" f with
    | [ a ] -> Option.map (fun ((ty : pg_type), _) -> ty.name) (type_of env a)
    | _ -> None
  in
  match (str "name" rt, node v) with
  | _, Some ("FuncCall", f) when func_name f = "extract" ->
      "extract is NUMERIC, which qb reads as text: write extract(...)::int, or \
       ::float8 for a fraction"
  | _, Some ("FuncCall", f) when func_name f = "avg" && arg_type f <> None ->
      "avg of a whole number is NUMERIC, which qb reads as text: for a float, \
       write avg(...)::float8"
  | _, Some ("FuncCall", f) when func_name f = "sum" && arg_type f <> None ->
      "sum of a BIGINT or NUMERIC is NUMERIC, which qb reads as text: write \
       sum(...)::bigint, or ::numeric to keep it"
  | _, Some ("FuncCall", f) ->
      let n = func_name f in
      Printf.sprintf
        "qb doesn't know what %s(...) returns: say so with a cast, e.g. \
         %s(...)::text"
        n n
  | Some n, _ ->
      Printf.sprintf
        "can't work out the type of column %s: write a cast, e.g. (...)::text" n
  | None, _ ->
      "can't work out the type of this expression: write a cast, e.g. \
       (...)::text"

let targets env items =
  List.concat
  @@ List.mapi
       (fun i t ->
         match node t with
         | Some ("ResTarget", rt) -> (
             let v = Option.value (field "val" rt) ~default:`Null in
             let at = loc rt in
             match node v with
             | Some ("ColumnRef", cr) when has_star cr -> (
                 let whole (s : source) =
                   Whole { table = s.table; nullable = s.outer }
                 in
                 match List.map sval (list "fields" cr) with
                 | [ None ] ->
                     List.filter_map
                       (fun s -> if s.qualified then None else Some (whole s))
                       env.scope
                 | [ Some t; None ] -> (
                     match List.find_opt (fun s -> s.alias = t) env.scope with
                     | Some s -> [ whole s ]
                     | None ->
                         fail ?at "unknown table %s%s" t (hint t (aliases env)))
                 | _ -> fail ?at "this column reference isn't supported")
             | _ -> (
                 let name =
                   match target_name rt with
                   | Some n -> n
                   | None -> Printf.sprintf "column%d" (i + 1)
                 in
                 match type_of env v with
                 | Some (ty, nullable) -> [ Column { name; ty; nullable } ]
                 | None -> fail ?at "%s" (untyped env rt v)))
         | _ -> [])
       items

let new_env scope names =
  let seen_aliases = Hashtbl.create 8 in
  List.iter
    (fun (s : source) ->
      if Hashtbl.mem seen_aliases s.alias then
        fail ?at:s.at "table %s is named twice: give one an alias" s.alias;
      Hashtbl.add seen_aliases s.alias ())
    scope;
  {
    scope;
    names;
    types = Array.make (Array.length names) None;
    seen = Array.make (Array.length names) None;
    null_tests = ref [];
  }

(* Every parameter must have a type by now. *)
let finish env row =
  let params =
    Array.to_list
      (Array.mapi
         (fun i (name, nullable, list) ->
           match env.types.(i) with
           | Some ty -> ({ name; ty; nullable; list; at = 0 } : param)
           | None ->
               fail ?at:env.seen.(i)
                 "can't work out the type of :%s: write a cast, e.g. :%s::text"
                 name name)
         env.names)
  in
  (params, row, !(env.null_tests))

let unsupported s k what =
  match field k s with
  | Some v -> fail ?at:(loc v) "%s isn't supported" what
  | None -> ()

let walk_bool env j =
  (match node j with
  | Some ("ParamRef", pr) -> set_param env pr (simple "bool")
  | _ -> ());
  walk env j

let select schema names s =
  unsupported s "withClause" "WITH";
  unsupported s "valuesLists" "VALUES";
  unsupported s "larg" "UNION / INTERSECT / EXCEPT";
  unsupported s "intoClause" "SELECT INTO";
  let froms = List.map (sources schema) (list "fromClause" s) in
  let env = new_env (List.concat_map fst froms) names in
  let part k = Option.value (field k s) ~default:`Null in
  List.iter (walk_bool env) (List.concat_map snd froms);
  walk_bool env (part "whereClause");
  walk env (part "distinctClause");
  walk env (part "targetList");
  let aliases =
    List.filter_map
      (fun t ->
        match node t with Some ("ResTarget", rt) -> str "name" rt | _ -> None)
      (list "targetList" s)
  in
  walk ~aliases env (part "groupClause");
  walk_bool env (part "havingClause");
  walk ~aliases env (part "sortClause");
  List.iter
    (fun k ->
      match node (part k) with
      | Some ("ParamRef", pr) ->
          walk env (part k);
          set_param env pr (simple "int8")
      | _ -> walk env (part k))
    [ "limitCount"; "limitOffset" ];
  finish env (targets env (list "targetList" s))

(* ---------- INSERT, UPDATE, DELETE ---------- *)

(* The table a statement writes to. *)
let target schema s =
  let rel = Option.value (field "relation" s) ~default:`Null in
  match sources schema (`Assoc [ ("RangeVar", rel) ]) with
  | [ src ], _ -> src
  | _ -> fail "unexpected table"

(* RETURNING: [returningList] before Postgres 18's parser. *)
let returning env s =
  let items =
    match field "returningClause" s with
    | Some rc -> list "exprs" rc
    | None -> list "returningList" s
  in
  walk env (`List items);
  targets env items

let table_column (src : source) ?at name =
  match
    List.find_opt (fun (c : Schema.column) -> c.name = name) src.table.columns
  with
  | Some c -> c
  | None ->
      fail ?at "unknown column %s.%s%s" src.table.name name
        (hint name (column_names src))

(* A value going into [col]: a parameter there takes the column's type. *)
let assign env (col : Schema.column) v =
  (* Through arithmetic and the like too: [:n + 1] into an INTEGER column. *)
  expect env v col.ty;
  walk env v

(* SET col = value, ... *)
let set_list env src items =
  List.iter
    (fun t ->
      match node t with
      | Some ("ResTarget", rt) ->
          let col =
            table_column src ?at:(loc rt)
              (Option.value (str "name" rt) ~default:"")
          in
          assign env col (Option.value (field "val" rt) ~default:`Null)
      | Some (_, body) ->
          fail ?at:(loc body) "this SET isn't supported: write col = value"
      | None -> ())
    items

let insert schema names s =
  unsupported s "withClause" "WITH";
  let src = target schema s in
  let env = new_env [ src ] names in
  let cols =
    match list "cols" s with
    | [] -> src.table.columns
    | cs ->
        List.filter_map
          (fun c ->
            match node c with
            | Some ("ResTarget", rt) ->
                Some
                  (table_column src ?at:(loc rt)
                     (Option.value (str "name" rt) ~default:""))
            | _ -> None)
          cs
  in
  (* Columns the INSERT must give: the database has nothing to put there. *)
  let require given =
    match
      List.filter
        (fun (c : Schema.column) ->
          c.not_null && (not c.generated) && (not c.has_default)
          && not
               (List.exists (fun (g : Schema.column) -> g.name = c.name) given))
        src.table.columns
    with
    | [] -> ()
    | missing ->
        let names = List.map (fun (c : Schema.column) -> c.name) missing in
        fail
          ?at:(loc (Option.value (field "relation" s) ~default:`Null))
          "INSERT leaves out %s: %s NOT NULL and %s no default"
          (String.concat ", " names)
          (if List.length names = 1 then "it is" else "they are")
          (if List.length names = 1 then "has" else "have")
  in
  (match Option.bind (field "selectStmt" s) node with
  | None -> require [] (* DEFAULT VALUES *)
  | Some ("SelectStmt", sel) -> (
      match list "valuesLists" sel with
      | [] -> fail "INSERT ... SELECT isn't supported: write VALUES"
      | rows ->
          (* Without a column list, the values fill the first columns. *)
          let explicit_cols = list "cols" s <> [] in
          let width =
            List.fold_left
              (fun w row ->
                match node row with
                | Some ("List", l) -> max w (List.length (list "items" l))
                | _ -> w)
              0 rows
          in
          require
            (if explicit_cols then cols
             else List.filteri (fun i _ -> i < width) cols);
          (* VALUES can't name the table's columns. *)
          let values = { env with scope = [] } in
          List.iter
            (fun row ->
              let items =
                match node row with
                | Some ("List", l) -> list "items" l
                | _ -> []
              in
              if List.length items > List.length cols then
                fail
                  ?at:(loc (List.nth items (List.length cols)))
                  "INSERT has more values than columns";
              (if explicit_cols && List.length items < List.length cols then
                 let at =
                   match List.rev items with
                   | last :: _ -> loc last
                   | [] ->
                       loc (Option.value (field "relation" s) ~default:`Null)
                 in
                 fail ?at "INSERT has fewer values than columns");
              List.iteri (fun i v -> assign values (List.nth cols i) v) items)
            rows)
  | Some _ -> fail "this INSERT isn't supported");
  (match field "onConflictClause" s with
  | None -> ()
  | Some oc ->
      (match field "infer" oc with
      | Some inf ->
          List.iter
            (fun elt ->
              match node elt with
              | Some ("IndexElem", ie) -> (
                  match str "name" ie with
                  | Some col -> ignore (table_column src ?at:(loc ie) col)
                  | None -> Option.iter (walk env) (field "expr" ie))
              | _ -> ())
            (list "indexElems" inf);
          walk_bool env (Option.value (field "whereClause" inf) ~default:`Null)
      | None -> ());
      let env =
        {
          env with
          scope = [ src; { src with alias = "excluded"; qualified = true } ];
        }
      in
      set_list env src (list "targetList" oc);
      walk_bool env (Option.value (field "whereClause" oc) ~default:`Null));
  finish env (returning env s)

let update schema names s =
  unsupported s "withClause" "WITH";
  let src = target schema s in
  let froms = List.map (sources schema) (list "fromClause" s) in
  let env = new_env (src :: List.concat_map fst froms) names in
  List.iter (walk_bool env) (List.concat_map snd froms);
  set_list env src (list "targetList" s);
  walk_bool env (Option.value (field "whereClause" s) ~default:`Null);
  finish env (returning env s)

let delete schema names s =
  unsupported s "withClause" "WITH";
  let src = target schema s in
  let froms = List.map (sources schema) (list "usingClause" s) in
  let env = new_env (src :: List.concat_map fst froms) names in
  List.iter (walk_bool env) (List.concat_map snd froms);
  walk_bool env (Option.value (field "whereClause" s) ~default:`Null);
  finish env (returning env s)

(* ---------- entry points ---------- *)

let query ?fallback schema (name, name_at, start, text) =
  if not (valid_name name) then
    raise
      (Fail (Printf.sprintf "%S isn't a name for a query" name, Some name_at));
  let in_file back = function
    | Fail (m, at) ->
        let pos =
          match at with Some p -> Some (start + back p) | None -> fallback
        in
        Fail (m, pos)
    | e -> e
  in
  let pg_sql, params, back, first =
    try number_params text with e -> raise (in_file Fun.id e)
  in
  try
    let stmt =
      match Pg.statements pg_sql with
      | Error e -> raise (Fail (e.message, e.position))
      | Ok [ s ] -> s
      | Ok [] -> fail "there is no statement here"
      | Ok _ -> fail "several statements: a query is one statement"
    in
    let params, row, null_tests =
      match node stmt with
      | Some ("SelectStmt", s) -> select schema (Array.of_list params) s
      | Some ("InsertStmt", s) -> insert schema (Array.of_list params) s
      | Some ("UpdateStmt", s) -> update schema (Array.of_list params) s
      | Some ("DeleteStmt", s) -> delete schema (Array.of_list params) s
      | _ -> fail "only SELECT, INSERT, UPDATE and DELETE are read"
    in
    {
      name;
      sql = trim_end (cast_null_tests pg_sql params null_tests);
      params =
        List.map
          (fun (p : param) ->
            {
              p with
              at = start + Option.value (List.assoc_opt p.name first) ~default:0;
            })
          params;
      row;
    }
  with e -> raise (in_file back e)

let of_sql schema text =
  try
    let entries = split text in
    let seen = Hashtbl.create 8 in
    let qs =
      List.map
        (fun ((name, name_at, _, _) as entry) ->
          if Hashtbl.mem seen name then
            fail ~at:name_at "two queries are named %s" name;
          Hashtbl.add seen name ();
          query ~fallback:name_at schema entry)
        entries
    in
    Ok qs
  with Fail (message, position) -> Error { Pg.message; position }

let one schema text =
  try Ok (query schema ("query", 0, 0, text))
  with Fail (message, position) -> Error { Pg.message; position }
