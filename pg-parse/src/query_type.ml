(* Scope resolution, expression type inference, and AST walking for SQL queries. *)

open Schema
open Query_ast

(* ---------- FROM: the tables in scope ---------- *)

let rec sources schema j =
  match node j with
  | Some ("RangeVar", r) -> (
      let name = Option.value (str "relname" r) ~default:"" in
      match find ?schema:(str "schemaname" r) schema name with
      | None ->
          fail ?at:(loc r) "unknown table %s%s" name
            (hint name (List.map (fun (t : table) -> t.name) schema.tables))
      | Some table ->
          let alias =
            match field "alias" r with
            | Some a -> Option.value (str "aliasname" a) ~default:name
            | None -> name
          in
          ( [ { alias; table; outer = false; qualified = false; at = loc r } ],
            [] ))
  | Some ("JoinExpr", je) ->
      if list "usingClause" je <> [] || bool "isNatural" je then
        fail ?at:(loc je)
          "JOIN ... USING and NATURAL JOIN aren't supported: write ON";
      let get k = sources schema (Option.value (field k je) ~default:`Null) in
      let l, lq = get "larg" and r, rq = get "rarg" in
      let outer = List.map (fun s -> { s with outer = true }) in
      let l, r =
        match str "jointype" je with
        | Some "JOIN_LEFT" -> (l, outer r)
        | Some "JOIN_RIGHT" -> (outer l, r)
        | Some "JOIN_FULL" -> (outer l, outer r)
        | _ -> (l, r)
      in
      (l @ r, lq @ rq @ Option.to_list (field "quals" je))
  | Some ("RangeSubselect", r) ->
      fail ?at:(loc r) "subqueries in FROM aren't supported"
  | Some (kind, body) -> fail ?at:(loc body) "%s in FROM isn't supported" kind
  | None -> fail "unexpected FROM"

(* ---------- types ---------- *)

let simple name = { name; mods = []; array_dims = 0 }

(* Types an OCaml value of one kind can be sent as. *)
let family (ty : pg_type) =
  match ty.name with
  | "int2" | "int4" | "int8" -> "integer"
  | "float4" | "float8" -> "float"
  | "text" | "varchar" | "bpchar" | "citext" | "name" -> "text"
  | "timestamp" | "timestamptz" -> "timestamp"
  | n -> n

let compatible (a : pg_type) (b : pg_type) =
  a.array_dims = b.array_dims && family a = family b

let column_names (s : source) =
  List.map (fun (c : Schema.column) -> c.name) s.table.columns

let aliases env = List.map (fun s -> s.alias) env.scope

let column_of env cr =
  let at = loc cr in
  let lookup (s : source) c =
    List.find_opt (fun (col : Schema.column) -> col.name = c) s.table.columns
    |> Option.map (fun (col : Schema.column) ->
        (col.ty, s.outer || not col.not_null))
  in
  match List.map sval (list "fields" cr) with
  | [ Some c ] -> (
      match
        List.filter_map
          (fun s -> if s.qualified then None else lookup s c)
          env.scope
      with
      | [ x ] -> x
      | [] ->
          fail ?at "unknown column %s%s" c
            (hint c
               (List.concat_map
                  (fun s -> if s.qualified then [] else column_names s)
                  env.scope))
      | _ -> fail ?at "column %s is in several tables: write table.%s" c c)
  | [ Some t; Some c ] -> (
      match List.find_opt (fun s -> s.alias = t) env.scope with
      | None -> fail ?at "unknown table %s%s" t (hint t (aliases env))
      | Some s -> (
          match lookup s c with
          | Some x -> x
          | None ->
              fail ?at "unknown column %s.%s%s" t c (hint c (column_names s))))
  | _ -> fail ?at "this column reference isn't supported"

let has_star cr =
  List.exists
    (fun f -> match node f with Some ("A_Star", _) -> true | _ -> false)
    (list "fields" cr)

let set_param env pr (ty : pg_type) =
  match field "number" pr with
  | Some (`Int n) when n >= 1 && n <= Array.length env.types -> (
      match env.types.(n - 1) with
      | None -> env.types.(n - 1) <- Some ty
      | Some old when compatible old ty -> ()
      | Some old ->
          fail ?at:(loc pr) "parameter :%s is used as both %s and %s"
            (let name, _, _ = env.names.(n - 1) in
             name)
            (type_sql old) (type_sql ty))
  | _ -> ()

(* + - * / % and ||, between two operands. *)
let arithmetic a =
  match (str "kind" a, names (list "name" a)) with
  | Some "AEXPR_OP", [ (("+" | "-" | "*" | "/" | "%" | "||") as op) ] -> Some op
  | _ -> None

let func_name f =
  match List.rev (names (list "funcname" f)) with n :: _ -> n | [] -> ""

let comparison a =
  match str "kind" a with
  | Some "AEXPR_OP" -> (
      match names (list "name" a) with
      (* The last three: contains, is contained by, overlaps. *)
      | [ ("=" | "<>" | "!=" | "<" | ">" | "<=" | ">=" | "@>" | "<@" | "&&") ]
        ->
          true
      | _ -> false)
  | Some
      ( "AEXPR_IN" | "AEXPR_LIKE" | "AEXPR_ILIKE" | "AEXPR_BETWEEN"
      | "AEXPR_NOT_BETWEEN" | "AEXPR_BETWEEN_SYM" | "AEXPR_NOT_BETWEEN_SYM"
      | "AEXPR_DISTINCT" | "AEXPR_NOT_DISTINCT" ) ->
      true
  | _ -> false

(* Functions of text giving text, and giving an integer: NULL when an
   argument is. *)
let text_functions =
  [
    "lower"; "upper"; "initcap"; "btrim"; "ltrim"; "rtrim"; "replace";
    "substr"; "substring"; "left"; "right"; "lpad"; "rpad"; "repeat";
    "reverse"; "translate"; "split_part"; "md5"; "to_char";
  ]
  [@@ocamlformat "disable"]

let integer_functions =
  [
    "length"; "char_length"; "character_length"; "octet_length"; "bit_length";
    "strpos"; "position"; "ascii";
  ]
  [@@ocamlformat "disable"]

let is_null_const j =
  match node j with Some ("A_Const", c) -> bool "isnull" c | _ -> false

(* One type for values of several: the widest number, or the first. *)
let common (tys : pg_type list) =
  match tys with
  | [] -> None
  | first :: rest ->
      if List.for_all (compatible first) rest then
        let widest names =
          List.find_opt
            (fun n -> List.exists (fun (t : pg_type) -> t.name = n) tys)
            names
        in
        Some
          (match
             match family first with
             | "integer" -> widest [ "int8"; "int4"; "int2" ]
             | "float" -> widest [ "float8"; "float4" ]
             | _ -> None
           with
          | Some n when first.array_dims = 0 -> simple n
          | _ -> first)
      else None

(* The type of an expression and whether it can be NULL, when that can be
   told without knowing Postgres's functions and operators. *)
let rec type_of env j : (pg_type * bool) option =
  match node j with
  | Some ("ColumnRef", cr) when not (has_star cr) -> Some (column_of env cr)
  | Some ("ParamRef", pr) -> (
      match field "number" pr with
      | Some (`Int n) when n >= 1 && n <= Array.length env.types ->
          let _, optional, _ = env.names.(n - 1) in
          Option.map (fun ty -> (ty, optional)) env.types.(n - 1)
      | _ -> None)
  | Some ("A_Const", c) ->
      if bool "isnull" c then None
      else if field "ival" c <> None then Some (simple "int4", false)
      else if field "sval" c <> None then Some (simple "text", false)
      else if field "boolval" c <> None then Some (simple "bool", false)
      else if field "fval" c <> None then Some (simple "numeric", false)
      else None
  | Some ("TypeCast", tc) ->
      let ty = pg_type (Option.value (field "typeName" tc) ~default:`Null) in
      let arg = Option.value (field "arg" tc) ~default:`Null in
      (match node arg with
      | Some ("ParamRef", pr) -> set_param env pr ty
      | _ -> ());
      let nullable =
        match type_of env arg with Some (_, n) -> n | None -> true
      in
      Some (ty, nullable)
  | Some ("FuncCall", f) -> (
      match (func_name f, list "args" f) with
      | "count", _ -> Some (simple "int8", false)
      (* NULL over no rows. *)
      | ("min" | "max"), [ a ] ->
          Option.map (fun (ty, _) -> (ty, true)) (type_of env a)
      (* Postgres widens a sum: of integers to BIGINT, and of BIGINT to
         NUMERIC, which is left to a cast. *)
      | "sum", [ a ] -> (
          match type_of env a with
          | Some (({ name = "int2" | "int4"; array_dims = 0; _ } : pg_type), _)
            ->
              Some (simple "int8", true)
          | Some (({ name = "float4" | "float8"; array_dims = 0; _ } as ty), _)
            ->
              Some (ty, true)
          | _ -> None)
      | "avg", [ a ] -> (
          match type_of env a with
          | Some
              (({ name = "float4" | "float8"; array_dims = 0; _ } : pg_type), _)
            ->
              Some (simple "float8", true)
          | _ -> None)
      | ("bool_and" | "bool_or" | "every"), [ _ ] -> Some (simple "bool", true)
      | "string_agg", _ -> Some (simple "text", true)
      | "abs", [ a ] -> type_of env a
      | "date_trunc", [ _; a ] -> type_of env a
      | ( ( "now" | "transaction_timestamp" | "statement_timestamp"
          | "clock_timestamp" ),
          [] ) ->
          Some (simple "timestamptz", false)
      | "random", [] -> Some (simple "float8", false)
      | "date_part", args ->
          Some (simple "float8", List.exists (can_be_null env) args)
      | "gen_random_uuid", [] -> Some (simple "uuid", false)
      (* concat reads a NULL as an empty string. *)
      | "concat", _ -> Some (simple "text", false)
      | name, args when List.mem name text_functions ->
          Some (simple "text", List.exists (can_be_null env) args)
      | name, args when List.mem name integer_functions ->
          Some (simple "int4", List.exists (can_be_null env) args)
      | _ -> None)
  (* CURRENT_DATE and the like, written without brackets. *)
  | Some ("SQLValueFunction", f) -> (
      match str "op" f with
      | Some "SVFOP_CURRENT_DATE" -> Some (simple "date", false)
      | Some ("SVFOP_CURRENT_TIMESTAMP" | "SVFOP_CURRENT_TIMESTAMP_N") ->
          Some (simple "timestamptz", false)
      | Some ("SVFOP_LOCALTIMESTAMP" | "SVFOP_LOCALTIMESTAMP_N") ->
          Some (simple "timestamp", false)
      | _ -> None)
  (* Conditions are BOOLEAN: NULL when a side is, except IS [NOT] DISTINCT
     FROM, IS NULL and IS TRUE, which always answer. *)
  | Some ("A_Expr", a) when comparison a ->
      let sides =
        List.concat_map
          (fun k ->
            match field k a with
            | None -> []
            | Some j -> (
                match node j with
                | Some ("List", l) -> list "items" l
                | _ -> [ j ]))
          [ "lexpr"; "rexpr" ]
      in
      let always =
        match str "kind" a with
        | Some ("AEXPR_DISTINCT" | "AEXPR_NOT_DISTINCT") -> true
        | _ -> false
      in
      Some (simple "bool", (not always) && List.exists (can_be_null env) sides)
  | Some ("BoolExpr", b) ->
      Some (simple "bool", List.exists (can_be_null env) (list "args" b))
  | Some (("NullTest" | "BooleanTest"), _) -> Some (simple "bool", false)
  (* NULLIF(a, b): a, or NULL. *)
  | Some ("A_Expr", a) when str "kind" a = Some "AEXPR_NULLIF" ->
      Option.map
        (fun (ty, _) -> (ty, true))
        (Option.bind (field "lexpr" a) (type_of env))
  (* GREATEST / LEAST skip NULLs. *)
  | Some ("MinMaxExpr", m) -> (
      let args = list "args" m in
      let typed =
        List.filter_map
          (fun a -> if is_null_const a then None else type_of env a)
          args
      in
      match common (List.map fst typed) with
      | None -> None
      | Some ty ->
          List.iter
            (fun a ->
              match node a with
              | Some ("ParamRef", pr) -> set_param env pr ty
              | _ -> ())
            args;
          if
            List.for_all
              (fun a -> is_null_const a || type_of env a <> None)
              args
          then
            Some
              ( ty,
                List.for_all
                  (fun a -> is_null_const a || can_be_null env a)
                  args )
          else None)
  (* CASE: the type its branches share; NULL with no ELSE, or when a branch
     is. *)
  | Some ("CaseExpr", c) -> (
      let branches =
        List.filter_map
          (fun w ->
            match node w with
            | Some ("CaseWhen", w) -> field "result" w
            | _ -> None)
          (list "args" c)
      in
      let default = field "defresult" c in
      let results = branches @ Option.to_list default in
      let typed =
        List.filter_map
          (fun r -> if is_null_const r then None else type_of env r)
          results
      in
      match common (List.map fst typed) with
      | None -> None
      | Some ty ->
          List.iter
            (fun r ->
              match node r with
              | Some ("ParamRef", pr) -> set_param env pr ty
              | _ -> ())
            results;
          Some
            ( ty,
              default = None
              || List.exists
                   (fun r -> is_null_const r || can_be_null env r)
                   results ))
  (* -x has x's type. *)
  | Some ("A_Expr", a)
    when field "lexpr" a = None
         && str "kind" a = Some "AEXPR_OP"
         && List.mem (names (list "name" a)) [ [ "-" ]; [ "+" ] ] ->
      Option.bind (field "rexpr" a) (type_of env)
  (* JSON: -> gives JSON again, ->> text; NULL when the key isn't there. *)
  | Some ("A_Expr", a)
    when str "kind" a = Some "AEXPR_OP"
         && List.mem (names (list "name" a)) [ [ "->>" ]; [ "#>>" ] ] ->
      Some (simple "text", true)
  | Some ("A_Expr", a)
    when str "kind" a = Some "AEXPR_OP"
         && List.mem (names (list "name" a)) [ [ "->" ]; [ "#>" ] ] ->
      Option.map
        (fun (ty, _) -> (ty, true))
        (Option.bind (field "lexpr" a) (type_of env))
  | Some ("A_Expr", a) -> (
      let side k = Option.bind (field k a) (type_of env) in
      match (arithmetic a, side "lexpr", side "rexpr") with
      (* A whole number with a float is DOUBLE PRECISION. *)
      | Some op, Some (l, ln), Some (r, rn)
        when op <> "||" && l.array_dims = 0 && r.array_dims = 0
             && List.sort compare [ family l; family r ]
                = [ "float"; "integer" ] ->
          Some (simple "float8", ln || rn)
      | Some op, Some (l, ln), Some (r, rn)
        when l.array_dims = 0 && r.array_dims = 0 && family l = family r -> (
          let wider names =
            List.find_opt (fun n -> l.name = n || r.name = n) names
            |> Option.map (fun n -> (simple n, ln || rn))
          in
          match (op, family l) with
          | "||", "text" -> Some (simple "text", ln || rn)
          | "||", _ -> None
          | _, "integer" -> wider [ "int8"; "int4"; "int2" ]
          | _, "float" -> wider [ "float8"; "float4" ]
          | _ -> None)
      | _ -> None)
  | Some ("CoalesceExpr", c) -> (
      let args = list "args" c in
      let typed = List.filter_map (type_of env) args in
      match typed with
      | [] -> None
      | (first, _) :: _ ->
          (* As Postgres does: the widest of its numbers. *)
          let ty = Option.value (common (List.map fst typed)) ~default:first in
          List.iter
            (fun a ->
              match node a with
              | Some ("ParamRef", pr) -> set_param env pr ty
              | _ -> ())
            args;
          Some
            ( ty,
              List.for_all
                (fun a ->
                  is_null_const a
                  || match type_of env a with Some (_, n) -> n | None -> true)
                args ))
  | _ -> None

(* An expression whose type isn't known may be NULL. *)
and can_be_null env j =
  match type_of env j with Some (_, n) -> n | None -> true

(* ---------- parameters, from how they are used ---------- *)

(* Functions whose first argument is text. *)
let text_argument_functions =
  [
    "lower"; "upper"; "initcap"; "btrim"; "ltrim"; "rtrim"; "replace";
    "substr"; "substring"; "left"; "right"; "lpad"; "rpad"; "repeat";
    "reverse"; "translate"; "split_part"; "md5"; "length"; "char_length";
    "character_length"; "octet_length"; "bit_length"; "ascii";
  ]
  [@@ocamlformat "disable"]

(* [j] is to have type [ty]: the parameters that must then have it too, in
   arithmetic, a concatenation, a COALESCE or a function that keeps its
   argument's type. *)
let rec expect env j (ty : pg_type) =
  let sides a =
    List.iter
      (fun k -> Option.iter (fun x -> expect env x ty) (field k a))
      [ "lexpr"; "rexpr" ]
  in
  match node j with
  | Some ("ParamRef", pr) -> set_param env pr ty
  | Some ("A_Expr", a) when ty.array_dims = 0 -> (
      match (arithmetic a, family ty) with
      | Some "||", "text" -> sides a
      | Some "||", _ -> ()
      | Some _, ("integer" | "float") -> sides a
      | _ -> ())
  | Some ("CoalesceExpr", c) ->
      List.iter (fun a -> expect env a ty) (list "args" c)
  | Some ("FuncCall", f)
    when family ty = "text"
         && List.mem (func_name f)
              [ "lower"; "upper"; "initcap"; "btrim"; "ltrim"; "rtrim" ] -> (
      (* These take TEXT, whatever kind of text they are compared with. *)
      match list "args" f with
      | a :: _ -> expect env a (simple "text")
      | [] -> ())
  | _ -> ()

(* [column = :p], [column + :p] and the like give :p the column's type. *)
let unify env a b =
  let is_const j =
    match node j with Some ("A_Const", _) -> true | _ -> false
  in
  let one a b =
    match (node a, type_of env b) with
    | Some ("ParamRef", pr), Some (ty, _) -> set_param env pr ty
    (* [col > :n + 1]: the parameters of the other side's arithmetic. A
       constant says too little for that: it fits many types. *)
    | _, Some (ty, _) when not (is_const b) -> expect env a ty
    | _ -> ()
  in
  one a b;
  one b a

(* Checks every column named in [j] and types the parameters it can.
   [aliases] are output column names, which ORDER BY and GROUP BY may use. *)
let rec walk ?(aliases = []) env (j : J.t) =
  match j with
  | `Assoc [ (("SubLink" | "SelectStmt"), body) ] ->
      fail ?at:(loc body) "subqueries aren't supported"
  | `Assoc [ ("BoolExpr", (`Assoc kv as b)) ] ->
      List.iter
        (fun a ->
          match node a with
          | Some ("ParamRef", pr) -> set_param env pr (simple "bool")
          | _ -> ())
        (list "args" b);
      List.iter (fun (_, v) -> walk ~aliases env v) kv
  | `Assoc [ ("BooleanTest", bt) ] ->
      let arg = Option.value (field "arg" bt) ~default:`Null in
      (match node arg with
      | Some ("ParamRef", pr) -> set_param env pr (simple "bool")
      | _ -> ());
      walk ~aliases env arg
  | `Assoc [ ("NullTest", nt) ] ->
      let arg = Option.value (field "arg" nt) ~default:`Null in
      (match node arg with
      | Some ("ParamRef", pr) -> (
          match (loc pr, field "number" pr) with
          | Some at, Some (`Int n) ->
              env.null_tests := (at, n) :: !(env.null_tests)
          | _ -> ())
      | _ -> ());
      walk ~aliases env arg
  | `Assoc [ ("ParamRef", pr) ] -> (
      match field "number" pr with
      | Some (`Int n) when n >= 1 && n <= Array.length env.seen ->
          if env.seen.(n - 1) = None then env.seen.(n - 1) <- loc pr
      | _ -> ())
  | `Assoc [ ("ColumnRef", cr) ] -> (
      if not (has_star cr) then
        match List.map sval (list "fields" cr) with
        | [ Some c ] when List.mem c aliases -> ()
        | _ -> ignore (column_of env cr))
  | `Assoc [ ("A_Expr", a) ] ->
      let get k = Option.value (field k a) ~default:`Null in
      let l = get "lexpr" and r = get "rexpr" in
      (let is_const j =
         match node j with Some ("A_Const", _) -> true | _ -> false
       in
       (* x = ANY(array): one value against an array's elements. *)
       (match (str "kind" a, node l, node r) with
       | Some ("AEXPR_OP_ANY" | "AEXPR_OP_ALL"), _, Some ("ParamRef", pr) ->
           let name =
             match field "number" pr with
             | Some (`Int n) when n >= 1 && n <= Array.length env.names ->
                 let name, _, _ = env.names.(n - 1) in
                 name
             | _ -> "p"
           in
           fail ?at:(loc pr)
             "ANY(:%s) takes an array, which a parameter can't be: write IN \
              :%s for a list of values"
             name name
       | Some ("AEXPR_OP_ANY" | "AEXPR_OP_ALL"), Some ("ParamRef", pr), _ -> (
           match type_of env r with
           | Some (ty, _) when ty.array_dims > 0 ->
               set_param env pr { ty with array_dims = ty.array_dims - 1 }
           | _ -> ())
       | _ -> ());
       if comparison a then
         let rs =
           match node r with Some ("List", l) -> list "items" l | _ -> [ r ]
         in
         List.iter (unify env l) rs
       else if str "kind" a = Some "AEXPR_NULLIF" then unify env l r
       else if
         (* [:p - 100] says little about :p: a constant fits many types.
            ['%' || :p] says it is text. *)
         match arithmetic a with
         | Some "||" -> true
         | Some _ -> not (is_const l || is_const r)
         | None -> false
       then unify env l r);
      walk ~aliases env l;
      walk ~aliases env r
  | `Assoc [ ("FuncCall", (`Assoc kv as f)) ] ->
      (* lower(:p): the parameter is text. *)
      (if List.mem (func_name f) text_argument_functions then
         match list "args" f with
         | a :: _ -> (
             match node a with
             | Some ("ParamRef", pr) -> set_param env pr (simple "text")
             | _ -> ())
         | [] -> ());
      List.iter (fun (_, v) -> walk ~aliases env v) kv
  | `Assoc [ ("TypeCast", _) ] as tc -> (
      ignore (type_of env tc);
      match tc with
      | `Assoc [ (_, body) ] ->
          walk ~aliases env (Option.value (field "arg" body) ~default:`Null)
      | _ -> ())
  | `Assoc [ ("CaseExpr", (`Assoc kv as c)) ] as expr ->
      List.iter (fun (_, v) -> walk ~aliases env v) kv;
      if aliases = [] then (
        (match field "arg" c with
        | Some arg ->
            List.iter
              (fun w ->
                match node w with
                | Some ("CaseWhen", w) ->
                    Option.iter (unify env arg) (field "expr" w)
                | _ -> ())
              (list "args" c)
        | None ->
            List.iter
              (fun w ->
                match node w with
                | Some ("CaseWhen", w) -> (
                    match Option.bind (field "expr" w) node with
                    | Some ("ParamRef", pr) -> set_param env pr (simple "bool")
                    | _ -> ())
                | _ -> ())
              (list "args" c));
        ignore (type_of env expr))
  | `Assoc [ (("CoalesceExpr" | "MinMaxExpr"), `Assoc kv) ] as expr ->
      List.iter (fun (_, v) -> walk ~aliases env v) kv;
      if aliases = [] then ignore (type_of env expr)
  | `Assoc kv -> List.iter (fun (_, v) -> walk ~aliases env v) kv
  | `List l -> List.iter (walk ~aliases env) l
  | _ -> ()
