(* Queries written as SQL: what goes in and what comes out, worked out from
   the parse tree and a Schema. *)

open Schema

type param = {
  name : string;
  ty : pg_type;
  nullable : bool;
  list : bool;
  at : int;
}

type column = { name : string; ty : pg_type; nullable : bool }
type item = Column of column | Whole of { table : table; nullable : bool }
type t = { name : string; sql : string; params : param list; row : item list }

(* A message and where in the text being read, if known. *)
exception Fail of string * int option

let fail ?at fmt = Printf.ksprintf (fun m -> raise (Fail (m, at))) fmt
let loc j = match field "location" j with Some (`Int i) -> Some i | _ -> None

(* ---------- "did you mean" ---------- *)

(* Edits (insert, delete, replace, swap two neighbours) between two names. *)
let distance a b =
  let n = String.length a and m = String.length b in
  let d = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = 0 to n do
    d.(i).(0) <- i
  done;
  for j = 0 to m do
    d.(0).(j) <- j
  done;
  for i = 1 to n do
    for j = 1 to m do
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      d.(i).(j) <-
        min
          (min (d.(i - 1).(j) + 1) (d.(i).(j - 1) + 1))
          (d.(i - 1).(j - 1) + cost);
      if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1] then
        d.(i).(j) <- min d.(i).(j) (d.(i - 2).(j - 2) + 1)
    done
  done;
  d.(n).(m)

(* " (did you mean title?)" for the nearest of [candidates], if one is near:
   a third of the name's letters wrong at most. *)
let hint name candidates =
  let name' = String.lowercase_ascii name in
  let best =
    List.fold_left
      (fun best c ->
        let d = distance name' (String.lowercase_ascii c) in
        match best with Some (_, d') when d' <= d -> best | _ -> Some (c, d))
      None
      (List.sort_uniq compare candidates)
  in
  match best with
  | Some (c, d) when d <= max 1 (String.length name / 3) && c <> name ->
      Printf.sprintf " (did you mean %s?)" c
  | _ -> ""

(* ---------- :name parameters ---------- *)

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident c = is_ident_start c || (c >= '0' && c <= '9')

(* Postgres numbers its parameters: each [:name] becomes [$n], the same [n]
   every time the name comes back. [:name?] says the value may be NULL.
   [IN :name] is a list of values: it becomes [IN ($n)], and the list is
   spelled out when the statement runs. Comments are dropped. Returns the
   text, the names with their [?] in order of [n], the names that are
   lists, where each output byte came from, and where each name is first
   written. *)
let number_params sql =
  let n = String.length sql in
  let out = Buffer.create n in
  let origin = ref [] in
  let add i c =
    Buffer.add_char out c;
    origin := i :: !origin
  in
  let params = ref [] in
  (* Where each name is first written. *)
  let first = ref [] in
  (* Each name, and whether it is used as a list. *)
  let kinds = Hashtbl.create 8 in
  (* Whether what has been written so far ends with the keyword IN. *)
  let after_in () =
    let rec back k =
      if
        k > 0
        &&
        match Buffer.nth out (k - 1) with
        | ' ' | '\n' | '\t' | '\r' -> true
        | _ -> false
      then back (k - 1)
      else k
    in
    let k = back (Buffer.length out) in
    k >= 2
    && Char.lowercase_ascii (Buffer.nth out (k - 2)) = 'i'
    && Char.lowercase_ascii (Buffer.nth out (k - 1)) = 'n'
    && (k = 2 || not (is_ident (Buffer.nth out (k - 3))))
  in
  let index name =
    let rec go i = function
      | [] -> None
      | (p, _) :: _ when p = name -> Some i
      | _ :: rest -> go (i + 1) rest
    in
    go 1 !params
  in
  let rec go : Sql_tokens.token list -> unit = function
    | [] -> ()
    (* A line comment is dropped, its newline kept; a block comment is a
       space. *)
    | { kind = Comment; text; at } :: rest ->
        if not (String.starts_with ~prefix:"--" text) then add at ' ';
        go rest
    | { kind = Named name; at = i; text } :: rest ->
        (* :name? has its ? right after it. *)
        let optional, rest =
          match rest with
          | { kind = Question; at; _ } :: rest when at = i + String.length text
            ->
              (true, rest)
          | _ -> (false, rest)
        in
        let is_list = after_in () in
        if is_list && optional then
          raise (Fail ("a list can't be NULL: write IN :" ^ name, Some i));
        (match Hashtbl.find_opt kinds name with
        | Some l when l <> is_list ->
            raise
              (Fail
                 ( Printf.sprintf
                     ":%s is a list in one place (IN :%s) and a single value \
                      in another"
                     name name,
                   Some i ))
        | _ -> Hashtbl.replace kinds name is_list);
        let k =
          match index name with
          | Some k ->
              if optional then
                params := List.map (fun (p, o) -> (p, o || p = name)) !params;
              k
          | None ->
              params := !params @ [ (name, optional) ];
              first := (name, i) :: !first;
              List.length !params
        in
        String.iter (add i)
          (if is_list then Printf.sprintf "($%d)" k else "$" ^ string_of_int k);
        go rest
    | { kind = Numbered _; at; _ } :: _ ->
        raise (Fail ("parameters are written :name here, not $1", Some at))
    (* Strings, quoted names and $$-quoted bodies among them: nothing inside
       is a parameter. *)
    | { at; text; _ } :: rest ->
        String.iteri (fun k c -> add (at + k) c) text;
        go rest
  in
  (* A [Fail] from the scan itself is positioned in the input. *)
  go (Sql_tokens.tokens sql);
  let origin = Array.of_list (List.rev !origin) in
  ( Buffer.contents out,
    List.map
      (fun (name, optional) ->
        (name, optional, Hashtbl.find_opt kinds name = Some true))
      !params,
    (fun p -> if p < Array.length origin then origin.(p) else n),
    !first )

(* ---------- FROM: the tables in scope ---------- *)

(* [qualified] is the EXCLUDED row of ON CONFLICT: its columns are only
   meant when written excluded.c. *)
type source = { alias : string; table : table; outer : bool; qualified : bool }

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
          ([ { alias; table; outer = false; qualified = false } ], []))
  | Some ("JoinExpr", je) ->
      if list "usingClause" je <> [] || bool "isNatural" je then
        fail "JOIN ... USING and NATURAL JOIN aren't supported: write ON";
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

type env = {
  scope : source list;
  names : (string * bool * bool) array;
      (* each $n's name, whether it is written :name?, and IN :name *)
  types : pg_type option array;
  seen : int option array; (* where each $n is first used *)
  null_tests : (int * int) list ref; (* [$n IS NULL]: where the $n is, and n *)
}

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
      let typed = List.filter_map (type_of env) (list "args" m) in
      match common (List.map fst typed) with
      | Some ty when List.length typed = List.length (list "args" m) ->
          Some (ty, List.for_all snd typed)
      | _ -> None)
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
          let is_null a =
            match node a with
            | Some ("A_Const", c) -> bool "isnull" c
            | _ -> false
          in
          Some
            ( ty,
              List.for_all
                (fun a ->
                  is_null a
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
  | `Assoc kv -> List.iter (fun (_, v) -> walk ~aliases env v) kv
  | `List l -> List.iter (walk ~aliases env) l
  | _ -> ()

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
  let rec dup = function
    | [] -> ()
    | (s : source) :: rest ->
        if List.exists (fun (r : source) -> r.alias = s.alias) rest then
          fail "table %s is named twice: give one an alias" s.alias;
        dup rest
  in
  dup scope;
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
  match field k s with Some _ -> fail "%s isn't supported" what | None -> ()

let select schema names s =
  unsupported s "withClause" "WITH";
  unsupported s "valuesLists" "VALUES";
  unsupported s "larg" "UNION / INTERSECT / EXCEPT";
  unsupported s "intoClause" "SELECT INTO";
  let froms = List.map (sources schema) (list "fromClause" s) in
  let env = new_env (List.concat_map fst froms) names in
  let part k = Option.value (field k s) ~default:`Null in
  List.iter (walk env) (List.concat_map snd froms);
  walk env (part "whereClause");
  walk env (part "targetList");
  let aliases =
    List.filter_map
      (fun t ->
        match node t with Some ("ResTarget", rt) -> str "name" rt | _ -> None)
      (list "targetList" s)
  in
  walk ~aliases env (part "groupClause");
  walk env (part "havingClause");
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
          let width =
            List.fold_left
              (fun w row ->
                match node row with
                | Some ("List", l) -> max w (List.length (list "items" l))
                | _ -> w)
              0 rows
          in
          require
            (if list "cols" s <> [] then cols
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
                fail "INSERT has more values than columns";
              List.iteri (fun i v -> assign values (List.nth cols i) v) items)
            rows)
  | Some _ -> fail "this INSERT isn't supported");
  (match field "onConflictClause" s with
  | None -> ()
  | Some oc ->
      let env =
        {
          env with
          scope = [ src; { src with alias = "excluded"; qualified = true } ];
        }
      in
      set_list env src (list "targetList" oc);
      walk env (Option.value (field "whereClause" oc) ~default:`Null));
  finish env (returning env s)

let update schema names s =
  unsupported s "withClause" "WITH";
  let src = target schema s in
  let froms = List.map (sources schema) (list "fromClause" s) in
  let env = new_env (src :: List.concat_map fst froms) names in
  List.iter (walk env) (List.concat_map snd froms);
  set_list env src (list "targetList" s);
  walk env (Option.value (field "whereClause" s) ~default:`Null);
  finish env (returning env s)

let delete schema names s =
  unsupported s "withClause" "WITH";
  let src = target schema s in
  let froms = List.map (sources schema) (list "usingClause" s) in
  let env = new_env (src :: List.concat_map fst froms) names in
  List.iter (walk env) (List.concat_map snd froms);
  walk env (Option.value (field "whereClause" s) ~default:`Null);
  finish env (returning env s)

(* ---------- files ---------- *)

(* [-- name: get_user] starts a query. *)
let name_line line =
  let line = String.trim line in
  let prefix p s =
    let n = String.length p in
    if String.length s >= n && String.sub s 0 n = p then
      Some (String.trim (String.sub s n (String.length s - n)))
    else None
  in
  Option.bind (prefix "--" line) (prefix "name:")

(* Each query's name and text, with the text's offset in the file. A query
   starts at a line that is a [-- name:] comment and runs to the next one;
   one written inside a string or a longer comment isn't a start. *)
let split text =
  let n = String.length text in
  let toks = Sql_tokens.tokens text in
  let line_start at =
    if at = 0 then 0
    else
      match String.rindex_from_opt text (at - 1) '\n' with
      | Some i -> i + 1
      | None -> 0
  in
  let blank s = String.trim s = "" in
  (* Each name, its line's offset, and where the text after it starts. *)
  let names =
    List.filter_map
      (fun (t : Sql_tokens.token) ->
        let line = line_start t.at in
        match t.kind with
        | Comment when blank (String.sub text line (t.at - line)) ->
            Option.map
              (fun name ->
                (name, line, min n (t.at + String.length t.text + 1)))
              (name_line t.text)
        | _ -> None)
      toks
  in
  (* Before the first name there may be comments, and nothing else. *)
  let first = match names with (_, line, _) :: _ -> line | [] -> n in
  List.iter
    (fun (t : Sql_tokens.token) ->
      if t.at < first && t.kind <> Comment && not (blank t.text) then
        raise
          (Fail
             ( "expected -- name: query_name before this statement",
               Some (line_start t.at) )))
    toks;
  let rec go = function
    | [] -> []
    | (name, line, start) :: rest ->
        let stop = match rest with (_, next, _) :: _ -> next | [] -> n in
        (name, line, start, String.sub text start (max 0 (stop - start)))
        :: go rest
  in
  go names

let valid_name name =
  name <> "" && is_ident_start name.[0] && String.for_all is_ident name

(* Trailing space and semicolon aren't part of the statement Caqti sends. *)
let rec trim_end s =
  let s = String.trim s in
  let n = String.length s in
  if n > 0 && s.[n - 1] = ';' then trim_end (String.sub s 0 (n - 1)) else s

(* Postgres can't tell the type of a bare [$1 IS NULL], even when $1 is
   typed elsewhere in the statement, so the type worked out here is written
   in: [CAST($1 AS TEXT) IS NULL]. *)
let cast_null_tests sql (params : param list) null_tests =
  List.sort_uniq (fun (a, _) (b, _) -> compare b a) null_tests
  |> List.fold_left
       (fun sql (at, n) ->
         let token = "$" ^ string_of_int n in
         let len = String.length token in
         match List.nth_opt params (n - 1) with
         | Some p
           when at + len <= String.length sql && String.sub sql at len = token
           ->
             String.sub sql 0 at
             ^ Printf.sprintf "CAST(%s AS %s)" token (type_sql p.ty)
             ^ String.sub sql (at + len) (String.length sql - at - len)
         | _ -> sql)
       sql

let query schema (name, name_at, start, text) =
  if not (valid_name name) then
    raise
      (Fail (Printf.sprintf "%S isn't a name for a query" name, Some name_at));
  let in_file back = function
    | Fail (m, at) ->
        Fail
          (m, Some (match at with Some p -> start + back p | None -> name_at))
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
    let qs = List.map (query schema) (split text) in
    let rec dup = function
      | [] -> ()
      | (q : t) :: rest ->
          if List.exists (fun (r : t) -> r.name = q.name) rest then
            fail "two queries are named %s" q.name;
          dup rest
    in
    dup qs;
    Ok qs
  with Fail (message, position) -> Error { Pg.message; position }

let one schema text =
  try Ok (query schema ("query", 0, 0, text))
  with Fail (message, position) -> Error { Pg.message; position }
