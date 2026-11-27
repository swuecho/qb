(* Statements written as SQL, with list-argument expansion. *)

open Ty
open Table

(* The row Caqti decodes and how to turn it into ['a]. *)
type 'a row_dec = Row_dec : 'x Caqti_type.t * ('x -> 'a) -> 'a row_dec

(* One value, or a list of them written out as [$1, $2, ...]. *)
type sarg = One of param | Many : 'a ty * 'a list -> sarg

type 'a stmt = {
  sql : string;
  args : sarg list;
  row : 'a row_dec;
  name : string option;
}

type arg = sarg
type 'a row = 'a row_dec

let arg ty v = One (Param (ty.caqti, v))
let arg_list ty vs = Many (ty, vs)
let ret ty = Row_dec (ty.caqti, Fun.id)
let unit = Row_dec (Caqti_type.unit, Fun.id)

let whole t =
  base "Qb.Stmt.whole" t;
  match decoder t.row with Dec (ct, d, _) -> Row_dec (ct, d)

let opt (Row_dec (t, d)) = Row_dec (Caqti_type.option t, Option.map d)
let map f (Row_dec (t, d)) = Row_dec (t, fun x -> f (d x))

let both (Row_dec (ta, da)) (Row_dec (tb, db)) =
  Row_dec (Caqti_type.t2 ta tb, fun (x, y) -> (da x, db y))

let ( let+ ) r f = map f r
let ( and+ ) = both
let make ?name sql args row = { sql; args; row; name }
let sql s = s.sql
let stmt_name s = s.name

(* What [x IN (...)] is given for an empty list: a subquery with no rows,
   so that IN is false and NOT IN is true. Postgres wants its type. *)
let no_values dialect ty =
  match dialect with
  | Postgres ->
      Printf.sprintf "SELECT CAST(NULL AS %s) WHERE 1 = 0" (ty.ddl Postgres)
  | Sqlite -> "SELECT NULL WHERE 1 = 0"
  | Mysql -> "SELECT NULL FROM DUAL WHERE 1 = 0"

(* The text to send and its parameters in order. Validates placeholder bounds
   for every statement, and renumbers [$k] when a list argument is present: a
   list of three at [$2] becomes [$2, $3, $4] and what was [$3] becomes [$5]. *)
let expand dialect s =
  let args = Array.of_list s.args in
  let nargs = Array.length args in
  let label = match s.name with Some n -> " (" ^ n ^ ")" | None -> "" in
  let toks = Sql_tokens.tokens s.sql in
  let numbered =
    List.exists
      (fun (t : Sql_tokens.token) ->
        match t.kind with Numbered _ -> true | _ -> false)
      toks
  in
  let check_numbered k =
    if k < 1 || k > nargs then
      invalid_arg
        (Printf.sprintf "Qb.Stmt%s: $%d has no argument (%d given)" label k
           nargs)
  in
  (if numbered then
     List.iter
       (fun (t : Sql_tokens.token) ->
         match t.kind with Numbered k -> check_numbered k | _ -> ())
       toks
   else
     let qn =
       List.fold_left
         (fun acc (t : Sql_tokens.token) ->
           match t.kind with Question -> acc + 1 | _ -> acc)
         0 toks
     in
     if qn <> nargs then
       invalid_arg
         (Printf.sprintf
            "Qb.Stmt%s: SQL has %d ? placeholder%s but %d argument%s given"
            label qn
            (if qn = 1 then "" else "s")
            nargs
            (if nargs = 1 then " was" else "s were")));
  let size = function One _ -> 1 | Many (_, vs) -> List.length vs in
  let params =
    List.concat_map
      (function
        | One p -> [ p ]
        | Many (ty, vs) -> List.map (fun v -> Param (ty.caqti, v)) vs)
      s.args
  in
  if Array.for_all (function One _ -> true | Many _ -> false) args then
    (s.sql, params)
  else
    let start = Array.make nargs 1 in
    for k = 1 to nargs - 1 do
      start.(k) <- start.(k - 1) + size args.(k - 1)
    done;
    let used = Array.make nargs false in
    let b = Buffer.create (String.length s.sql + 32) in
    List.iter
      (fun (t : Sql_tokens.token) ->
        match t.kind with
        | Numbered k -> (
            check_numbered k;
            used.(k - 1) <- true;
            match args.(k - 1) with
            | One _ -> Printf.bprintf b "$%d" start.(k - 1)
            | Many (ty, []) -> Buffer.add_string b (no_values dialect ty)
            | Many (_, vs) ->
                List.iteri
                  (fun m _ ->
                    if m > 0 then Buffer.add_string b ", ";
                    Printf.bprintf b "$%d" (start.(k - 1) + m))
                  vs)
        | _ -> Buffer.add_string b t.text)
      toks;
    Array.iteri
      (fun k u ->
        match args.(k) with
        | Many _ when not u ->
            invalid_arg
              (Printf.sprintf
                 "Qb.Stmt%s: the list argument %d is not in the SQL as $%d"
                 label (k + 1) (k + 1))
        | _ -> ())
      used;
    (Buffer.contents b, params)

(* A statement's parameters as one nested-tuple Caqti type and value. *)
let pack_args dialect s =
  let sql, params = expand dialect s in
  (sql, params, Cache.pack_rev (List.rev params))

let stmt_sql ?(dialect = Postgres) s = Stdlib.fst (expand dialect s)

(* A statement whose text depends on how long a list argument is. *)
let variadic s = List.exists (function Many _ -> true | One _ -> false) s.args
