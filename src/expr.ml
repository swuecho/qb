(* Typed SQL expressions, aggregates, window functions, scalar functions,
   keyset tuple comparisons, JSON/Postgres helpers, and raw SQL fragments. *)

open Ty
open Table

type cmp_op = Eq | Ne | Gt | Ge | Lt | Le

let cmp_op_sql = function
  | Eq -> "="
  | Ne -> "<>"
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

type nulls = Nulls_default | Nulls_first | Nulls_last

type frag =
  | Sql of string
  | Param : 'a ty * 'a -> frag
  | Expr : 'a t -> frag
  | Col_frag : ('r, _) col -> frag

and any = Any : _ t -> any
and order = Order : _ t * string * nulls -> order
and window_spec = { partition_by : any list; order_by : order list }

and 'a t =
  | Col : int option * ('r, 'a) col -> 'a t
  | Opt_col : int * ('r, 'a) col -> 'a option t
  | Opt_col_flat : int * ('r, 'a option) col -> 'a option t
  | Excluded : ('r, 'a) col -> 'a t
  | Lit : 'a ty * 'a -> 'a t
  | Null : 'a ty -> 'a option t
  | Some_ : 'a t -> 'a option t
  | Binop : string * 'a t * 'b t * 'c ty -> 'c t
  | Concat : string t * string t -> string t
  | Cmp : cmp_op * 'a t * 'a t -> bool t
  | Cmp_opt : cmp_op * 'a option t * 'a t -> bool t
  | Tuple_cmp : cmp_op * any list * any list -> bool t
  | And : bool t * bool t -> bool t
  | Or : bool t * bool t -> bool t
  | Not : bool t -> bool t
  | Is_null : 'a option t -> bool t
  | Is_not_null : 'a option t -> bool t
  | In_list : 'a t * 'a ty * 'a list -> bool t
  | Between : 'a t * 'a t * 'a t -> bool t
  | Like : string t * string t * bool -> bool t
  | Like_opt : string option t * string t * bool -> bool t
  | Coalesce : 'a option t * 'a t -> 'a t
  | Coalesce_opt : 'a option t * 'a option t -> 'a option t
  | Nullif : 'a t * 'a t -> 'a option t
  | Case : (bool t * 'a t) list * 'a t option * 'a ty -> 'a t
  | Count_all : int t
  | Count : _ t * bool -> int t
  | Sum_int : int t -> int option t
  | Sum_int64 : int64 t -> int64 option t
  | Sum_float : float t -> float option t
  | Avg : _ t -> float option t
  | Min : 'a t -> 'a option t
  | Max : 'a t -> 'a option t
  | Min_opt : 'a option t -> 'a option t
  | Max_opt : 'a option t -> 'a option t
  | Filter : 'a t * bool t -> 'a t
  | Over : 'a t * window_spec -> 'a t
  | Win_rank : string * window_spec -> int t
  | Win_nav :
      string * 'a t * int option * 'a t option * window_spec * 'b ty
      -> 'b t
  | Lower : string t -> string t
  | Upper : string t -> string t
  | Trim : string t -> string t
  | Length : string t -> int t
  | Now : Ptime.t t
  | Cast : 'b ty * _ t -> 'b t
  | Json_get : _ t * string t * bool -> string option t (* true: ->> text *)
  | Json_contains : _ t * string t -> bool t
  | Json_has_key : _ t * string t -> bool t
  | Json_path_text : _ t * string list -> string option t
  | Pg_any : 'a t * string t -> bool t
  | Date_trunc : string * Ptime.t t -> Ptime.t t
  | Raw : 'a ty * frag list -> 'a t
  | Sub : 'a ty * (out -> unit) -> 'a t

let any e = Any e
let asc e = Order (e, "ASC", Nulls_default)
let desc e = Order (e, "DESC", Nulls_default)
let nulls_first (Order (e, dir, _)) = Order (e, dir, Nulls_first)
let nulls_last (Order (e, dir, _)) = Order (e, dir, Nulls_last)

let rec ty_of : type a. a t -> a ty = function
  | Col (_, c) -> c.ty
  | Opt_col (_, c) -> nullable c.ty
  | Opt_col_flat (_, c) -> c.ty
  | Excluded c -> c.ty
  | Lit (ty, _) -> ty
  | Null ty -> nullable ty
  | Some_ e -> nullable (ty_of e)
  | Binop (_, _, _, ty) -> ty
  | Concat _ -> text
  | Cmp _ -> bool
  | Cmp_opt _ -> bool
  | Tuple_cmp _ -> bool
  | And _ -> bool
  | Or _ -> bool
  | Not _ -> bool
  | Is_null _ -> bool
  | Is_not_null _ -> bool
  | In_list _ -> bool
  | Between _ -> bool
  | Like _ -> bool
  | Like_opt _ -> bool
  | Coalesce (_, fallback) -> ty_of fallback
  | Coalesce_opt (a, _) -> ty_of a
  | Nullif (a, _) -> nullable (ty_of a)
  | Case (_, _, ty) -> ty
  | Count_all -> int
  | Count _ -> int
  | Sum_int _ -> nullable int
  | Sum_int64 _ -> nullable int64
  | Sum_float _ -> nullable float
  | Avg _ -> nullable float
  | Min e -> nullable (ty_of e)
  | Max e -> nullable (ty_of e)
  | Min_opt e -> ty_of e
  | Max_opt e -> ty_of e
  | Filter (e, _) -> ty_of e
  | Over (e, _) -> ty_of e
  | Win_rank _ -> int
  | Win_nav (_, _, _, _, _, ty) -> ty
  | Lower _ -> text
  | Upper _ -> text
  | Trim _ -> text
  | Length _ -> int
  | Now -> ptime
  | Cast (ty, _) -> ty
  | Json_get _ -> nullable text
  | Json_contains _ -> bool
  | Json_has_key _ -> bool
  | Json_path_text _ -> nullable text
  | Pg_any _ -> bool
  | Date_trunc _ -> ptime
  | Raw (ty, _) -> ty
  | Sub (ty, _) -> ty

(* Constructors *)

let col c = Col (None, c)
let ref_col id c = Col (Some id, c)
let opt_ref_col id c = Opt_col (id, c)
let opt_ref_col_flat id c = Opt_col_flat (id, c)
let excluded c = Excluded c
let lit ty v = Lit (ty, v)
let int v = Lit (Ty.int, v)
let int64 v = Lit (Ty.int64, v)
let float v = Lit (Ty.float, v)
let text v = Lit (Ty.text, v)
let bool v = Lit (Ty.bool, v)
let ptime v = Lit (Ty.ptime, v)
let date v = Lit (Ty.date, v)
let uuid v = Lit (Ty.uuid, v)
let json v = Lit (Ty.json, v)
let null ty = Null ty
let some e = Some_ e
let lower s = Lower s
let upper s = Upper s
let trim s = Trim s
let length s = Length s
let concat a b = Concat (a, b)
let coalesce a b = Coalesce (a, b)
let coalesce_opt a b = Coalesce_opt (a, b)
let nullif a b = Nullif (a, b)
let now () = Now
let cast ty e = Cast (ty, e)

let case_when branches ~else_ =
  match branches with
  | [] -> else_
  | _ -> Case (branches, Some else_, ty_of else_)

let case_when_opt branches =
  match branches with
  | [] -> invalid_arg "Qb.Expr.case_when_opt: at least one branch is required"
  | (_, first) :: _ ->
      let branches = List.map (fun (c, v) -> (c, Some_ v)) branches in
      Case (branches, None, nullable (ty_of first))

let count_all = Count_all
let count e = Count (e, false)
let count_distinct e = Count (e, true)
let sum_int e = Sum_int e
let sum_int64 e = Sum_int64 e
let sum_float e = Sum_float e
let avg e = Avg e
let min e = Min e
let max e = Max e
let min_opt e = Min_opt e
let max_opt e = Max_opt e
let filter cond agg = Filter (agg, cond)

let over ?(partition_by = []) ?(order_by = []) e =
  Over (e, { partition_by; order_by })

let row_number ?(partition_by = []) ?(order_by = []) () =
  Win_rank ("ROW_NUMBER", { partition_by; order_by })

let rank ?(partition_by = []) ?(order_by = []) () =
  Win_rank ("RANK", { partition_by; order_by })

let dense_rank ?(partition_by = []) ?(order_by = []) () =
  Win_rank ("DENSE_RANK", { partition_by; order_by })

let lag ?offset ?(partition_by = []) ?(order_by = []) e =
  Win_nav
    ("LAG", e, offset, None, { partition_by; order_by }, nullable (ty_of e))

let lag_default ?(offset = 1) ~default ?(partition_by = []) ?(order_by = []) e =
  Win_nav
    ("LAG", e, Some offset, Some default, { partition_by; order_by }, ty_of e)

let lead ?offset ?(partition_by = []) ?(order_by = []) e =
  Win_nav
    ("LEAD", e, offset, None, { partition_by; order_by }, nullable (ty_of e))

let lead_default ?(offset = 1) ~default ?(partition_by = []) ?(order_by = []) e
    =
  Win_nav
    ("LEAD", e, Some offset, Some default, { partition_by; order_by }, ty_of e)

let first_value ?(partition_by = []) ?(order_by = []) e =
  Win_nav
    ( "FIRST_VALUE",
      e,
      None,
      None,
      { partition_by; order_by },
      nullable (ty_of e) )

let last_value ?(partition_by = []) ?(order_by = []) e =
  Win_nav
    ("LAST_VALUE", e, None, None, { partition_by; order_by }, nullable (ty_of e))

let sub ty render = Sub (ty, render)

module Keyset = struct
  let cmp2 op (a1, a2) (b1, b2) =
    Tuple_cmp (op, [ Any a1; Any a2 ], [ Any b1; Any b2 ])

  let cmp3 op (a1, a2, a3) (b1, b2, b3) =
    Tuple_cmp (op, [ Any a1; Any a2; Any a3 ], [ Any b1; Any b2; Any b3 ])

  let gt2 a b = cmp2 Gt a b
  let ge2 a b = cmp2 Ge a b
  let lt2 a b = cmp2 Lt a b
  let le2 a b = cmp2 Le a b
  let gt3 a b = cmp3 Gt a b
  let ge3 a b = cmp3 Ge a b
  let lt3 a b = cmp3 Lt a b
  let le3 a b = cmp3 Le a b
end

module Json = struct
  let get doc key = Json_get (doc, key, false)
  let get_text doc key = Json_get (doc, key, true)
  let get_opt doc key = Json_get (doc, key, false)
  let get_text_opt doc key = Json_get (doc, key, true)
  let contains doc sub = Json_contains (doc, sub)
  let contains_opt doc sub = Json_contains (doc, sub)
  let has_key doc key = Json_has_key (doc, key)
  let has_key_opt doc key = Json_has_key (doc, key)

  let path_text doc path =
    if path = [] then invalid_arg "Qb.Expr.Json.path_text: empty path";
    Json_path_text (doc, path)

  let path_text_opt doc path =
    if path = [] then invalid_arg "Qb.Expr.Json.path_text_opt: empty path";
    Json_path_text (doc, path)
end

module Pg = struct
  let any_array elem arr = Pg_any (elem, arr)
  let date_trunc unit ts = Date_trunc (unit, ts)
end

module Raw = struct
  type nonrec frag = frag

  let sql s = Sql s
  let param ty v = Param (ty, v)
  let expr e = Expr e
  let col c = Col_frag c
  let make ty frags = Raw (ty, frags)
  let bool frags = Raw (Ty.bool, frags)
end

module Ops = struct
  let ( = ) a b = Cmp (Eq, a, b)
  let ( <> ) a b = Cmp (Ne, a, b)
  let ( > ) a b = Cmp (Gt, a, b)
  let ( >= ) a b = Cmp (Ge, a, b)
  let ( < ) a b = Cmp (Lt, a, b)
  let ( <= ) a b = Cmp (Le, a, b)
  let ( =? ) a b = Cmp_opt (Eq, a, b)
  let ( <>? ) a b = Cmp_opt (Ne, a, b)
  let ( >? ) a b = Cmp_opt (Gt, a, b)
  let ( >=? ) a b = Cmp_opt (Ge, a, b)
  let ( <? ) a b = Cmp_opt (Lt, a, b)
  let ( <=? ) a b = Cmp_opt (Le, a, b)
  let ( && ) a b = And (a, b)
  let ( || ) a b = Or (a, b)
  let not a = Not a

  let all_of = function
    | [] -> None
    | c :: cs -> Some (List.fold_left ( && ) c cs)

  let any_of = function
    | [] -> None
    | c :: cs -> Some (List.fold_left ( || ) c cs)

  let is_null e = Is_null e
  let is_not_null e = Is_not_null e
  let is_in e vs = In_list (e, ty_of e, vs)
  let not_in e vs = Not (In_list (e, ty_of e, vs))
  let between e lo hi = Between (e, lo, hi)
  let like a pat = Like (a, pat, false)
  let ilike a pat = Like (a, pat, true)
  let like_opt a pat = Like_opt (a, pat, false)
  let ilike_opt a pat = Like_opt (a, pat, true)
  let ( + ) a b = Binop ("+", a, b, Ty.int)
  let ( - ) a b = Binop ("-", a, b, Ty.int)
  let ( * ) a b = Binop ("*", a, b, Ty.int)
  let ( / ) a b = Binop ("/", a, b, Ty.int)
  let ( +. ) a b = Binop ("+", a, b, Ty.float)
  let ( -. ) a b = Binop ("-", a, b, Ty.float)
  let ( *. ) a b = Binop ("*", a, b, Ty.float)
  let ( /. ) a b = Binop ("/", a, b, Ty.float)
  let ( ^ ) a b = Concat (a, b)
end

(* Rendering *)

let is_null_expr : type a. a t -> bool = function
  | Null _ -> true
  | Lit (ty, v) -> ty.is_null v
  | _ -> false

(* MySQL's CAST() accepts CHAR/SIGNED/DOUBLE rather than TEXT/BIGINT. *)
let cast_type dialect (ty : _ ty) =
  let s = ty.ddl dialect in
  match dialect with
  | Mysql -> (
      match String.uppercase_ascii s with
      | "TEXT" -> "CHAR"
      | "BIGINT" | "INTEGER" | "INT" -> "SIGNED"
      | _ -> s)
  | Postgres | Sqlite -> s

(* For aggregates that need an outer CAST on Postgres/SQLite/MySQL (SUM/AVG),
   FILTER (WHERE ...) and OVER (...) must sit inside CAST(... AS T), not outside. *)
let rec unwrap_modifiers : type a.
    bool t list ->
    window_spec option ->
    a t ->
    a t * bool t list * window_spec option =
 fun filters win -> function
  | Filter (inner, cond) -> unwrap_modifiers (cond :: filters) win inner
  | Over (inner, w) -> unwrap_modifiers filters (Some w) inner
  | base -> (base, filters, win)

let rec render : type a. out -> a t -> unit =
 fun o e ->
  match e with
  | Filter _ | Over _ ->
      let base, filters, win = unwrap_modifiers [] None e in
      render_with_mods o base filters win
  | Sum_int _ | Sum_int64 _ | Sum_float _ | Avg _ ->
      render_with_mods o e [] None
  | Col (None, c) -> colname o c
  | Col (Some id, c) -> alias_colname o (lookup_ref o id) c
  | Opt_col (id, c) -> alias_colname o (lookup_ref o id) c
  | Opt_col_flat (id, c) -> alias_colname o (lookup_ref o id) c
  | Excluded c ->
      str o "EXCLUDED.";
      name o c.name
  | Lit (ty, v) -> param_ty o ty v
  | Null _ -> str o "NULL"
  | Some_ e -> render o e
  | Binop (op, a, b, _) ->
      str o "(";
      render o a;
      str o (" " ^ op ^ " ");
      render o b;
      str o ")"
  | Concat (a, b) -> (
      match o.dialect with
      | Mysql ->
          str o "CONCAT(";
          render o a;
          str o ", ";
          render o b;
          str o ")"
      | Postgres | Sqlite ->
          str o "(";
          render o a;
          str o " || ";
          render o b;
          str o ")")
  | Cmp (Eq, a, b) when is_null_expr b ->
      render o a;
      str o " IS NULL"
  | Cmp (Ne, a, b) when is_null_expr b ->
      render o a;
      str o " IS NOT NULL"
  | Cmp (Eq, a, b) when is_null_expr a ->
      render o b;
      str o " IS NULL"
  | Cmp (Ne, a, b) when is_null_expr a ->
      render o b;
      str o " IS NOT NULL"
  | Cmp (op, a, b) ->
      render o a;
      str o (" " ^ cmp_op_sql op ^ " ");
      render o b
  | Cmp_opt (op, a, b) ->
      render o a;
      str o (" " ^ cmp_op_sql op ^ " ");
      render o b
  | Tuple_cmp (op, lhs, rhs) ->
      str o "(";
      sep o ", " (fun (Any x) -> render o x) lhs;
      str o (") " ^ cmp_op_sql op ^ " (");
      sep o ", " (fun (Any x) -> render o x) rhs;
      str o ")"
  | And (a, b) ->
      str o "(";
      render o a;
      str o " AND ";
      render o b;
      str o ")"
  | Or (a, b) ->
      str o "(";
      render o a;
      str o " OR ";
      render o b;
      str o ")"
  | Not a ->
      str o "NOT (";
      render o a;
      str o ")"
  | Is_null e ->
      render o e;
      str o " IS NULL"
  | Is_not_null e ->
      render o e;
      str o " IS NOT NULL"
  | In_list (_, _, []) -> str o "1 = 0"
  | In_list (e, ty, vs) ->
      o.variadic <- true;
      render o e;
      str o " IN (";
      sep o ", " (param_ty o ty) vs;
      str o ")"
  | Between (e, lo, hi) ->
      str o "(";
      render o e;
      str o " BETWEEN ";
      render o lo;
      str o " AND ";
      render o hi;
      str o ")"
  | Like (a, pat, ci) -> render_like o a pat ci
  | Like_opt (a, pat, ci) -> render_like o a pat ci
  | Coalesce (a, b) ->
      str o "COALESCE(";
      render o a;
      str o ", ";
      render o b;
      str o ")"
  | Coalesce_opt (a, b) ->
      str o "COALESCE(";
      render o a;
      str o ", ";
      render o b;
      str o ")"
  | Nullif (a, b) ->
      str o "NULLIF(";
      render o a;
      str o ", ";
      render o b;
      str o ")"
  | Case (branches, else_, _) ->
      str o "CASE";
      List.iter
        (fun (cond, value) ->
          str o " WHEN ";
          render o cond;
          str o " THEN ";
          render o value)
        branches;
      (match else_ with
      | Some e ->
          str o " ELSE ";
          render o e
      | None -> ());
      str o " END"
  | Count_all -> str o "COUNT(*)"
  | Count (e, false) ->
      str o "COUNT(";
      render o e;
      str o ")"
  | Count (e, true) ->
      str o "COUNT(DISTINCT ";
      render o e;
      str o ")"
  | Min e ->
      str o "MIN(";
      render o e;
      str o ")"
  | Max e ->
      str o "MAX(";
      render o e;
      str o ")"
  | Min_opt e ->
      str o "MIN(";
      render o e;
      str o ")"
  | Max_opt e ->
      str o "MAX(";
      render o e;
      str o ")"
  | Win_rank (fn, w) ->
      str o (fn ^ "()");
      render_window o w
  | Win_nav (fn, e, offset, default, w, _) ->
      str o (fn ^ "(");
      render o e;
      (match offset with
      | Some n -> str o (Printf.sprintf ", %d" n)
      | None -> ());
      (match default with
      | Some d ->
          str o ", ";
          render o d
      | None -> ());
      str o ")";
      render_window o w
  | Lower e ->
      str o "LOWER(";
      render o e;
      str o ")"
  | Upper e ->
      str o "UPPER(";
      render o e;
      str o ")"
  | Trim e ->
      str o "TRIM(";
      render o e;
      str o ")"
  | Length e -> (
      match o.dialect with
      | Sqlite ->
          str o "LENGTH(";
          render o e;
          str o ")"
      | Postgres | Mysql ->
          str o "CHAR_LENGTH(";
          render o e;
          str o ")")
  | Now -> (
      match o.dialect with
      | Sqlite -> str o "CURRENT_TIMESTAMP"
      | Postgres | Mysql -> str o "NOW()")
  | Cast (ty, e) ->
      str o "CAST(";
      render o e;
      str o " AS ";
      str o (cast_type o.dialect ty);
      str o ")"
  | Json_get (doc, key, as_text) ->
      str o "(";
      render o doc;
      str o (if as_text then " ->> " else " -> ");
      render o key;
      str o ")"
  | Json_contains (doc, sub) ->
      str o "(";
      render o doc;
      str o " @> CAST(";
      render o sub;
      str o " AS JSONB))"
  | Json_has_key (doc, key) ->
      str o "jsonb_exists(";
      render o doc;
      str o ", ";
      render o key;
      str o ")"
  | Json_path_text (doc, path) ->
      str o "jsonb_extract_path_text(";
      render o doc;
      List.iter
        (fun seg ->
          str o ", ";
          param_ty o Ty.text seg)
        path;
      str o ")"
  | Pg_any (elem, arr) ->
      render o elem;
      str o " = ANY(";
      render o arr;
      str o ")"
  | Date_trunc (unit, ts) ->
      str o "date_trunc(";
      param_ty o Ty.text unit;
      str o ", ";
      render o ts;
      str o ")"
  | Raw (_, frags) -> List.iter (render_frag o) frags
  | Sub (_, f) -> f o

and render_mods o filters win =
  (match filters with
  | [] -> ()
  | c :: cs ->
      let combined = List.fold_left (fun acc x -> And (acc, x)) c cs in
      str o " FILTER (WHERE ";
      render o combined;
      str o ")");
  Option.iter (render_window o) win

and render_with_mods : type a.
    out -> a t -> bool t list -> window_spec option -> unit =
 fun o base filters win ->
  let wrap_cast cast_ty inner =
    str o "CAST(";
    inner ();
    render_mods o filters win;
    str o (" AS " ^ cast_ty ^ ")")
  in
  let plain inner =
    inner ();
    render_mods o filters win
  in
  match base with
  | Sum_int e ->
      let inner () =
        str o "SUM(";
        render o e;
        str o ")"
      in
      if o.dialect = Postgres then wrap_cast "BIGINT" inner else plain inner
  | Sum_int64 e ->
      let inner () =
        str o "SUM(";
        render o e;
        str o ")"
      in
      if o.dialect = Postgres then wrap_cast "BIGINT" inner else plain inner
  | Sum_float e ->
      let inner () =
        str o "SUM(";
        render o e;
        str o ")"
      in
      if o.dialect = Postgres then wrap_cast "DOUBLE PRECISION" inner
      else plain inner
  | Avg e ->
      let inner () =
        str o "AVG(";
        render o e;
        str o ")"
      in
      let ty =
        match o.dialect with
        | Postgres -> "DOUBLE PRECISION"
        | Sqlite -> "REAL"
        | Mysql -> "DOUBLE"
      in
      wrap_cast ty inner
  | _ -> plain (fun () -> render o base)

and render_window o w =
  str o " OVER (";
  if w.partition_by <> [] then (
    str o "PARTITION BY ";
    sep o ", " (fun (Any e) -> render o e) w.partition_by;
    if w.order_by <> [] then str o " ");
  if w.order_by <> [] then (
    str o "ORDER BY ";
    sep o ", " (render_order o) w.order_by);
  str o ")"

and render_order o (Order (e, dir, nulls)) =
  match (nulls, o.dialect) with
  | Nulls_default, _ ->
      render o e;
      str o (" " ^ dir)
  | Nulls_first, (Postgres | Sqlite) ->
      render o e;
      str o (" " ^ dir ^ " NULLS FIRST")
  | Nulls_last, (Postgres | Sqlite) ->
      render o e;
      str o (" " ^ dir ^ " NULLS LAST")
  | Nulls_first, Mysql ->
      render o e;
      str o " IS NULL DESC, ";
      render o e;
      str o (" " ^ dir)
  | Nulls_last, Mysql ->
      render o e;
      str o " IS NULL ASC, ";
      render o e;
      str o (" " ^ dir)

and render_like : type a b. out -> a t -> b t -> bool -> unit =
 fun o a pat ci ->
  if not ci then (
    render o a;
    str o " LIKE ";
    render o pat)
  else if o.dialect = Postgres then (
    render o a;
    str o " ILIKE ";
    render o pat)
  else (
    str o "LOWER(";
    render o a;
    str o ") LIKE LOWER(";
    render o pat;
    str o ")")

and render_frag o = function
  | Sql s -> str o s
  | Param (ty, v) -> param_ty o ty v
  | Expr e -> render o e
  | Col_frag c -> colname o c
