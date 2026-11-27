(* Scoped multi-table query builder: arbitrary joins, aggregates, GROUP BY /
   HAVING, subqueries, CTEs, set operations, row locking, and multi-table
   writes. *)

open Ty
open Table

(* Thread- and domain-safe unique handle ids. During SQL rendering each handle
   id is assigned a deterministic alias ("t0", "t1", ...) in traversal order. *)
let next_ref_id = Atomic.make 1
let fresh_id () = Atomic.fetch_and_add next_ref_id 1

(* ---------- handles ---------- *)

type 'r ref = { rid : int; rtable : 'r table option }
type 'r opt_ref = { orid : int; ortable : 'r table option }
type 's cte = { cte_name : string }

let ( .@() ) r c = Expr.ref_col r.rid c
let ( .@?() ) r c = Expr.opt_ref_col r.orid c
let ( .@??() ) r c = Expr.opt_ref_col_flat r.orid c

(* ---------- projections ---------- *)

type render_item = out -> unit

type 'a proj =
  | Proj : 'x Caqti_type.t * ('x -> 'a) * render_item list -> 'a proj

let expr e =
  let ty = Expr.ty_of e in
  Proj (ty.caqti, Fun.id, [ (fun o -> Expr.render o e) ])

let whole r =
  match r.rtable with
  | Some t -> (
      match decoder t.row with
      | Dec (ct, dec, cols) ->
          let items =
            List.map
              (fun (Any c) o -> alias_colname o (lookup_ref o r.rid) c)
              cols
          in
          Proj (ct, dec, items))
  | None ->
      invalid_arg
        "Qb.Query.whole: only base tables can be selected whole; select \
         exported columns for a CTE or subquery"

let whole_opt r =
  match r.ortable with
  | Some t -> (
      match decoder t.row with
      | Dec (ct, dec, cols) ->
          let items =
            List.map
              (fun (Any c) o -> alias_colname o (lookup_ref o r.orid) c)
              cols
          in
          Proj (Caqti_type.option ct, Option.map dec, items))
  | None ->
      invalid_arg
        "Qb.Query.whole_opt: only base tables can be selected whole; select \
         exported columns for a CTE or subquery"

let unit = Proj (Caqti_type.unit, Fun.id, [])
let map f (Proj (ct, dec, items)) = Proj (ct, (fun x -> f (dec x)), items)

let both (Proj (ta, da, ia)) (Proj (tb, db, ib)) =
  Proj (Caqti_type.t2 ta tb, (fun (x, y) -> (da x, db y)), ia @ ib)

let ( let+ ) p f = map f p
let ( and+ ) = both

(* ---------- named projections (for CTEs and derived tables) ---------- *)

type named_item = string * (out -> unit)
type ('s, 'a) named = Named of named_item list * 'a

let field alias_name e =
  let ty = Expr.ty_of e in
  let c : ('s, 'a) col =
    {
      name = alias_name;
      ty;
      get = (fun _ -> invalid_arg "Qb.Query.field: virtual column");
      primary_key = false;
      auto_increment = false;
      path = [];
    }
  in
  Named ([ (alias_name, fun o -> Expr.render o e) ], c)

module Named = struct
  let map f (Named (items, v)) = Named (items, f v)
  let both (Named (ia, va)) (Named (ib, vb)) = Named (ia @ ib, (va, vb))
  let ( let+ ) n f = map f n
  let ( and+ ) = both
end

(* ---------- query AST ---------- *)

type join_kind = Inner | Left | Right | Full | Cross

let join_keyword = function
  | Inner -> " INNER JOIN "
  | Left -> " LEFT JOIN "
  | Right -> " RIGHT JOIN "
  | Full -> " FULL JOIN "
  | Cross -> " CROSS JOIN "

type order = Expr.order

let asc = Expr.asc
let desc = Expr.desc
let nulls_first = Expr.nulls_first
let nulls_last = Expr.nulls_last

type lock_mode =
  | For_update
  | For_update_nowait
  | For_update_skip_locked
  | For_share
  | For_share_nowait
  | For_share_skip_locked

type compound_op = Union | Union_all | Intersect | Except

let compound_sql = function
  | Union -> " UNION "
  | Union_all -> " UNION ALL "
  | Intersect -> " INTERSECT "
  | Except -> " EXCEPT "

type source =
  | Table_src : 'r table * int -> source
  | Subquery_src : select_plan * int -> source
  | Cte_src : string * int -> source

and join_item = { kind : join_kind; source : source; on : bool Expr.t option }

and cte_def = {
  cte_def_name : string;
  cte_plan : select_plan;
  cte_step : select_plan option;
  cte_union_all : bool;
}

and select_plan = {
  ctes : cte_def list;
  distinct : bool;
  distinct_on : Expr.any list;
  select_items : (string option * render_item) list;
  from : source option;
  joins : join_item list;
  wheres : bool Expr.t list;
  group_by : Expr.any list;
  havings : bool Expr.t list;
  order_by : order list;
  limit : int option;
  offset : int option;
  lock : lock_mode option;
}

type query_body =
  | Simple of select_plan
  | Compound of {
      op : compound_op;
      left : query_body;
      right : query_body;
      order_by : order list;
      limit : int option;
      offset : int option;
    }

type 'a t =
  | Query : 'x Caqti_type.t * ('x -> 'a) * query_body * 'a single_ty -> 'a t

and 'a single_ty = Single : 'a ty -> 'a single_ty | Multi : _ single_ty

type ('s, 'exports) sub_query = {
  sub_plan : select_plan;
  sub_exports : 'exports;
}

(* ---------- pure builder state ---------- *)

type state = {
  st_ctes : cte_def list; (* reversed *)
  st_from : source option;
  st_joins : join_item list; (* reversed *)
  st_distinct : bool;
  st_distinct_on : Expr.any list;
  st_wheres : bool Expr.t list; (* reversed *)
  st_group_by : Expr.any list; (* reversed *)
  st_havings : bool Expr.t list; (* reversed *)
  st_order_by : order list;
  st_limit : int option;
  st_offset : int option;
  st_lock : lock_mode option;
}

let empty_state =
  {
    st_ctes = [];
    st_from = None;
    st_joins = [];
    st_distinct = false;
    st_distinct_on = [];
    st_wheres = [];
    st_group_by = [];
    st_havings = [];
    st_order_by = [];
    st_limit = None;
    st_offset = None;
    st_lock = None;
  }

type 'a build = state -> state * 'a

let return x st = (st, x)

let ( let* ) m f st =
  let st, x = m st in
  f x st

let check_from st =
  if Option.is_some st.st_from then
    invalid_arg "Qb.Query.from: FROM was already set; use join for more tables"

let check_has_from fn st =
  if Option.is_none st.st_from then
    invalid_arg (fn ^ ": call from before joining")

let from table st =
  base "Qb.Query.from" table;
  check_from st;
  let id = fresh_id () in
  ( { st with st_from = Some (Table_src (table, id)) },
    { rid = id; rtable = Some table } )

let add_join kind table ~on st =
  base "Qb.Query.join" table;
  check_has_from "Qb.Query.join" st;
  let id = fresh_id () in
  let r = { rid = id; rtable = Some table } in
  let cond = on r in
  let j = { kind; source = Table_src (table, id); on = Some cond } in
  ({ st with st_joins = j :: st.st_joins }, (id, r))

let join table ~on st =
  let st, (_, r) = add_join Inner table ~on st in
  (st, r)

let left_join table ~on st =
  let st, (id, _) = add_join Left table ~on st in
  (st, { orid = id; ortable = Some table })

let right_join table ~on st =
  let st, (id, _) = add_join Right table ~on st in
  (st, { orid = id; ortable = Some table })

let full_join table ~on st =
  let st, (id, _) = add_join Full table ~on st in
  (st, { orid = id; ortable = Some table })

let cross_join table st =
  base "Qb.Query.cross_join" table;
  check_has_from "Qb.Query.cross_join" st;
  let id = fresh_id () in
  let j = { kind = Cross; source = Table_src (table, id); on = None } in
  ({ st with st_joins = j :: st.st_joins }, { rid = id; rtable = Some table })

(* Relation-driven joins *)

let join_many (type a b) (parent_ref : a ref) (rel : (a, b) Rel.has_many) =
  match rel with
  | Rel.Has_many { parent_key; child; child_key } ->
      join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) = parent_ref.@(parent_key)))
  | Rel.Has_many_opt { parent_key; child; child_key } ->
      join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) =? parent_ref.@(parent_key)))

let left_join_many (type a b) (parent_ref : a ref) (rel : (a, b) Rel.has_many) =
  match rel with
  | Rel.Has_many { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) = parent_ref.@(parent_key)))
  | Rel.Has_many_opt { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) =? parent_ref.@(parent_key)))

let left_join_many_opt (type a b) (parent_ref : a opt_ref)
    (rel : (a, b) Rel.has_many) =
  match rel with
  | Rel.Has_many { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(parent_ref.@?(parent_key) =? c.@(child_key)))
  | Rel.Has_many_opt { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) = parent_ref.@?(parent_key)))

let join_one (type a b) (child_ref : a ref) (rel : (a, b) Rel.belongs_to) =
  match rel with
  | Rel.Belongs_to { child_key; parent; parent_key } ->
      join parent ~on:(fun p ->
          Expr.Ops.(child_ref.@(child_key) = p.@(parent_key)))
  | Rel.Belongs_to_opt { child_key; parent; parent_key } ->
      join parent ~on:(fun p ->
          Expr.Ops.(child_ref.@(child_key) =? p.@(parent_key)))

let left_join_one (type a b) (child_ref : a ref) (rel : (a, b) Rel.belongs_to) =
  match rel with
  | Rel.Belongs_to { child_key; parent; parent_key } ->
      left_join parent ~on:(fun p ->
          Expr.Ops.(child_ref.@(child_key) = p.@(parent_key)))
  | Rel.Belongs_to_opt { child_key; parent; parent_key } ->
      left_join parent ~on:(fun p ->
          Expr.Ops.(child_ref.@(child_key) =? p.@(parent_key)))

let left_join_one_opt (type a b) (child_ref : a opt_ref)
    (rel : (a, b) Rel.belongs_to) =
  match rel with
  | Rel.Belongs_to { child_key; parent; parent_key } ->
      left_join parent ~on:(fun p ->
          Expr.Ops.(child_ref.@?(child_key) =? p.@(parent_key)))
  | Rel.Belongs_to_opt { child_key; parent; parent_key } ->
      left_join parent ~on:(fun p ->
          Expr.Ops.(child_ref.@??(child_key) =? p.@(parent_key)))

let join_has_one (type a b) (parent_ref : a ref) (rel : (a, b) Rel.has_one) =
  match rel with
  | Rel.Has_one { parent_key; child; child_key } ->
      join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) = parent_ref.@(parent_key)))
  | Rel.Has_one_opt { parent_key; child; child_key } ->
      join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) =? parent_ref.@(parent_key)))

let left_join_has_one (type a b) (parent_ref : a ref) (rel : (a, b) Rel.has_one)
    =
  match rel with
  | Rel.Has_one { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) = parent_ref.@(parent_key)))
  | Rel.Has_one_opt { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) =? parent_ref.@(parent_key)))

let left_join_has_one_opt (type a b) (parent_ref : a opt_ref)
    (rel : (a, b) Rel.has_one) =
  match rel with
  | Rel.Has_one { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(parent_ref.@?(parent_key) =? c.@(child_key)))
  | Rel.Has_one_opt { parent_key; child; child_key } ->
      left_join child ~on:(fun c ->
          Expr.Ops.(c.@(child_key) = parent_ref.@?(parent_key)))

let join_m2m (type a b) (src_ref : a ref) (rel : (a, b) Rel.many_to_many) =
  match rel with
  | Rel.Many_to_many
      {
        source_key;
        through;
        through_source_key;
        through_target_key;
        target;
        target_key;
      } ->
      let* j =
        join through ~on:(fun j ->
            Expr.Ops.(j.@(through_source_key) = src_ref.@(source_key)))
      in
      join target ~on:(fun t ->
          Expr.Ops.(t.@(target_key) = j.@(through_target_key)))

let left_join_m2m (type a b) (src_ref : a ref) (rel : (a, b) Rel.many_to_many) =
  match rel with
  | Rel.Many_to_many
      {
        source_key;
        through;
        through_source_key;
        through_target_key;
        target;
        target_key;
      } ->
      let* j =
        left_join through ~on:(fun j ->
            Expr.Ops.(j.@(through_source_key) = src_ref.@(source_key)))
      in
      left_join target ~on:(fun t ->
          Expr.Ops.(j.@?(through_target_key) =? t.@(target_key)))

let left_join_m2m_opt (type a b) (src_ref : a opt_ref)
    (rel : (a, b) Rel.many_to_many) =
  match rel with
  | Rel.Many_to_many
      {
        source_key;
        through;
        through_source_key;
        through_target_key;
        target;
        target_key;
      } ->
      let* j =
        left_join through ~on:(fun j ->
            Expr.Ops.(src_ref.@?(source_key) =? j.@(through_source_key)))
      in
      left_join target ~on:(fun t ->
          Expr.Ops.(j.@?(through_target_key) =? t.@(target_key)))

(* CTEs and derived tables *)

let make_plan st select_items =
  {
    ctes = List.rev st.st_ctes;
    distinct = st.st_distinct;
    distinct_on = st.st_distinct_on;
    select_items;
    from = st.st_from;
    joins = List.rev st.st_joins;
    wheres = List.rev st.st_wheres;
    group_by = List.rev st.st_group_by;
    havings = List.rev st.st_havings;
    order_by = st.st_order_by;
    limit = st.st_limit;
    offset = st.st_offset;
    lock = st.st_lock;
  }

let select_named (Named (items, exports)) st =
  if items = [] then invalid_arg "Qb.Query.select_named: no columns";
  let select_items = List.map (fun (n, f) -> (Some n, f)) items in
  (st, { sub_plan = make_plan st select_items; sub_exports = exports })

let with_cte cte_name sub_build st =
  let _, sq = sub_build empty_state in
  let def =
    {
      cte_def_name = cte_name;
      cte_plan = sq.sub_plan;
      cte_step = None;
      cte_union_all = true;
    }
  in
  ({ st with st_ctes = def :: st.st_ctes }, ({ cte_name }, sq.sub_exports))

let with_recursive_cte ?(union_all = true) cte_name ~base ~step st =
  let _, base_sq = base empty_state in
  let self = { cte_name } in
  let _, step_sq = step self base_sq.sub_exports empty_state in
  let def =
    {
      cte_def_name = cte_name;
      cte_plan = base_sq.sub_plan;
      cte_step = Some step_sq.sub_plan;
      cte_union_all = union_all;
    }
  in
  ({ st with st_ctes = def :: st.st_ctes }, (self, base_sq.sub_exports))

let from_cte cte st =
  check_from st;
  let id = fresh_id () in
  ( { st with st_from = Some (Cte_src (cte.cte_name, id)) },
    { rid = id; rtable = None } )

let join_cte cte ~on st =
  check_has_from "Qb.Query.join_cte" st;
  let id = fresh_id () in
  let r = { rid = id; rtable = None } in
  let j =
    { kind = Inner; source = Cte_src (cte.cte_name, id); on = Some (on r) }
  in
  ({ st with st_joins = j :: st.st_joins }, r)

let left_join_cte cte ~on st =
  check_has_from "Qb.Query.left_join_cte" st;
  let id = fresh_id () in
  let r = { rid = id; rtable = None } in
  let j =
    { kind = Left; source = Cte_src (cte.cte_name, id); on = Some (on r) }
  in
  ({ st with st_joins = j :: st.st_joins }, { orid = id; ortable = None })

let from_subquery sub_build st =
  check_from st;
  let _, sq = sub_build empty_state in
  let id = fresh_id () in
  ( { st with st_from = Some (Subquery_src (sq.sub_plan, id)) },
    ({ rid = id; rtable = None }, sq.sub_exports) )

let join_subquery sub_build ~on st =
  check_has_from "Qb.Query.join_subquery" st;
  let _, sq = sub_build empty_state in
  let id = fresh_id () in
  let r = { rid = id; rtable = None } in
  let j =
    {
      kind = Inner;
      source = Subquery_src (sq.sub_plan, id);
      on = Some (on r sq.sub_exports);
    }
  in
  ({ st with st_joins = j :: st.st_joins }, (r, sq.sub_exports))

let left_join_subquery sub_build ~on st =
  check_has_from "Qb.Query.left_join_subquery" st;
  let _, sq = sub_build empty_state in
  let id = fresh_id () in
  let r = { rid = id; rtable = None } in
  let j =
    {
      kind = Left;
      source = Subquery_src (sq.sub_plan, id);
      on = Some (on r sq.sub_exports);
    }
  in
  ( { st with st_joins = j :: st.st_joins },
    ({ orid = id; ortable = None }, sq.sub_exports) )

(* Clauses *)

let where cond st = ({ st with st_wheres = cond :: st.st_wheres }, ())

let where_opt cond st =
  match cond with None -> (st, ()) | Some c -> where c st

let when_ cond f st = if cond then f () st else (st, ())
let opt o f st = match o with None -> (st, ()) | Some x -> f x st
let distinct () st = ({ st with st_distinct = true }, ())

let distinct_on exprs st =
  if exprs = [] then invalid_arg "Qb.Query.distinct_on: empty expression list";
  ({ st with st_distinct_on = exprs }, ())

let group_by exprs st =
  ({ st with st_group_by = List.rev_append exprs st.st_group_by }, ())

let having cond st = ({ st with st_havings = cond :: st.st_havings }, ())

let having_opt cond st =
  match cond with None -> (st, ()) | Some c -> having c st

let order_by orders st = ({ st with st_order_by = orders }, ())

let limit n st =
  if n < 0 then invalid_arg "Qb.Query.limit: negative limit";
  ({ st with st_limit = Some n }, ())

let offset n st =
  if n < 0 then invalid_arg "Qb.Query.offset: negative offset";
  ({ st with st_offset = Some n }, ())

let for_update ?(nowait = false) ?(skip_locked = false) () st =
  if nowait && skip_locked then
    invalid_arg "Qb.Query.for_update: choose at most one of nowait, skip_locked";
  let mode =
    if nowait then For_update_nowait
    else if skip_locked then For_update_skip_locked
    else For_update
  in
  ({ st with st_lock = Some mode }, ())

let for_share ?(nowait = false) ?(skip_locked = false) () st =
  if nowait && skip_locked then
    invalid_arg "Qb.Query.for_share: choose at most one of nowait, skip_locked";
  let mode =
    if nowait then For_share_nowait
    else if skip_locked then For_share_skip_locked
    else For_share
  in
  ({ st with st_lock = Some mode }, ())

(* Finishing a query *)

let select (Proj (ct, dec, items)) st =
  if items = [] then invalid_arg "Qb.Query.select: no columns";
  let select_items = List.map (fun f -> (None, f)) items in
  let plan = make_plan st select_items in
  (st, Query (ct, dec, Simple plan, Multi))

let select_one e st =
  let ty = Expr.ty_of e in
  let plan = make_plan st [ (None, fun o -> Expr.render o e) ] in
  (st, Query (ty.caqti, Fun.id, Simple plan, Single ty))

let build b = Stdlib.snd (b empty_state)

let sub b =
  let st, () = b empty_state in
  let plan = make_plan st [ (None, fun o -> str o "1") ] in
  Query (Caqti_type.unit, Fun.id, Simple plan, Multi)

(* Compound queries *)

let combine op (Query (ct, dec, left, sty)) (Query (_, _, right, _)) =
  Query
    ( ct,
      dec,
      Compound { op; left; right; order_by = []; limit = None; offset = None },
      sty )

let union a b = combine Union a b
let union_all a b = combine Union_all a b
let intersect a b = combine Intersect a b
let except a b = combine Except a b

let with_limit n (Query (ct, dec, body, sty)) =
  if n < 0 then invalid_arg "Qb.Query.with_limit: negative limit";
  let body =
    match body with
    | Simple p -> Simple { p with limit = Some n }
    | Compound c -> Compound { c with limit = Some n }
  in
  Query (ct, dec, body, sty)

let with_offset n (Query (ct, dec, body, sty)) =
  if n < 0 then invalid_arg "Qb.Query.with_offset: negative offset";
  let body =
    match body with
    | Simple p -> Simple { p with offset = Some n }
    | Compound c -> Compound { c with offset = Some n }
  in
  Query (ct, dec, body, sty)

(* ---------- SQL rendering ---------- *)

let register_source o = function
  | Table_src (_, id) | Subquery_src (_, id) | Cte_src (_, id) ->
      ignore (register_ref o id)

let render_order = Expr.render_order

let render_limit_offset o limit offset =
  match (limit, offset, o.dialect) with
  | Some n, Some m, _ -> str o (Printf.sprintf " LIMIT %d OFFSET %d" n m)
  | Some n, None, _ -> str o (Printf.sprintf " LIMIT %d" n)
  | None, Some m, Postgres -> str o (Printf.sprintf " OFFSET %d" m)
  | None, Some m, Sqlite -> str o (Printf.sprintf " LIMIT -1 OFFSET %d" m)
  | None, Some m, Mysql ->
      str o (Printf.sprintf " LIMIT 18446744073709551615 OFFSET %d" m)
  | None, None, _ -> ()

let render_lock o = function
  | None -> ()
  | Some mode -> (
      match (mode, o.dialect) with
      | _, Sqlite -> ()
      | For_update, (Postgres | Mysql) -> str o " FOR UPDATE"
      | For_update_nowait, (Postgres | Mysql) -> str o " FOR UPDATE NOWAIT"
      | For_update_skip_locked, (Postgres | Mysql) ->
          str o " FOR UPDATE SKIP LOCKED"
      | For_share, Postgres -> str o " FOR SHARE"
      | For_share_nowait, Postgres -> str o " FOR SHARE NOWAIT"
      | For_share_skip_locked, Postgres -> str o " FOR SHARE SKIP LOCKED"
      | For_share, Mysql -> str o " FOR SHARE"
      | For_share_nowait, Mysql -> str o " FOR SHARE NOWAIT"
      | For_share_skip_locked, Mysql -> str o " FOR SHARE SKIP LOCKED")

let render_conjunction o = function
  | [] -> ()
  | [ c ] -> Expr.render o c
  | c :: cs ->
      let combined = List.fold_left (fun acc x -> Expr.Ops.(acc && x)) c cs in
      Expr.render o combined

let rec render_source o = function
  | Table_src (t, id) ->
      str o (table_name o.dialect t);
      str o " AS ";
      name o (lookup_ref o id)
  | Cte_src (cte_name, id) ->
      name o cte_name;
      str o " AS ";
      name o (lookup_ref o id)
  | Subquery_src (plan, id) ->
      str o "(";
      render_plan o plan;
      str o ") AS ";
      name o (lookup_ref o id)

and render_ctes o = function
  | [] -> ()
  | ctes ->
      let recursive = List.exists (fun c -> Option.is_some c.cte_step) ctes in
      str o (if recursive then "WITH RECURSIVE " else "WITH ");
      sep o ", "
        (fun c ->
          name o c.cte_def_name;
          str o " AS (";
          render_plan o c.cte_plan;
          (match c.cte_step with
          | None -> ()
          | Some step_plan ->
              str o (if c.cte_union_all then " UNION ALL " else " UNION ");
              render_plan o step_plan);
          str o ")")
        ctes;
      str o " "

and render_plan ?(for_count = false) o plan =
  (* Register source aliases first so SELECT can reference FROM/JOIN aliases. *)
  Option.iter (register_source o) plan.from;
  List.iter (fun j -> register_source o j.source) plan.joins;
  render_ctes o plan.ctes;
  if for_count then str o "SELECT COUNT(*)"
  else (
    (match plan.distinct_on with
    | [] -> str o (if plan.distinct then "SELECT DISTINCT " else "SELECT ")
    | exprs ->
        str o "SELECT DISTINCT ON (";
        sep o ", " (fun (Expr.Any e) -> Expr.render o e) exprs;
        str o ") ");
    sep o ", "
      (fun (alias_opt, render_item) ->
        render_item o;
        match alias_opt with
        | Some a ->
            str o " AS ";
            name o a
        | None -> ())
      plan.select_items);
  (match plan.from with
  | None -> ()
  | Some src ->
      str o " FROM ";
      render_source o src;
      List.iter
        (fun j ->
          str o (join_keyword j.kind);
          render_source o j.source;
          match j.on with
          | None -> ()
          | Some cond ->
              str o " ON ";
              Expr.render o cond)
        plan.joins);
  if plan.wheres <> [] then (
    str o " WHERE ";
    render_conjunction o plan.wheres);
  if plan.group_by <> [] then (
    str o " GROUP BY ";
    sep o ", " (fun (Expr.Any e) -> Expr.render o e) plan.group_by);
  if plan.havings <> [] then (
    str o " HAVING ";
    render_conjunction o plan.havings);
  if not for_count then (
    if plan.order_by <> [] then (
      str o " ORDER BY ";
      sep o ", " (render_order o) plan.order_by);
    render_limit_offset o plan.limit plan.offset;
    render_lock o plan.lock)

let rec render_body ?(nested = false) o = function
  | Simple plan -> render_plan o plan
  | Compound { op; left; right; order_by; limit; offset } ->
      let wrap b =
        match b with
        | Simple p when p.order_by <> [] || p.limit <> None || p.offset <> None
          ->
            str o "(";
            render_body ~nested:true o b;
            str o ")"
        | Compound _ when not nested ->
            str o "(";
            render_body ~nested:true o b;
            str o ")"
        | _ -> render_body ~nested:true o b
      in
      wrap left;
      str o (compound_sql op);
      wrap right;
      if order_by <> [] then (
        str o " ORDER BY ";
        sep o ", " (render_order o) order_by);
      render_limit_offset o limit offset

(* ---------- subquery expressions ---------- *)

let exists (Query (_, _, body, _)) =
  Expr.sub Ty.bool (fun o ->
      str o "EXISTS (";
      render_body o body;
      str o ")")

let not_exists (Query (_, _, body, _)) =
  Expr.sub Ty.bool (fun o ->
      str o "NOT EXISTS (";
      render_body o body;
      str o ")")

let single_ty_of fn = function
  | Single ty -> ty
  | Multi ->
      invalid_arg
        (fn ^ ": subquery must select a single column (use Qb.Query.select_one)")

let in_query e (Query (_, _, body, sty)) =
  ignore (single_ty_of "Qb.Query.in_query" sty);
  Expr.sub Ty.bool (fun o ->
      Expr.render o e;
      str o " IN (";
      render_body o body;
      str o ")")

let not_in_query e (Query (_, _, body, sty)) =
  ignore (single_ty_of "Qb.Query.not_in_query" sty);
  Expr.sub Ty.bool (fun o ->
      Expr.render o e;
      str o " NOT IN (";
      render_body o body;
      str o ")")

let scalar (Query (_, _, body, sty)) =
  let ty = single_ty_of "Qb.Query.scalar" sty in
  Expr.sub (Ty.nullable ty) (fun o ->
      str o "(";
      render_body o body;
      str o ")")

let scalar_opt (Query (_, _, body, sty)) =
  let ty = single_ty_of "Qb.Query.scalar_opt" sty in
  Expr.sub ty (fun o ->
      str o "(";
      render_body o body;
      str o ")")

(* ---------- preparing and rendering SELECT / COUNT ---------- *)

type 'a prepared =
  | Prepared : 'x Caqti_type.t * ('x -> 'a) * (out -> unit) -> 'a prepared

let apply_limit limit body =
  match limit with
  | None -> body
  | Some n -> (
      match body with
      | Simple p ->
          let limit =
            match p.limit with None -> Some n | Some m -> Some (min n m)
          in
          Simple { p with limit }
      | Compound c ->
          let limit =
            match c.limit with None -> Some n | Some m -> Some (min n m)
          in
          Compound { c with limit })

let prepare ?limit (Query (ct, dec, body, _)) =
  let body = apply_limit limit body in
  Prepared (ct, dec, fun o -> render_body o body)

let to_sql ?(dialect = Postgres) q =
  let o = out dialect in
  (match prepare q with Prepared (_, _, render) -> render o);
  Buffer.contents o.buf

let render_count o (Query (_, _, body, _)) =
  match body with
  | Simple plan
    when (not plan.distinct) && plan.distinct_on = [] && plan.group_by = [] ->
      render_plan ~for_count:true o plan
  | _ ->
      let strip_tail = function
        | Simple p ->
            Simple { p with order_by = []; limit = None; offset = None }
        | Compound c ->
            Compound { c with order_by = []; limit = None; offset = None }
      in
      str o "SELECT COUNT(*) FROM (";
      render_body o (strip_tail body);
      str o ") AS ";
      name o "_qb_count"

let count_sql ?(dialect = Postgres) q =
  let o = out dialect in
  render_count o q;
  Buffer.contents o.buf

(* ---------- multi-table writes ---------- *)

(* INSERT INTO target (cols) SELECT ... *)

let render_insert_select o target target_sel (Query (_, _, body, _)) =
  base "Qb.Query.insert_select" target;
  check_nonempty "Qb.Query.insert_select" target_sel;
  match decoder target_sel with
  | Dec (_, _, cols) ->
      str o "INSERT INTO ";
      str o (table_name o.dialect target);
      str o " (";
      col_names o cols;
      str o ") ";
      render_body o body

let insert_select_sql ?(dialect = Postgres) target target_sel q =
  let o = out dialect in
  render_insert_select o target target_sel q;
  Buffer.contents o.buf

(* UPDATE target AS t0 SET ... [FROM ...] WHERE ... *)

type 'r update_plan = {
  upd_target : 'r table;
  upd_id : int;
  upd_assigns : (string * (out -> unit)) list;
  upd_from : source option;
  upd_joins : join_item list;
  upd_wheres : bool Expr.t list;
  upd_all_rows : bool;
}

type 'r update_assign = Upd_set : ('r, 'a) col * 'a Expr.t -> 'r update_assign

let set c e = Upd_set (c, e)
let set_val c v = Upd_set (c, Expr.lit c.ty v)

let dedup_upd assigns =
  let last = Hashtbl.create 8 in
  List.iter (fun (Upd_set (c, _) as a) -> Hashtbl.replace last c.name a) assigns;
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun (Upd_set (c, _)) ->
      if Hashtbl.mem seen c.name then None
      else (
        Hashtbl.add seen c.name ();
        Hashtbl.find_opt last c.name))
    assigns

let update_from ?(all_rows = false) target fn =
  base "Qb.Query.update_from" target;
  let id = fresh_id () in
  let target_ref = { rid = id; rtable = Some target } in
  let st, assigns = fn target_ref empty_state in
  let assigns = dedup_upd assigns in
  if assigns = [] then invalid_arg "Qb.Query.update_from: no assignments";
  if st.st_wheres = [] && not all_rows then
    invalid_arg
      "Qb.Query.update_from: missing where (pass ~all_rows:true to update \
       every row)";
  {
    upd_target = target;
    upd_id = id;
    upd_assigns =
      List.map
        (fun (Upd_set (c, e)) -> (c.name, fun o -> Expr.render o e))
        assigns;
    upd_from = st.st_from;
    upd_joins = List.rev st.st_joins;
    upd_wheres = List.rev st.st_wheres;
    upd_all_rows = all_rows;
  }

let render_update_from o u =
  ignore (register_ref o u.upd_id);
  Option.iter (register_source o) u.upd_from;
  List.iter (fun j -> register_source o j.source) u.upd_joins;
  str o "UPDATE ";
  str o (table_name o.dialect u.upd_target);
  str o " AS ";
  name o (lookup_ref o u.upd_id);
  str o " SET ";
  sep o ", "
    (fun (cname, render_rhs) ->
      name o cname;
      str o " = ";
      render_rhs o)
    u.upd_assigns;
  (match u.upd_from with
  | None -> ()
  | Some src ->
      str o " FROM ";
      render_source o src;
      List.iter
        (fun j ->
          str o (join_keyword j.kind);
          render_source o j.source;
          match j.on with
          | None -> ()
          | Some cond ->
              str o " ON ";
              Expr.render o cond)
        u.upd_joins);
  if u.upd_wheres <> [] then (
    str o " WHERE ";
    render_conjunction o u.upd_wheres)

let update_from_sql ?(dialect = Postgres) u =
  let o = out dialect in
  render_update_from o u;
  Buffer.contents o.buf

let render_returning_sel o id sel =
  check_nonempty "Qb.Query: returning" sel;
  match decoder sel with
  | Dec (_, _, cols) -> (
      str o " RETURNING ";
      match o.dialect with
      | Sqlite -> col_names o cols
      | Postgres | Mysql ->
          sep o ", " (fun (Any c) -> alias_colname o (lookup_ref o id) c) cols)

let update_from_returning_sql ?(dialect = Postgres) u returning =
  let o = out dialect in
  render_update_from o u;
  render_returning_sel o u.upd_id returning;
  Buffer.contents o.buf

(* DELETE FROM target AS t0 [USING ...] WHERE ... *)

type 'r delete_plan = {
  del_target : 'r table;
  del_id : int;
  del_using : source option;
  del_joins : join_item list;
  del_wheres : bool Expr.t list;
}

let delete_using ?(all_rows = false) target fn =
  base "Qb.Query.delete_using" target;
  let id = fresh_id () in
  let target_ref = { rid = id; rtable = Some target } in
  let st, () = fn target_ref empty_state in
  if st.st_wheres = [] && not all_rows then
    invalid_arg
      "Qb.Query.delete_using: missing where (pass ~all_rows:true to delete \
       every row)";
  {
    del_target = target;
    del_id = id;
    del_using = st.st_from;
    del_joins = List.rev st.st_joins;
    del_wheres = List.rev st.st_wheres;
  }

let render_delete_using o d =
  ignore (register_ref o d.del_id);
  Option.iter (register_source o) d.del_using;
  List.iter (fun j -> register_source o j.source) d.del_joins;
  str o "DELETE FROM ";
  str o (table_name o.dialect d.del_target);
  str o " AS ";
  name o (lookup_ref o d.del_id);
  (match d.del_using with
  | None -> ()
  | Some src ->
      str o " USING ";
      render_source o src;
      List.iter
        (fun j ->
          str o (join_keyword j.kind);
          render_source o j.source;
          match j.on with
          | None -> ()
          | Some cond ->
              str o " ON ";
              Expr.render o cond)
        d.del_joins);
  if d.del_wheres <> [] then (
    str o " WHERE ";
    render_conjunction o d.del_wheres)

let delete_using_sql ?(dialect = Postgres) d =
  let o = out dialect in
  render_delete_using o d;
  Buffer.contents o.buf

let delete_using_returning_sql ?(dialect = Postgres) d returning =
  let o = out dialect in
  render_delete_using o d;
  render_returning_sel o d.del_id returning;
  Buffer.contents o.buf
