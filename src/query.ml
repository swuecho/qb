(* Conditions, assignments, joins, queries, and SQL rendering. *)

open Ty
open Table

(* ---------- conditions: column, operator, plain OCaml value ---------- *)

type op = Eq | Ne | Gt | Ge | Lt | Le

let op_sql = function
  | Eq -> "="
  | Ne -> "<>"
  | Gt -> ">"
  | Ge -> ">="
  | Lt -> "<"
  | Le -> "<="

type 'r cond =
  | True : 'r cond
  | Cmp : ('r, 'a) col * op * 'a -> 'r cond
  | Cols : ('r, 'a) col * op * ('r, 'a) col -> 'r cond
  | In : ('r, 'a) col * 'a list -> 'r cond
  (* The pattern is a string, or [Some] of one for a nullable column. *)
  | Like : ('r, 'a) col * 'a * bool -> 'r cond (* true: ILIKE *)
  | And : 'r cond * 'r cond -> 'r cond
  | Or : 'r cond * 'r cond -> 'r cond
  | Not : 'r cond -> 'r cond

let all_rows = True

type 'r assign =
  | Set : ('r, 'a) col * 'a -> 'r assign
  | Set_expr : ('r, 'a) col * 'a Expr.t -> 'r assign

let set c v = Set (c, v)
let set_expr c e = Set_expr (c, e)
let assign_col = function Set (c, _) -> Any c | Set_expr (c, _) -> Any c

let assign_col_name = function
  | Set (c, _) -> c.name
  | Set_expr (c, _) -> c.name

let render_assign_value o = function
  | Set (c, v) -> param o c v
  | Set_expr (_, e) -> Expr.render o e

(* Keep the last assignment for each column, in first-seen column order, so
   overriding defaults (e.g. [base @ [ col := v ]]) works consistently across
   insert, upsert, and update. *)
let dedup_assigns assigns =
  let last = Hashtbl.create 8 in
  List.iter (fun a -> Hashtbl.replace last (assign_col_name a) a) assigns;
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun a ->
      let cname = assign_col_name a in
      if Hashtbl.mem seen cname then None
      else (
        Hashtbl.add seen cname ();
        Hashtbl.find_opt last cname))
    assigns

let dedup_cols cols =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun (Any c) ->
      if Hashtbl.mem seen c.name then false
      else (
        Hashtbl.add seen c.name ();
        true))
    cols

(* A new row: some columns of a table, the rest left to the database. *)
type 'r values = { vtable : 'r table; vassigns : 'r assign list }

let values vtable vassigns = { vtable; vassigns = dedup_assigns vassigns }

(* What to do when an insert hits a unique or primary-key conflict. *)
type 'r on_conflict =
  | Do_nothing of 'r any_col list
  | Do_update of {
      target : 'r any_col list;
      excluded : 'r any_col list;
      set : 'r assign list;
      where : 'r cond option;
    }

module On_conflict = struct
  type 'r column = 'r any_col

  let col c = Any c
  let do_nothing ?(target = []) () = Do_nothing (dedup_cols target)

  let do_update ~target ?(set = []) ?(excluded = []) ?where () =
    let target = dedup_cols target in
    let set = dedup_assigns set in
    let set_names = List.map assign_col_name set in
    let excluded =
      dedup_cols excluded
      |> List.filter (fun (Any c) -> not (List.mem c.name set_names))
    in
    if target = [] then
      invalid_arg "Qb.On_conflict.do_update: target must not be empty";
    if set = [] && excluded = [] then
      invalid_arg
        "Qb.On_conflict.do_update: give at least one column in ~set or \
         ~excluded";
    Do_update { target; excluded; set; where }
end

module Ops = struct
  let ( = ) c v = Cmp (c, Eq, v)
  let ( <> ) c v = Cmp (c, Ne, v)
  let ( > ) c v = Cmp (c, Gt, v)
  let ( >= ) c v = Cmp (c, Ge, v)
  let ( < ) c v = Cmp (c, Lt, v)
  let ( <= ) c v = Cmp (c, Le, v)
  let ( === ) a b = Cols (a, Eq, b)
  let ( == ) a b = Cols (fst a, Eq, snd b)
  let ( && ) a b = And (a, b)
  let ( || ) a b = Or (a, b)
  let not a = Not a

  let all_of = function
    | [] -> None
    | c :: cs -> Some (List.fold_left ( && ) c cs)

  let any_of = function
    | [] -> None
    | c :: cs -> Some (List.fold_left ( || ) c cs)

  let is_in c vs = In (c, vs)
  let not_in c vs = Not (In (c, vs))
  let between c lo hi = And (Cmp (c, Ge, lo), Cmp (c, Le, hi))
  let like c pattern = Like (c, pattern, false)
  let ilike c pattern = Like (c, pattern, true)
  let like_opt c pattern = Like (c, Some pattern, false)
  let ilike_opt c pattern = Like (c, Some pattern, true)
  let is_null c = Cmp (c, Eq, None)
  let is_not_null c = Cmp (c, Ne, None)

  (* A nullable column against a plain value. *)
  let ( =? ) c v = Cmp (c, Eq, Some v)
  let ( <>? ) c v = Cmp (c, Ne, Some v)
  let ( >? ) c v = Cmp (c, Gt, Some v)
  let ( >=? ) c v = Cmp (c, Ge, Some v)
  let ( <? ) c v = Cmp (c, Lt, Some v)
  let ( <=? ) c v = Cmp (c, Le, Some v)
  let ( := ) c v = Set (c, v)
end

let rec render_cond : type r. out -> r cond -> unit =
 fun o -> function
  | True -> str o "1 = 1"
  (* [email = None] must be IS NULL: in SQL, [= NULL] is never true. *)
  | Cmp (c, Eq, v) when c.ty.is_null v ->
      colname o c;
      str o " IS NULL"
  | Cmp (c, Ne, v) when c.ty.is_null v ->
      colname o c;
      str o " IS NOT NULL"
  | Cmp (c, op, v) ->
      colname o c;
      str o (" " ^ op_sql op ^ " ");
      param o c v
  | Cols (a, op, b) ->
      colname o a;
      str o (" " ^ op_sql op ^ " ");
      colname o b
  | In (_, []) -> str o "1 = 0"
  | In (c, vs) ->
      o.variadic <- true;
      colname o c;
      str o " IN (";
      sep o ", " (param o c) vs;
      str o ")"
  | Like (c, p, false) ->
      colname o c;
      str o " LIKE ";
      param o c p
  | Like (c, p, true) when o.dialect = Postgres ->
      colname o c;
      str o " ILIKE ";
      param o c p
  (* Only Postgres has ILIKE. *)
  | Like (c, p, true) ->
      str o "LOWER(";
      colname o c;
      str o ") LIKE LOWER(";
      param o c p;
      str o ")"
  | And (a, b) -> binary o "AND" a b
  | Or (a, b) -> binary o "OR" a b
  | Not a ->
      str o "NOT (";
      render_cond o a;
      str o ")"

and binary : type r. out -> string -> r cond -> r cond -> unit =
 fun o op a b ->
  str o "(";
  render_cond o a;
  str o (" " ^ op ^ " ");
  render_cond o b;
  str o ")"

let render_where o = function
  | None | Some True -> ()
  | Some w ->
      str o " WHERE ";
      render_cond o w

(* ---------- joins ---------- *)

let rec lift_cond : type r s. (r, s) lift -> r cond -> s cond =
 fun l -> function
  | True -> True
  | Cmp (c, op, v) -> Cmp (l.lift c, op, v)
  | Cols (a, op, b) -> Cols (l.lift a, op, l.lift b)
  | In (c, vs) -> In (l.lift c, vs)
  | Like (c, p, ci) -> Like (l.lift c, p, ci)
  | And (a, b) -> And (lift_cond l a, lift_cond l b)
  | Or (a, b) -> Or (lift_cond l a, lift_cond l b)
  | Not a -> Not (lift_cond l a)

let snd_cond c = lift_cond (lift R Stdlib.snd) c
let fst_row t = lift_sel (lift L Stdlib.fst) t.row
let snd_row t = lift_sel (lift R Stdlib.snd) t.row

let render_from : type r. out -> r table -> side list -> unit =
 fun o t path ->
  match t.joined with
  | Some f -> f o path
  | None ->
      str o (table_name o.dialect t);
      if path <> [] then (
        str o " AS ";
        name o (alias path))

let make_join keyword left right ~on row =
  let render o path =
    render_from o left (path @ [ L ]);
    str o keyword;
    (* [a JOIN (b JOIN c ON ...) ON ...] *)
    let nested = right.joined <> None in
    if nested then str o "(";
    render_from o right (path @ [ R ]);
    if nested then str o ")";
    str o " ON ";
    let outer = o.prefix in
    o.prefix <- path;
    render_cond o on;
    o.prefix <- outer
  in
  match decoder row with
  | Dec (_, _, cols) ->
      let name = left.tname ^ "_" ^ right.tname in
      { tschema = None; tname = name; row; cols; joined = Some render }

(* ---------- queries ---------- *)

type filtered
type unfiltered
type plain
type joined
type nulls = Nulls_default | Nulls_first | Nulls_last
type 'r order = Order : ('r, _) col * string * nulls -> 'r order

(* What a query returns. [Plain]: a selection of its table's columns.
   [Joined]: a selection of the left table's columns paired with the right
   table's rows ('c is the right row, or an option of it for a left join).

   The join lives here, not in the query's row type: a joined query is still
   a query on its left table, so [where] and [order_by] take that table's
   own columns before and after the join alike. Only when SQL is made
   ([flatten]) does it become a query on the pair, with every column lifted
   onto its side. *)
type ('r, 'a) output =
  | Plain : ('r, 'a) sel -> ('r, 'a) output
  | Joined : {
      keyword : string;
      left : ('r, 's) sel;
      right : 'b table;
      on : ('r * 'b) cond;
      right_sel : ('r * 'b, 'c) sel;
    }
      -> ('r, 's * 'c) output

(* ['r] is the (left) table's row, ['a] is what this query returns, ['w]
   records whether [where] was called. *)
type ('r, 'a, 'w) query = {
  table : 'r table;
  out : ('r, 'a) output;
  distinct : bool;
  where : 'r cond option;
  order : 'r order list;
  limit : int option;
  offset : int option;
}

let from table =
  {
    table;
    out = Plain table.row;
    distinct = false;
    where = None;
    order = [];
    limit = None;
    offset = None;
  }

let distinct q = { q with distinct = true }

let add_where cond q =
  let where =
    match (q.where, cond) with
    | None, c | Some True, c | Some c, True -> c
    | Some w, c -> And (w, c)
  in
  { q with where = Some where }

let where cond q = add_where cond q
let where_opt cond q = match cond with None -> q | Some c -> add_where c q

let select : type r a b w. (r, b) sel -> (r, a, w) query -> (r, b, w) query =
 fun sel q ->
  check_nonempty "Qb.select" sel;
  match q.out with
  | Joined _ ->
      invalid_arg
        "Qb.select: select before the join; after it, the right table would be \
         lost"
  | Plain _ -> { q with out = Plain sel }

let asc c = Order (c, "ASC", Nulls_default)
let desc c = Order (c, "DESC", Nulls_default)
let nulls_first (Order (c, dir, _)) = Order (c, dir, Nulls_first)
let nulls_last (Order (c, dir, _)) = Order (c, dir, Nulls_last)
let order_by order q = { q with order }

let limit n q =
  if n < 0 then invalid_arg "Qb.limit: negative limit";
  { q with limit = Some n }

let offset n q =
  if n < 0 then invalid_arg "Qb.offset: negative offset";
  { q with offset = Some n }

(* A join is recorded in the query's output; see [output]. The right
   table's own condition goes into the ON, which for a left join is also what
   keeps the unmatched left rows. *)
let join_with : type r s b c w v.
    string ->
    b table ->
    ?where:b cond ->
    on:(r * b) cond ->
    (r * b, c) sel ->
    (r, s, w) query ->
    (r, s * c, v) query =
 fun keyword right ?where ~on right_sel q ->
  match q.out with
  | Joined _ ->
      invalid_arg
        "Qb: one join per query; for three tables or more, write the SQL"
  | Plain left ->
      let on = match where with None -> on | Some w -> And (on, snd_cond w) in
      { q with out = Joined { keyword; left; right; on; right_sel } }

let join ?where right ~on q =
  join_with " JOIN " right ?where ~on (snd_row right) q

let left_join ?where right ~on q =
  join_with " LEFT JOIN " right ?where ~on (Sel.Expr (Opt (snd_row right))) q

let lift_order l (Order (c, dir, nulls)) = Order (l.lift c, dir, nulls)

(* The query as SQL sees it: a joined query becomes a plain query on the
   pair, its where, order and selection lifted onto the left side. *)
type 'a flat = Flat : ('r, 'a, 'w) query -> 'a flat

let flatten : type r a w. (r, a, w) query -> a flat =
 fun q ->
  match q.out with
  | Plain _ -> Flat q
  | Joined { keyword; left; right; on; right_sel } ->
      let l = lift L Stdlib.fst in
      let table =
        make_join keyword q.table right ~on
          (Sel.Expr (Both (fst_row q.table, snd_row right)))
      in
      Flat
        {
          table;
          out = Plain (Sel.Expr (Both (lift_sel l left, right_sel)));
          distinct = q.distinct;
          where = Option.map (lift_cond l) q.where;
          order = List.map (lift_order l) q.order;
          limit = q.limit;
          offset = q.offset;
        }

let not_joined : type r a w. string -> (r, a, w) query -> unit =
 fun fn q ->
  match q.out with
  | Joined _ -> invalid_arg (fn ^ ": a join is not a table")
  | Plain _ -> ()

let render_order o (Order (c, dir, nulls)) =
  match (nulls, o.dialect) with
  | Nulls_default, _ ->
      colname o c;
      str o (" " ^ dir)
  | Nulls_first, (Postgres | Sqlite) ->
      colname o c;
      str o (" " ^ dir ^ " NULLS FIRST")
  | Nulls_last, (Postgres | Sqlite) ->
      colname o c;
      str o (" " ^ dir ^ " NULLS LAST")
  (* MySQL has no NULLS FIRST / NULLS LAST; sort by IS NULL first. *)
  | Nulls_first, Mysql ->
      colname o c;
      str o " IS NULL DESC, ";
      colname o c;
      str o (" " ^ dir)
  | Nulls_last, Mysql ->
      colname o c;
      str o " IS NULL ASC, ";
      colname o c;
      str o (" " ^ dir)

let render_select o q cols =
  str o (if q.distinct then "SELECT DISTINCT " else "SELECT ");
  qualified_names o cols;
  str o " FROM ";
  render_from o q.table [];
  render_where o q.where;
  if q.order <> [] then (
    str o " ORDER BY ";
    sep o ", " (render_order o) q.order);
  match (q.limit, q.offset, o.dialect) with
  | Some n, Some m, _ -> str o (Printf.sprintf " LIMIT %d OFFSET %d" n m)
  | Some n, None, _ -> str o (Printf.sprintf " LIMIT %d" n)
  | None, Some m, Postgres -> str o (Printf.sprintf " OFFSET %d" m)
  (* SQLite and MySQL require LIMIT whenever OFFSET is given. *)
  | None, Some m, Sqlite -> str o (Printf.sprintf " LIMIT -1 OFFSET %d" m)
  | None, Some m, Mysql ->
      str o (Printf.sprintf " LIMIT 18446744073709551615 OFFSET %d" m)
  | None, None, _ -> ()

(* A SELECT ready to render: Caqti's row type, how to build the result from
   it, and the renderer. *)
type 'a prepared =
  | Prepared : 'x Caqti_type.t * ('x -> 'a) * (out -> unit) -> 'a prepared

let prepare : type r a w. ?limit:int -> (r, a, w) query -> a prepared =
 fun ?limit q ->
  match flatten q with
  | Flat q -> (
      let q =
        match (limit, q.limit) with
        | None, _ -> q
        | Some n, None -> { q with limit = Some n }
        | Some n, Some m -> { q with limit = Some (min n m) }
      in
      match q.out with
      | Joined _ -> assert false (* flatten leaves none *)
      | Plain sel -> (
          match decoder sel with
          | Dec (rt, decode, cols) ->
              Prepared (rt, decode, fun o -> render_select o q cols)))

let to_sql ?(dialect = Postgres) q =
  let o = out dialect in
  (match prepare q with Prepared (_, _, render) -> render o);
  Buffer.contents o.buf

let render_count o q =
  match flatten q with
  | Flat q ->
      str o "SELECT COUNT(*) FROM ";
      render_from o q.table [];
      render_where o q.where

let render_insert o t cols values =
  base "Qb: insert" t;
  str o "INSERT INTO ";
  str o (table_name o.dialect t);
  match (cols, o.dialect) with
  | [], Mysql -> str o " () VALUES ()"
  | [], (Postgres | Sqlite) -> str o " DEFAULT VALUES"
  | _ ->
      str o " (";
      col_names o cols;
      str o ") VALUES (";
      sep o ", " (fun f -> f ()) values;
      str o ")"

let render_insert_many o t rows =
  base "Qb: insert_many" t;
  o.variadic <- true;
  str o "INSERT INTO ";
  str o (table_name o.dialect t);
  str o " (";
  col_names o t.cols;
  str o ") VALUES ";
  sep o ", "
    (fun row ->
      str o "(";
      sep o ", " (fun (Any c) -> param o c (c.get row)) t.cols;
      str o ")")
    rows

let render_insert_many_values o vs =
  match vs with
  | [] -> invalid_arg "Qb.insert_many_values: no rows"
  | first :: rest -> (
      let t = first.vtable in
      base "Qb: insert_many_values" t;
      List.iter
        (fun v ->
          if v.vtable.tname <> t.tname || v.vtable.tschema <> t.tschema then
            invalid_arg
              "Qb.insert_many_values: all rows must be for the same table")
        rest;
      o.variadic <- true;
      let col_names_in_order =
        let seen = Hashtbl.create 8 in
        let acc = ref [] in
        List.iter
          (fun v ->
            List.iter
              (fun a ->
                let cname = assign_col_name a in
                if not (Hashtbl.mem seen cname) then (
                  Hashtbl.add seen cname ();
                  acc := cname :: !acc))
              v.vassigns)
          vs;
        List.rev !acc
      in
      match (col_names_in_order, vs) with
      | [], [ _ ] -> render_insert o t [] []
      | [], _ -> (
          match (o.dialect, t.cols) with
          | Sqlite, _ ->
              invalid_arg
                "Qb.insert_many_values: SQLite can't insert multiple \
                 all-default rows in one statement"
          | _, [] -> invalid_arg "Qb.insert_many_values: table has no columns"
          | (Postgres | Mysql), Any first_col :: _ ->
              str o "INSERT INTO ";
              str o (table_name o.dialect t);
              str o " (";
              name o first_col.name;
              str o ") VALUES ";
              sep o ", " (fun _ -> str o "(DEFAULT)") vs)
      | cols, _ ->
          str o "INSERT INTO ";
          str o (table_name o.dialect t);
          str o " (";
          sep o ", " (name o) cols;
          str o ") VALUES ";
          sep o ", "
            (fun v ->
              let lookup cname =
                List.fold_left
                  (fun acc a ->
                    if assign_col_name a = cname then
                      Some (fun () -> render_assign_value o a)
                    else acc)
                  None v.vassigns
              in
              str o "(";
              sep o ", "
                (fun cname ->
                  match lookup cname with
                  | Some emit -> emit ()
                  | None ->
                      if o.dialect = Sqlite then
                        invalid_arg
                          "Qb.insert_many_values: on SQLite every row must set \
                           the same columns"
                      else str o "DEFAULT")
                cols;
              str o ")")
            vs)

let render_returning o sel =
  match decoder sel with
  | Dec (_, _, cols) ->
      str o " RETURNING ";
      col_names o cols

let render_on_conflict o t = function
  | Do_nothing [] -> str o " ON CONFLICT DO NOTHING"
  | Do_nothing target ->
      str o " ON CONFLICT (";
      col_names o target;
      str o ") DO NOTHING"
  | Do_update { target; excluded; set; where } ->
      str o " ON CONFLICT (";
      col_names o target;
      str o ") DO UPDATE SET ";
      let items =
        List.map
          (fun (Any c) () ->
            name o c.name;
            str o " = EXCLUDED.";
            name o c.name)
          excluded
        @ List.map
            (fun a () ->
              let cname = assign_col_name a in
              name o cname;
              str o " = ";
              let prev = o.table_qual in
              o.table_qual <- Some t.tname;
              Fun.protect
                ~finally:(fun () -> o.table_qual <- prev)
                (fun () -> render_assign_value o a))
            set
      in
      sep o ", " (fun f -> f ()) items;
      let prev = o.table_qual in
      o.table_qual <- Some t.tname;
      Fun.protect
        ~finally:(fun () -> o.table_qual <- prev)
        (fun () -> render_where o where)

let render_update o assigns q =
  not_joined "Qb: update" q;
  base "Qb: update" q.table;
  let assigns = dedup_assigns assigns in
  str o "UPDATE ";
  str o (table_name o.dialect q.table);
  str o " SET ";
  sep o ", "
    (fun a ->
      name o (assign_col_name a);
      str o " = ";
      render_assign_value o a)
    assigns;
  render_where o q.where

let render_delete o q =
  not_joined "Qb: delete" q;
  base "Qb: delete" q.table;
  str o "DELETE FROM ";
  str o (table_name o.dialect q.table);
  render_where o q.where

(* The text of each statement {!Runner.Make} runs, without running it. *)
let sql_of dialect f =
  let o = out dialect in
  f o;
  Buffer.contents o.buf

let count_sql ?(dialect = Postgres) q =
  sql_of dialect (fun o -> render_count o q)

let insert_sql ?(dialect = Postgres) t row =
  sql_of dialect (fun o ->
      render_insert o t t.cols
        (List.map (fun (Any c) () -> param o c (c.get row)) t.cols))

let insert_returning_sql ?(dialect = Postgres) t assigns returning =
  check_nonempty "Qb.insert_returning_sql" returning;
  let assigns = dedup_assigns assigns in
  match decoder returning with
  | Dec (_, _, cols) ->
      sql_of dialect (fun o ->
          render_insert o t
            (List.map assign_col assigns)
            (List.map (fun a () -> render_assign_value o a) assigns);
          str o " RETURNING ";
          col_names o cols)

let insert_values_sql ?dialect v =
  insert_returning_sql ?dialect v.vtable v.vassigns v.vtable.row

let insert_many_values_sql ?(dialect = Postgres) vs =
  match vs with
  | [] -> invalid_arg "Qb.insert_many_values_sql: no rows"
  | first :: _ ->
      sql_of dialect (fun o ->
          render_insert_many_values o vs;
          render_returning o first.vtable.row)

let upsert_sql ?(dialect = Postgres) conflict v =
  sql_of dialect (fun o ->
      render_insert o v.vtable
        (List.map assign_col v.vassigns)
        (List.map (fun a () -> render_assign_value o a) v.vassigns);
      render_on_conflict o v.vtable conflict;
      render_returning o v.vtable.row)

let upsert_many_sql ?(dialect = Postgres) conflict vs =
  match vs with
  | [] -> invalid_arg "Qb.upsert_many_sql: no rows"
  | first :: _ ->
      sql_of dialect (fun o ->
          render_insert_many_values o vs;
          render_on_conflict o first.vtable conflict;
          render_returning o first.vtable.row)

let insert_many_sql ?(dialect = Postgres) t rows =
  if rows = [] then invalid_arg "Qb.insert_many_sql: no rows";
  sql_of dialect (fun o -> render_insert_many o t rows)

let update_sql ?(dialect = Postgres) assigns q =
  if assigns = [] then invalid_arg "Qb.update_sql: no assignments";
  sql_of dialect (fun o -> render_update o assigns q)

let delete_sql ?(dialect = Postgres) q =
  sql_of dialect (fun o -> render_delete o q)

let update_returning_sql ?(dialect = Postgres) assigns q returning =
  if assigns = [] then invalid_arg "Qb.update_returning_sql: no assignments";
  check_nonempty "Qb.update_returning_sql" returning;
  sql_of dialect (fun o ->
      render_update o assigns q;
      render_returning o returning)

let delete_returning_sql ?(dialect = Postgres) q returning =
  check_nonempty "Qb.delete_returning_sql" returning;
  sql_of dialect (fun o ->
      render_delete o q;
      render_returning o returning)
