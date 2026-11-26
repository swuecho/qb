(* Qb: a Drizzle-style query builder for single-table CRUD on Caqti.
   See qb.mli for the interface and documentation. *)

type dialect = Postgres | Sqlite | Mysql

(* ---------- column types ---------- *)

(* [parse] and [show] turn a value into text and back, for values that come
   from a form (see Changeset); [blank] says a value counts as missing. *)
type 'a ty = {
  caqti : 'a Caqti_type.t;
  ddl : dialect -> string;
  nullable : bool;
  is_null : 'a -> bool;
  parse : (string -> 'a option) option;
  show : ('a -> string) option;
  blank : 'a -> bool;
}

(* ---------- values as text ---------- *)

let is_digit c = c >= '0' && c <= '9'

(* Decimal only: int_of_string would also take "0x1f", "0b1" and "1_000". *)
let parse_integer of_string s =
  let s = String.trim s in
  let n = String.length s in
  let start = if n > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0 in
  if start = n then None
  else if String.for_all is_digit (String.sub s start (n - start)) then
    of_string s
  else None

(* Decimal only: float_of_string would also take "nan", "inf" and hex. *)
let parse_float s =
  let s = String.trim s in
  if
    String.exists is_digit s
    && String.for_all (fun c -> is_digit c || String.contains "+-.eE" c) s
  then float_of_string_opt s
  else None

(* The shortest text that reads back as the same float. *)
let show_float f =
  let rec go p =
    let s = Printf.sprintf "%.*g" p f in
    if p >= 17 || float_of_string s = f then s else go (p + 1)
  in
  go 1

let parse_bool s =
  match String.lowercase_ascii (String.trim s) with
  | "true" | "1" | "on" | "yes" -> Some true
  | "false" | "0" | "off" | "no" -> Some false
  | _ -> None

(* RFC 3339, or what an HTML datetime-local input sends ("2026-11-22T10:30",
   no zone), read as UTC. *)
let parse_ptime s =
  let s = String.trim s in
  let rfc s =
    match Ptime.of_rfc3339 ~strict:false s with
    | Ok (t, _, _) -> Some t
    | Error _ -> None
  in
  match rfc s with
  | Some t -> Some t
  | None -> (
      match String.length s with
      | 16 -> rfc (s ^ ":00Z")
      | 19 -> rfc (s ^ "Z")
      | _ -> None)

let show_ptime t =
  let frac_s = if Ptime.(Span.equal (frac_s t) Span.zero) then 0 else 6 in
  Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s t

let parse_date s =
  match
    Scanf.sscanf (String.trim s) "%4d-%2d-%2d%!" (fun y m d -> (y, m, d))
  with
  | ymd -> Ptime.of_date ymd
  | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) -> None

let show_date t =
  let y, m, d = Ptime.to_date t in
  Printf.sprintf "%04d-%02d-%02d" y m d

let parse_uuid s =
  let s = String.lowercase_ascii (String.trim s) in
  let hex c = is_digit c || (c >= 'a' && c <= 'f') in
  if
    String.length s = 36
    && List.for_all
         (fun i ->
           match i with 8 | 13 | 18 | 23 -> s.[i] = '-' | _ -> hex s.[i])
         (List.init 36 Fun.id)
  then Some s
  else None

(* How to read and show a value of a Caqti type qb knows, found by asking
   Caqti whether the type is one of them. *)
let text_form : type a.
    a Caqti_type.t -> (string -> a option) option * (a -> string) option =
 fun t ->
  let module T = Caqti_type in
  let is u = T.unify u t in
  match is T.string with
  | Some T.Equal -> (Some Option.some, Some Fun.id)
  | None -> (
      match is T.int with
      | Some T.Equal ->
          (Some (parse_integer int_of_string_opt), Some string_of_int)
      | None -> (
          match is T.int64 with
          | Some T.Equal ->
              (Some (parse_integer Int64.of_string_opt), Some Int64.to_string)
          | None -> (
              match is T.int32 with
              | Some T.Equal ->
                  ( Some (parse_integer Int32.of_string_opt),
                    Some Int32.to_string )
              | None -> (
                  match is T.float with
                  | Some T.Equal -> (Some parse_float, Some show_float)
                  | None -> (
                      match is T.bool with
                      | Some T.Equal -> (Some parse_bool, Some string_of_bool)
                      | None -> (
                          match is T.ptime with
                          | Some T.Equal -> (Some parse_ptime, Some show_ptime)
                          | None -> (
                              match is T.pdate with
                              | Some T.Equal -> (Some parse_date, Some show_date)
                              | None -> (
                                  match is T.octets with
                                  | Some T.Equal ->
                                      (Some Option.some, Some Fun.id)
                                  | None -> (None, None)))))))))

(* Text that is only whitespace is blank. *)
let blank_of : type a. a Caqti_type.t -> a -> bool =
 fun t ->
  match Caqti_type.unify Caqti_type.string t with
  | Some Caqti_type.Equal -> fun s -> String.trim s = ""
  | None -> fun _ -> false

let ty ?sqlite ?mysql ?of_string ?to_string caqti ddl =
  let ddl = function
    | Postgres -> ddl
    | Sqlite -> Option.value sqlite ~default:ddl
    | Mysql -> Option.value mysql ~default:ddl
  in
  let parse, show = text_form caqti in
  let parse = match of_string with Some _ -> of_string | None -> parse in
  let show = match to_string with Some _ -> to_string | None -> show in
  {
    caqti;
    ddl;
    nullable = false;
    is_null = (fun _ -> false);
    parse;
    show;
    blank = blank_of caqti;
  }

(* OCaml ints are 63-bit and floats are double precision: Postgres INTEGER
   and REAL would overflow or round them. SQLite's INTEGER is already 64-bit,
   and only "INTEGER PRIMARY KEY" makes an auto-assigned rowid. *)
let int = ty Caqti_type.int "BIGINT" ~sqlite:"INTEGER"
let int64 = ty Caqti_type.int64 "BIGINT" ~sqlite:"INTEGER"

let float =
  ty Caqti_type.float "DOUBLE PRECISION" ~sqlite:"REAL" ~mysql:"DOUBLE"

let text = ty Caqti_type.string "TEXT"
let bool = ty Caqti_type.bool "BOOLEAN"
let octets = ty Caqti_type.octets "BYTEA" ~sqlite:"BLOB" ~mysql:"LONGBLOB"

let ptime =
  ty Caqti_type.ptime "TIMESTAMP WITH TIME ZONE" ~sqlite:"TEXT"
    ~mysql:"DATETIME(6)"

let date = ty Caqti_type.pdate "DATE" ~sqlite:"TEXT"

(* Caqti sends strings to Postgres untyped, so the server converts them to
   uuid / jsonb itself. *)
let uuid =
  ty Caqti_type.string "UUID" ~sqlite:"TEXT" ~mysql:"CHAR(36)"
    ~of_string:parse_uuid

let json = ty Caqti_type.string "JSONB" ~sqlite:"TEXT" ~mysql:"JSON"

(* Blank text is NULL: a form's empty field clears a nullable column. *)
let nullable t =
  {
    caqti = Caqti_type.option t.caqti;
    ddl = t.ddl;
    nullable = true;
    is_null = Option.is_none;
    parse =
      Option.map
        (fun p s ->
          if String.trim s = "" then Some None else Option.map Option.some (p s))
        t.parse;
    show = Option.map (fun f -> function None -> "" | Some v -> f v) t.show;
    blank = (function None -> true | Some v -> t.blank v);
  }

(* ---------- columns ---------- *)

(* Which side of a join, from the outermost join inwards. *)
type side = L | R

(* [get] reads the field back out of a row; insert uses it. [path] is empty
   for a table's own column, and says which table of a join it belongs to
   for a column lifted with [fst] / [snd]. *)
type ('r, 'a) col = {
  name : string;
  ty : 'a ty;
  get : 'r -> 'a;
  primary_key : bool;
  auto_increment : bool;
  path : side list;
}

let column ?(primary_key = false) ?(auto_increment = false) name ty get =
  { name; ty; get; primary_key; auto_increment; path = [] }

(* A column of one table of a join, moved onto the join's pair rows. *)
type ('r, 's) lift = { lift : 'a. ('r, 'a) col -> ('s, 'a) col }

let lift side get =
  {
    lift =
      (fun c ->
        { c with path = side :: c.path; get = (fun r -> c.get (get r)) });
  }

let fst c = (lift L Stdlib.fst).lift c
let snd c = (lift R Stdlib.snd).lift c

type 'r any_col = Any : ('r, _) col -> 'r any_col

(* Identifiers are always quoted, so names like "user" or "order" work. *)
let quote dialect name =
  match dialect with
  | Mysql -> "`" ^ String.concat "``" (String.split_on_char '`' name) ^ "`"
  | Postgres | Sqlite ->
      "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

(* ---------- rendering: SQL text + collected parameters ---------- *)

type param = Param : 'a Caqti_type.t * 'a -> param

type out = {
  dialect : dialect;
  buf : Buffer.t;
  mutable params : param list; (* reversed *)
  mutable prefix : side list;
      (* While rendering a nested join's ON: where that join sits. *)
  mutable variadic : bool;
      (* The text depends on how long a list is (IN, several rows): there is
         no end of such statements, so they aren't kept prepared. *)
}

let out dialect =
  {
    dialect;
    buf = Buffer.create 128;
    params = [];
    prefix = [];
    variadic = false;
  }

let str o s = Buffer.add_string o.buf s
let name o n = str o (quote o.dialect n)

(* Each table of a join is aliased by its path: "l", "r", "lr", ... *)
let alias path =
  String.concat "" (List.map (function L -> "l" | R -> "r") path)

(* A column, qualified by its table's alias when it comes from a join. *)
let colname o c =
  match o.prefix @ c.path with
  | [] -> name o c.name
  | p ->
      name o (alias p);
      str o ".";
      name o c.name

let param o (c : (_, 'a) col) (v : 'a) =
  o.params <- Param (c.ty.caqti, v) :: o.params;
  str o "?"

let sep o sep f xs =
  List.iteri
    (fun i x ->
      if i > 0 then str o sep;
      f x)
    xs

(* ---------- selection: an applicative over columns ---------- *)

(* A row read with list syntax: [select [ a; b ]] gives [Row.[ x; y ]]. *)
module Row = struct
  type _ t = [] : unit t | ( :: ) : 'a * 'b t -> ('a * 'b) t
end

(* [[]] and [::] live in [Sel], not at the top level, so [open Qb] does not
   shadow ordinary lists. OCaml still finds them from the expected type. *)
module Sel = struct
  type ('r, _) t =
    | [] : ('r, unit Row.t) t
    | ( :: ) : ('r, 'a) col * ('r, 'b Row.t) t -> ('r, ('a * 'b) Row.t) t
    | Expr : ('r, 'a) expr -> ('r, 'a) t

  and ('r, _) expr =
    | One : ('r, 'a) col -> ('r, 'a) expr
    | Map : ('a -> 'b) * ('r, 'a) t -> ('r, 'b) expr
    | Both : ('r, 'a) t * ('r, 'b) t -> ('r, 'a * 'b) expr
    | Opt : ('r, 'a) t -> ('r, 'a option) expr
  (* [None] when every column is NULL: the missing side of a left join. *)
end

type ('r, 'a) sel = ('r, 'a) Sel.t

let col c = Sel.Expr (One c)
let ( let+ ) s f = Sel.Expr (Map (f, s))
let ( and+ ) a b = Sel.Expr (Both (a, b))

(* The raw row Caqti decodes, how to turn it into ['a], and the columns. *)
type ('r, 'a) decoder =
  | Dec : 'x Caqti_type.t * ('x -> 'a) * 'r any_col list -> ('r, 'a) decoder

let rec decoder : type r a. (r, a) sel -> (r, a) decoder = function
  | [] -> Dec (Caqti_type.unit, (fun () -> Row.[]), List.[])
  | c :: rest -> (
      match decoder rest with
      | Dec (t, d, cs) ->
          Dec
            ( Caqti_type.t2 c.ty.caqti t,
              (fun (x, y) -> Row.(x :: d y)),
              List.cons (Any c) cs ))
  | Expr (One c) -> Dec (c.ty.caqti, Fun.id, List.[ Any c ])
  | Expr (Map (f, s)) -> (
      match decoder s with Dec (t, d, cs) -> Dec (t, (fun x -> f (d x)), cs))
  | Expr (Both (a, b)) -> (
      match (decoder a, decoder b) with
      | Dec (ta, da, ca), Dec (tb, db, cb) ->
          Dec (Caqti_type.t2 ta tb, (fun (x, y) -> (da x, db y)), ca @ cb))
  | Expr (Opt s) -> (
      match decoder s with
      | Dec (t, d, cs) -> Dec (Caqti_type.option t, Option.map d, cs))

(* [select []] would render [SELECT  FROM ...]. *)
let check_nonempty fn sel =
  match decoder sel with
  | Dec (_, _, List.[]) -> invalid_arg (fn ^ ": no columns")
  | Dec _ -> ()

(* ---------- tables ---------- *)

(* A join is a table too: its rows are pairs, its columns lifted ones, and
   it renders its FROM clause itself, given where it sits in an outer join. *)
type 'r table = {
  tschema : string option;
  tname : string;
  row : ('r, 'r) sel;
  cols : 'r any_col list;
  joined : (out -> side list -> unit) option;
}

let table ?schema tname row =
  match decoder row with
  | Dec (_, _, cols) -> { tschema = schema; tname; row; cols; joined = None }

(* "schema"."table", each part quoted on its own. *)
let table_name dialect t =
  match t.tschema with
  | None -> quote dialect t.tname
  | Some s -> quote dialect s ^ "." ^ quote dialect t.tname

let column_ddl dialect (Any c) =
  let auto =
    match (c.auto_increment, dialect) with
    | false, _ | true, Sqlite -> ""
    | true, Postgres -> " GENERATED BY DEFAULT AS IDENTITY"
    | true, Mysql -> " AUTO_INCREMENT"
  in
  Printf.sprintf "%s %s%s%s" (quote dialect c.name) (c.ty.ddl dialect) auto
    (if c.primary_key then " PRIMARY KEY"
     else if c.ty.nullable then ""
     else " NOT NULL")

let base fn t =
  if t.joined <> None then invalid_arg (fn ^ ": a join is not a table")

let create_table_sql ?(dialect = Postgres) t =
  base "Qb.create_table_sql" t;
  Printf.sprintf "CREATE TABLE %s (%s)" (table_name dialect t)
    (String.concat ", " (List.map (column_ddl dialect) t.cols))

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

type 'r assign = Set : ('r, 'a) col * 'a -> 'r assign

let set c v = Set (c, v)

(* A new row: some columns of a table, the rest left to the database. *)
type 'r values = { vtable : 'r table; vassigns : 'r assign list }

let values vtable vassigns = { vtable; vassigns }

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

let col_names o cols = sep o ", " (fun (Any c) -> name o c.name) cols
let qualified_names o cols = sep o ", " (fun (Any c) -> colname o c) cols

(* ---------- joins ---------- *)

let rec lift_sel : type r s a. (r, s) lift -> (r, a) sel -> (s, a) sel =
 fun l -> function
  | [] -> []
  | c :: rest -> l.lift c :: lift_sel l rest
  | Expr (One c) -> Expr (One (l.lift c))
  | Expr (Map (f, s)) -> Expr (Map (f, lift_sel l s))
  | Expr (Both (a, b)) -> Expr (Both (lift_sel l a, lift_sel l b))
  | Expr (Opt s) -> Expr (Opt (lift_sel l s))

let some c = { c with ty = nullable c.ty; get = (fun r -> Some (c.get r)) }

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
type 'r order = Order : ('r, _) col * string -> 'r order

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
  where : 'r cond option;
  order : 'r order list;
  limit : int option;
  offset : int option;
}

let from table =
  {
    table;
    out = Plain table.row;
    where = None;
    order = [];
    limit = None;
    offset = None;
  }

let where cond q =
  let where =
    match (q.where, cond) with
    | None, c | Some True, c | Some c, True -> c
    | Some w, c -> And (w, c)
  in
  { q with where = Some where }

let select : type r a b w. (r, b) sel -> (r, a, w) query -> (r, b, w) query =
 fun sel q ->
  check_nonempty "Qb.select" sel;
  match q.out with
  | Joined _ ->
      invalid_arg
        "Qb.select: select before the join; after it, the right table would be \
         lost"
  | Plain _ -> { q with out = Plain sel }

let asc c = Order (c, "ASC")
let desc c = Order (c, "DESC")
let order_by order q = { q with order }
let limit n q = { q with limit = Some n }
let offset n q = { q with offset = Some n }

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

let lift_order l (Order (c, dir)) = Order (l.lift c, dir)

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

let render_select o q cols =
  str o "SELECT ";
  qualified_names o cols;
  str o " FROM ";
  render_from o q.table [];
  render_where o q.where;
  if q.order <> [] then (
    str o " ORDER BY ";
    sep o ", "
      (fun (Order (c, dir)) ->
        colname o c;
        str o (" " ^ dir))
      q.order);
  Option.iter (fun n -> str o (Printf.sprintf " LIMIT %d" n)) q.limit;
  Option.iter (fun n -> str o (Printf.sprintf " OFFSET %d" n)) q.offset

(* A SELECT ready to render: Caqti's row type, how to build the result from
   it, and the renderer. *)
type 'a prepared =
  | Prepared : 'x Caqti_type.t * ('x -> 'a) * (out -> unit) -> 'a prepared

let prepare : type r a w. ?limit:int -> (r, a, w) query -> a prepared =
 fun ?limit q ->
  match flatten q with
  | Flat q -> (
      let q =
        match limit with None -> q | Some n -> { q with limit = Some n }
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

let render_returning o sel =
  match decoder sel with
  | Dec (_, _, cols) ->
      str o " RETURNING ";
      col_names o cols

let render_update o assigns q =
  not_joined "Qb: update" q;
  base "Qb: update" q.table;
  str o "UPDATE ";
  str o (table_name o.dialect q.table);
  str o " SET ";
  sep o ", "
    (fun (Set (c, v)) ->
      name o c.name;
      str o " = ";
      param o c v)
    assigns;
  render_where o q.where

let render_delete o q =
  not_joined "Qb: delete" q;
  base "Qb: delete" q.table;
  str o "DELETE FROM ";
  str o (table_name o.dialect q.table);
  render_where o q.where

(* The text of each statement {!Make} runs, without running it. *)
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
  match decoder returning with
  | Dec (_, _, cols) ->
      sql_of dialect (fun o ->
          render_insert o t
            (List.map (fun (Set (c, _)) -> Any c) assigns)
            (List.map (fun (Set (c, v)) () -> param o c v) assigns);
          str o " RETURNING ";
          col_names o cols)

let insert_values_sql ?dialect v =
  insert_returning_sql ?dialect v.vtable v.vassigns v.vtable.row

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

(* ---------- errors ---------- *)

type violation = [ `Unique | `Foreign_key | `Not_null | `Check | `Other ]

let violation = function
  | (`Request_failed _ | `Response_failed _) as e -> (
      match Caqti_error.cause e with
      | `Unique_violation -> Some `Unique
      | `Foreign_key_violation | `Restrict_violation -> Some `Foreign_key
      | `Not_null_violation -> Some `Not_null
      | `Check_violation -> Some `Check
      | #Caqti_error.integrity_constraint_violation -> Some `Other
      | _ -> None)
  | _ -> None

let is_unique_violation e = violation e = Some `Unique

(* ---------- changesets ---------- *)

module Changeset = struct
  type kind =
    [ `Required
    | `Cast
    | `Length
    | `Range
    | `Inclusion
    | `Exclusion
    | `Format
    | `Unique
    | `Foreign_key
    | `Check
    | `Custom ]

  type error = { field : string; message : string; kind : kind }

  (* A constraint the database may refuse a write with, and the error it
     becomes. [named]: the name was given, not the default one. *)
  type cons = {
    ckind : [ `Unique | `Foreign_key | `Check ];
    cname : string;
    named : bool;
    ccol : string;
    cmessage : string;
  }

  type 'r t = {
    table : 'r table;
    data : 'r option;
    changes : 'r assign list; (* in the order made *)
    errors : error list; (* in the order found *)
    params : (string * string) list;
    constraints : cons list;
  }

  type 'r field = Field : ('r, 'a) col -> 'r field

  let field c = Field c

  let make table data =
    { table; data; changes = []; errors = []; params = []; constraints = [] }

  let for_insert table = make table None
  let for_update table row = make table (Some row)
  let data cs = cs.data
  let changes cs = cs.changes
  let errors cs = cs.errors
  let is_valid cs = cs.errors = []
  let has_error name cs = List.exists (fun e -> e.field = name) cs.errors

  let errors_on c cs =
    List.filter_map
      (fun e -> if e.field = c.name then Some e.message else None)
      cs.errors

  let add_error ?(kind = `Custom) c message cs =
    { cs with errors = cs.errors @ [ { field = c.name; message; kind } ] }

  (* The change made to [c], found by name; Caqti says whether its type is
     [c]'s, so no cast is needed. *)
  let get_change : type r a. (r, a) col -> r t -> a option =
   fun c cs ->
    let same_col : type b. (r, b) col -> b -> a option =
     fun c' v ->
      if c'.name <> c.name then None
      else
        match Caqti_type.unify c'.ty.caqti c.ty.caqti with
        | Some Caqti_type.Equal -> Some v
        | None -> None
    in
    List.find_map (fun (Set (c', v)) -> same_col c' v) cs.changes

  let get c cs =
    match get_change c cs with
    | Some v -> Some v
    | None -> Option.map c.get cs.data

  let drop_change c cs =
    {
      cs with
      changes = List.filter (fun (Set (c', _)) -> c'.name <> c.name) cs.changes;
    }

  let same a b = try a = b with Invalid_argument _ -> false

  (* A change to the value a row already has is no change. *)
  let put c v cs =
    let cs = drop_change c cs in
    match cs.data with
    | Some row when same (c.get row) v -> cs
    | _ -> { cs with changes = cs.changes @ [ Set (c, v) ] }

  let cast params fields cs =
    let cs =
      {
        cs with
        params =
          params
          @ List.filter (fun (k, _) -> not (List.mem_assoc k params)) cs.params;
      }
    in
    List.fold_left
      (fun cs (Field c) ->
        match List.assoc_opt c.name params with
        | None -> cs
        | Some s -> (
            let parse =
              match c.ty.parse with
              | Some p -> p
              | None ->
                  invalid_arg
                    (Printf.sprintf
                       "Qb.Changeset.cast: column %s has no of_string (see \
                        Qb.ty)"
                       c.name)
            in
            let cs = drop_change c cs in
            if (not c.ty.nullable) && String.trim s = "" then
              add_error ~kind:`Required c "can't be blank" cs
            else
              match parse s with
              | Some v -> put c v cs
              | None -> add_error ~kind:`Cast c "is invalid" cs))
      cs fields

  let validate_required fields cs =
    List.fold_left
      (fun cs (Field c) ->
        if has_error c.name cs then cs
        else
          match get c cs with
          | Some v when not (c.ty.blank v) -> cs
          | _ -> add_error ~kind:`Required c "can't be blank" cs)
      cs fields

  type 'a check = show:('a -> string) -> 'a -> (kind * string) option

  let run_checks c show checks v cs =
    List.fold_left
      (fun cs (check : _ check) ->
        match check ~show v with
        | Some (kind, message) -> add_error ~kind c message cs
        | None -> cs)
      cs checks

  let show_of c = Option.value c.ty.show ~default:(fun _ -> "the value")

  (* Like Ecto, a check looks at the change only: what is already stored was
     checked when it was written. A field that failed to cast is left alone. *)
  let validate c checks cs =
    if has_error c.name cs then cs
    else
      match get_change c cs with
      | None -> cs
      | Some v -> run_checks c (show_of c) checks v cs

  let validate_opt c checks cs =
    if has_error c.name cs then cs
    else
      match get_change c cs with
      | None | Some None -> cs
      | Some (Some v) ->
          let show = show_of c in
          run_checks c (fun v -> show (Some v)) checks v cs

  (* Characters, not bytes: UTF-8 is decoded. *)
  let utf8_length s =
    let rec go i n =
      if i >= String.length s then n
      else go (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (n + 1)
    in
    go 0 0

  let plural n = if n = 1 then "character" else "characters"

  let length ?min ?max ?is () : string check =
   fun ~show:_ s ->
    let n = utf8_length s in
    match (is, min, max) with
    | Some k, _, _ when n <> k ->
        Some (`Length, Printf.sprintf "should be %d %s" k (plural k))
    | _, Some k, _ when n < k ->
        Some (`Length, Printf.sprintf "should be at least %d %s" k (plural k))
    | _, _, Some k when n > k ->
        Some (`Length, Printf.sprintf "should be at most %d %s" k (plural k))
    | _ -> None

  let range ?gt ?ge ?lt ?le () : _ check =
   fun ~show v ->
    let fails bound ok message =
      match bound with
      | Some b when not (ok (compare v b)) ->
          Some (`Range, Printf.sprintf "must be %s %s" message (show b))
      | _ -> None
    in
    List.find_map Fun.id
      [
        fails gt (fun c -> c > 0) "greater than";
        fails ge (fun c -> c >= 0) "greater than or equal to";
        fails lt (fun c -> c < 0) "less than";
        fails le (fun c -> c <= 0) "less than or equal to";
      ]

  let one_of xs : _ check =
   fun ~show:_ v ->
    if List.mem v xs then None else Some (`Inclusion, "is invalid")

  let none_of xs : _ check =
   fun ~show:_ v ->
    if List.mem v xs then Some (`Exclusion, "is reserved") else None

  let format ?(message = "has invalid format") ok : string check =
   fun ~show:_ v -> if ok v then None else Some (`Format, message)

  let check message ok : _ check =
   fun ~show:_ v -> if ok v then None else Some (`Custom, message)

  (* Default names are Postgres's for a constraint on one column. *)
  let add_constraint ckind suffix ?name ~message c cs =
    let cname =
      match name with
      | Some n -> n
      | None -> Printf.sprintf "%s_%s_%s" cs.table.tname c.name suffix
    in
    {
      cs with
      constraints =
        cs.constraints
        @ [
            {
              ckind;
              cname;
              named = name <> None;
              ccol = c.name;
              cmessage = message;
            };
          ];
    }

  let unique_constraint ?name ?(message = "has already been taken") c cs =
    add_constraint `Unique "key" ?name ~message c cs

  let foreign_key_constraint ?name ?(message = "does not exist") c cs =
    add_constraint `Foreign_key "fkey" ?name ~message c cs

  let check_constraint ?name ?(message = "is invalid") c cs =
    add_constraint `Check "check" ?name ~message c cs

  (* ---- which constraint a driver's message names ---- *)

  let index_from s i sub =
    let n = String.length sub in
    let rec go i =
      if i + n > String.length s then None
      else if String.sub s i n = sub then Some i
      else go (i + 1)
    in
    go i

  (* Every piece of [s] between [opening] and [closing]. *)
  let between opening closing s =
    let rec go i acc =
      match index_from s i opening with
      | None -> List.rev acc
      | Some j -> (
          let start = j + String.length opening in
          match index_from s start closing with
          | None -> List.rev acc
          | Some k -> go (k + 1) (String.sub s start (k - start) :: acc))
    in
    go 0 []

  (* The names right after [marker], separated by ", ": "users.email,
     users.org". Caqti goes on after them on the same line. *)
  let names_after marker s =
    let ident c =
      (c >= 'a' && c <= 'z')
      || (c >= 'A' && c <= 'Z')
      || is_digit c || c = '_' || c = '.'
      || Char.code c >= 128
    in
    let rec names i acc =
      let j = ref i in
      while !j < String.length s && ident s.[!j] do
        incr j
      done;
      let acc = if !j > i then String.sub s i (!j - i) :: acc else acc in
      if !j > i && !j + 1 < String.length s && s.[!j] = ',' && s.[!j + 1] = ' '
      then names (!j + 2) acc
      else List.rev acc
    in
    match index_from s 0 marker with
    | None -> []
    | Some j -> names (j + String.length marker) []

  let after_dot s =
    match String.rindex_opt s '.' with
    | Some i -> String.sub s (i + 1) (String.length s - i - 1)
    | None -> s

  (* The names in a driver's message for a refused write:
     - Postgres: ... violates unique constraint "users_email_key"
     - SQLite:   UNIQUE constraint failed: users.email, users.org
                 CHECK constraint failed: users_age_check
     - MySQL:    Duplicate entry 'x' for key 'users.users_email_key'
                 ... CONSTRAINT `comments_author_id_fkey` FOREIGN KEY ...
                 Check constraint 'users_age_check' is violated. *)
  let names_in msg =
    let quoted =
      between "constraint \"" "\"" msg
      @ between "for key '" "'" msg
      @ between "CONSTRAINT `" "`" msg
      @ between "Check constraint '" "'" msg
    in
    let sqlite_check = names_after "CHECK constraint failed: " msg in
    let sqlite_unique = names_after "UNIQUE constraint failed: " msg in
    ( List.concat_map (fun n -> [ n; after_dot n ]) (quoted @ sqlite_check),
      sqlite_unique )

  let matches cs msg con =
    let names, sqlite_cols = names_in msg in
    List.mem con.cname names
    || con.ckind = `Unique
       && (List.mem (cs.table.tname ^ "." ^ con.ccol) sqlite_cols
          || ((not con.named) && List.mem con.ccol names))

  (* SQLite doesn't say which foreign key failed: it is the one declared, if
     only one was. *)
  let unnamed_foreign_key cs msg =
    match List.filter (fun c -> c.ckind = `Foreign_key) cs.constraints with
    | [ con ] when index_from msg 0 "FOREIGN KEY constraint failed" <> None ->
        Some con
    | _ -> None

  let on_error cs e =
    let found kind =
      let msg = Caqti_error.show e in
      match
        List.find_opt
          (fun con -> con.ckind = kind && matches cs msg con)
          cs.constraints
      with
      | Some con -> Some con
      | None when kind = `Foreign_key -> unnamed_foreign_key cs msg
      | None -> None
    in
    let con =
      match violation e with
      | Some ((`Unique | `Foreign_key | `Check) as kind) -> found kind
      | Some (`Not_null | `Other) | None -> None
    in
    Option.map
      (fun con ->
        {
          cs with
          errors =
            cs.errors
            @ [
                {
                  field = con.ccol;
                  message = con.cmessage;
                  kind = (con.ckind :> kind);
                };
              ];
        })
      con

  (* What a form shows for [c]: what was typed, or else the value. *)
  let input c cs =
    match List.assoc_opt c.name cs.params with
    | Some s -> s
    | None -> (
        match (get c cs, c.ty.show) with Some v, Some show -> show v | _ -> "")

  let primary_key cs =
    match List.filter (fun (Any c) -> c.primary_key) cs.table.cols with
    | [ pk ] -> pk
    | _ ->
        invalid_arg
          (Printf.sprintf
             "Qb.Changeset: table %s needs exactly one primary key column to \
              update a row"
             cs.table.tname)
end

(* ---------- statements ---------- *)

let src = Logs.Src.create "qb" ~doc:"SQL statements run by qb"

module Log = (val Logs.src_log src : Logs.LOG)

(* Fold the parameter list into one nested-tuple Caqti type and value. *)
type packed = Pack : 'a Caqti_type.t * 'a -> packed

let finish o =
  let sql = Buffer.contents o.buf in
  let pack (Pack (ts, vs)) (Param (t, v)) =
    Pack (Caqti_type.t2 t ts, (v, vs))
  in
  (* [o.params] is reversed, so fold_left restores SQL order. *)
  ( sql,
    List.rev o.params,
    List.fold_left pack (Pack (Caqti_type.unit, ())) o.params )

(* The parameters as a log line shows them: [18, "ann@x.org", NULL]. A long
   value is cut short. Caqti prints a value as part of a row, with the
   separator and line break that come before it there. *)
let pp_params ppf = function
  | [] -> ()
  | params ->
      let show (Param (t, v)) =
        let s = Format.asprintf "%a" Caqti_type.pp_value (t, v) in
        let n = String.length s in
        let rec start i =
          if i < n && (s.[i] = ',' || s.[i] = ' ' || s.[i] = '\n') then
            start (i + 1)
          else i
        in
        let s = String.sub s (start 0) (n - start 0) in
        let s = String.concat " " (String.split_on_char '\n' s) in
        if s = "NONE" then "NULL"
        else if String.length s > 80 then String.sub s 0 77 ^ "..."
        else s
      in
      Format.fprintf ppf " [%s]" (String.concat ", " (List.map show params))

(* ---------- SQL text ---------- *)

(* What Caqti is given to send. Caqti's own parser reads more into the text
   than a statement means ([?] as a parameter next to [$1], [$(...)]), and
   raises when it can't; here a parameter is [$n] where the text has any, and
   [?] otherwise, so that Postgres's [?] operators can be written. Nothing in
   a string, a quoted name, a comment or a $$-quoted body is one. *)
let caqti_query sql =
  let toks = Sql_tokens.tokens sql in
  let numbered =
    List.exists
      (fun (t : Sql_tokens.token) ->
        match t.kind with Numbered k -> k >= 1 | _ -> false)
      toks
  in
  let parts = ref [] and text = Buffer.create (String.length sql) in
  let next = ref 0 in
  let param k =
    if Buffer.length text > 0 then (
      parts := Caqti_query.L (Buffer.contents text) :: !parts;
      Buffer.clear text);
    parts := Caqti_query.P k :: !parts
  in
  List.iter
    (fun (t : Sql_tokens.token) ->
      match t.kind with
      | Numbered k when numbered && k >= 1 -> param (k - 1)
      | Question when not numbered ->
          param !next;
          incr next
      | _ -> Buffer.add_string text t.text)
    toks;
  if Buffer.length text > 0 then
    parts := Caqti_query.L (Buffer.contents text) :: !parts;
  Caqti_query.S (List.rev !parts)

(* Requests are cached by SQL text so the driver can keep them prepared.
   An entry is reused only if Caqti confirms its parameter and row types are
   the same as the new ones, so a hit is type-safe. The cache is bounded; past
   the limit, new statements run unprepared. It is not safe to share across
   domains. *)
module Cache = struct
  type 'm entry =
    | Entry :
        'p Caqti_type.t * 'x Caqti_type.t * ('p, 'x, 'm) Caqti_request.t
        -> 'm entry

  type 'm t = { mult : 'm Caqti_mult.t; table : (string, 'm entry) Hashtbl.t }

  let limit = 512
  let make mult = { mult; table = Hashtbl.create 64 }
  let zero : Caqti_mult.zero t = make Caqti_mult.zero
  let one : Caqti_mult.one t = make Caqti_mult.one
  let zero_or_one : Caqti_mult.zero_or_one t = make Caqti_mult.zero_or_one
  let zero_or_more : Caqti_mult.zero_or_more t = make Caqti_mult.zero_or_more

  (* [~oneshot]: a statement that isn't worth keeping, whatever room there
     is. *)
  let request (type p x) ?(oneshot = false) (c : 'm t) (pt : p Caqti_type.t)
      (rt : x Caqti_type.t) sql : (p, x, 'm) Caqti_request.t =
    (* Read only for a statement that isn't in the cache yet. *)
    let query = lazy (caqti_query sql) in
    let create ~oneshot =
      Caqti_request.create ~oneshot pt rt c.mult (fun _ -> Lazy.force query)
    in
    let fresh () =
      if oneshot || Hashtbl.length c.table >= limit then create ~oneshot:true
      else
        let r = create ~oneshot:false in
        Hashtbl.replace c.table sql (Entry (pt, rt, r));
        r
    in
    match Hashtbl.find_opt c.table sql with
    | Some (Entry (pt', rt', r)) -> (
        match (Caqti_type.unify pt' pt, Caqti_type.unify rt' rt) with
        | Some Caqti_type.Equal, Some Caqti_type.Equal -> r
        | _ -> fresh ())
    | None -> fresh ()
end

(* ---------- statements written as SQL ---------- *)

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

module Stmt = struct
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
end

let stmt_name s = s.name

(* What [x IN (...)] is given for an empty list: a subquery with no rows,
   so that IN is false and NOT IN is true. Postgres wants its type. *)
let no_values dialect ty =
  match dialect with
  | Postgres ->
      Printf.sprintf "SELECT CAST(NULL AS %s) WHERE 1 = 0" (ty.ddl Postgres)
  | Sqlite -> "SELECT NULL WHERE 1 = 0"
  | Mysql -> "SELECT NULL FROM DUAL WHERE 1 = 0"

(* The text to send and its parameters in order. With a list argument, each
   [$k] is renumbered: a list of three at [$2] becomes [$2, $3, $4] and what
   was [$3] becomes [$5]. Without one, the text is sent as it is. *)
let expand dialect s =
  let args = Array.of_list s.args in
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
    let start = Array.make (Array.length args) 1 in
    for k = 1 to Array.length args - 1 do
      start.(k) <- start.(k - 1) + size args.(k - 1)
    done;
    let used = Array.make (Array.length args) false in
    let b = Buffer.create (String.length s.sql + 32) in
    List.iter
      (fun (t : Sql_tokens.token) ->
        match t.kind with
        | Numbered k -> (
            if k < 1 || k > Array.length args then
              invalid_arg (Printf.sprintf "Qb.Stmt: $%d has no argument" k);
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
      (Sql_tokens.tokens s.sql);
    Array.iteri
      (fun k u ->
        match args.(k) with
        | Many _ when not u ->
            invalid_arg
              (Printf.sprintf
                 "Qb.Stmt: the list argument %d is not in the SQL as $%d"
                 (k + 1) (k + 1))
        | _ -> ())
      used;
    (Buffer.contents b, params)

(* A statement's parameters as one nested-tuple Caqti type and value. *)
let pack_args dialect s =
  let sql, params = expand dialect s in
  ( sql,
    params,
    List.fold_left
      (fun (Pack (ts, vs)) (Param (t, v)) -> Pack (Caqti_type.t2 t ts, (v, vs)))
      (Pack (Caqti_type.unit, ()))
      (List.rev params) )

let stmt_sql ?(dialect = Postgres) s = Stdlib.fst (expand dialect s)

(* A statement whose text depends on how long a list argument is. *)
let variadic s = List.exists (function Many _ -> true | One _ -> false) s.args

(* ---------- running on Caqti ---------- *)

module type BACKEND = sig
  type +'a fiber
  type (+'a, +'e) stream

  module type CONNECTION =
    Caqti_connection_sig.S
      with type 'a fiber := 'a fiber
       and type ('a, 'e) stream := ('a, 'e) stream

  val return : 'a -> 'a fiber
  val map : ('a -> 'b) -> 'a fiber -> 'b fiber
end

module type S = sig
  type 'a fiber
  type db

  val all :
    db ->
    ('r, 'a, _) query ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val first :
    db ->
    ('r, 'a, _) query ->
    ('a option, [> Caqti_error.call_or_retrieve ]) result fiber

  val count :
    db ->
    ('r, _, _) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert :
    db ->
    'r table ->
    'r ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_returning :
    db ->
    'r table ->
    'r assign list ->
    ('r, 'a) sel ->
    ('a, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_values :
    db -> 'r values -> ('r, [> Caqti_error.call_or_retrieve ]) result fiber

  val update :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete :
    db ->
    ('r, _, filtered * plain) query ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_many :
    db ->
    'r table ->
    'r list ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_count :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_count :
    db ->
    ('r, _, filtered * plain) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_returning :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_returning :
    db ->
    ('r, _, filtered * plain) query ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val transaction :
    db ->
    (unit -> ('a, 'e) result fiber) ->
    ('a, ([> Caqti_error.transact ] as 'e)) result fiber

  val create_table :
    db -> 'r table -> (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_changeset :
    db ->
    'r Changeset.t ->
    ('r, [> Caqti_error.call_or_retrieve | `Invalid of 'r Changeset.t ]) result
    fiber

  val update_changeset :
    db ->
    'r Changeset.t ->
    ( 'r,
      [> Caqti_error.call_or_retrieve | `Invalid of 'r Changeset.t | `Stale ]
    )
    result
    fiber

  val fetch_all :
    db -> 'a stmt -> ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val fetch_opt :
    db -> 'a stmt -> ('a option, [> Caqti_error.call_or_retrieve ]) result fiber

  val fetch_one :
    db -> 'a stmt -> ('a, [> Caqti_error.call_or_retrieve ]) result fiber

  val execute :
    db -> unit stmt -> (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val execute_count :
    db -> unit stmt -> (int, [> Caqti_error.call_or_retrieve ]) result fiber
end

module type GUARDED_BACKEND = sig
  include BACKEND

  val finally : (unit -> 'a fiber) -> (unit -> unit) -> 'a fiber
end

(* The runner; [G.finally] is there for a guarded backend. *)
module Make_with
    (B : BACKEND)
    (G : sig
      type finally = {
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      val finally : finally option
    end) =
struct
  type 'a fiber = 'a B.fiber
  type db = (module B.CONNECTION)

  let out_for (module Db : B.CONNECTION) =
    out
      (match Caqti_driver_info.dialect_tag Db.driver_info with
      | `Sqlite -> Sqlite
      | `Mysql -> Mysql
      | _ -> Postgres)

  (* One debug line for each statement, once it has run: its text, its
     parameters, what came of it and how long it took. Nothing is timed or
     formatted unless the source is at debug level. *)
  let logged sql params describe run =
    if Logs.Src.level src <> Some Logs.Debug then run ()
    else
      let clock = Mtime_clock.counter () in
      run ()
      |> B.map (fun r ->
          Log.debug (fun m ->
              m "%s%a -> %s, %a" sql pp_params params
                (match r with
                | Ok x -> describe x
                | Error (#Caqti_error.call_or_retrieve as e) ->
                    "failed: " ^ Caqti_error.show e
                | Error _ -> "failed")
                Mtime.Span.pp (Mtime_clock.count clock));
          r)

  let rows n = if n = 1 then "1 row" else string_of_int n ^ " rows"

  let exec (module Db : B.CONNECTION) o =
    match finish o with
    | sql, ps, Pack (pt, pv) ->
        logged sql ps
          (fun () -> "done")
          (fun () ->
            Db.exec
              (Cache.request ~oneshot:o.variadic Cache.zero pt Caqti_type.unit
                 sql)
              pv)

  (* Every driver qb renders for counts affected rows; one that can't says
     so as an error of the usual kind. *)
  let counted (module Db : B.CONNECTION) ~oneshot sql ps pt pv =
    logged sql ps
      (fun n -> rows n ^ " changed")
      (fun () ->
        Db.exec_with_affected_count
          (Cache.request ~oneshot Cache.zero pt Caqti_type.unit sql)
          pv
        |> B.map (function
          | Ok n -> Ok n
          | Error `Unsupported ->
              Error
                (Caqti_error.response_rejected ~uri:Uri.empty ~query:sql
                   (Caqti_error.Msg "this driver doesn't count affected rows"))
          | Error (#Caqti_error.call_or_retrieve as e) -> Error e))

  let exec_count db o =
    match finish o with
    | sql, ps, Pack (pt, pv) -> counted db ~oneshot:o.variadic sql ps pt pv

  let collect_list (module Db : B.CONNECTION) ~oneshot sql ps pt pv rt decode =
    logged sql ps
      (fun l -> rows (List.length l))
      (fun () ->
        Db.collect_list (Cache.request ~oneshot Cache.zero_or_more pt rt sql) pv)
    |> B.map (Result.map (List.map decode))

  let find_opt (module Db : B.CONNECTION) ~oneshot sql ps pt pv rt decode =
    logged sql ps
      (function Some _ -> "1 row" | None -> "no row")
      (fun () ->
        Db.find_opt (Cache.request ~oneshot Cache.zero_or_one pt rt sql) pv)
    |> B.map (Result.map (Option.map decode))

  let find (module Db : B.CONNECTION) ~oneshot sql ps pt pv rt decode =
    logged sql ps
      (fun _ -> "1 row")
      (fun () -> Db.find (Cache.request ~oneshot Cache.one pt rt sql) pv)
    |> B.map (Result.map decode)

  let collect db o returning =
    match decoder returning with
    | Dec (rt, decode, _) -> (
        render_returning o returning;
        match finish o with
        | sql, ps, Pack (pt, pv) ->
            collect_list db ~oneshot:o.variadic sql ps pt pv rt decode)

  let all db q =
    match prepare q with
    | Prepared (rt, decode, render) -> (
        let o = out_for db in
        render o;
        match finish o with
        | sql, ps, Pack (pt, pv) ->
            collect_list db ~oneshot:o.variadic sql ps pt pv rt decode)

  let first db q =
    match prepare ~limit:1 q with
    | Prepared (rt, decode, render) -> (
        let o = out_for db in
        render o;
        match finish o with
        | sql, ps, Pack (pt, pv) ->
            find_opt db ~oneshot:o.variadic sql ps pt pv rt decode)

  let count ((module Db : B.CONNECTION) as db) q =
    let o = out_for db in
    render_count o q;
    match finish o with
    | sql, ps, Pack (pt, pv) ->
        logged sql ps string_of_int (fun () ->
            Db.find
              (Cache.request ~oneshot:o.variadic Cache.one pt Caqti_type.int sql)
              pv)

  let insert db t row =
    let o = out_for db in
    render_insert o t t.cols
      (List.map (fun (Any c) () -> param o c (c.get row)) t.cols);
    exec db o

  let insert_returning db t assigns returning =
    check_nonempty "Qb.insert_returning" returning;
    match decoder returning with
    | Dec (rt, decode, cols) -> (
        let o = out_for db in
        render_insert o t
          (List.map (fun (Set (c, _)) -> Any c) assigns)
          (List.map (fun (Set (c, v)) () -> param o c v) assigns);
        str o " RETURNING ";
        col_names o cols;
        match finish o with
        | sql, ps, Pack (pt, pv) ->
            find db ~oneshot:o.variadic sql ps pt pv rt decode)

  let insert_values db v = insert_returning db v.vtable v.vassigns v.vtable.row

  let update db assigns q =
    match assigns with
    | [] -> B.return (Ok ())
    | _ ->
        let o = out_for db in
        render_update o assigns q;
        exec db o

  let delete db q =
    let o = out_for db in
    render_delete o q;
    exec db o

  let insert_many db t = function
    | [] -> B.return (Ok ())
    | rows ->
        let o = out_for db in
        render_insert_many o t rows;
        exec db o

  let update_count db assigns q =
    match assigns with
    | [] -> B.return (Ok 0)
    | _ ->
        let o = out_for db in
        render_update o assigns q;
        exec_count db o

  let delete_count db q =
    let o = out_for db in
    render_delete o q;
    exec_count db o

  let update_returning db assigns q returning =
    if assigns = [] then invalid_arg "Qb.update_returning: no assignments";
    check_nonempty "Qb.update_returning" returning;
    let o = out_for db in
    render_update o assigns q;
    collect db o returning

  let delete_returning db q returning =
    check_nonempty "Qb.delete_returning" returning;
    let o = out_for db in
    render_delete o q;
    collect db o returning

  (* A constraint the changeset declared becomes an error on it. *)
  let refused cs = function
    | Ok r -> Ok r
    | Error e -> (
        match e with
        | #Caqti_error.call_or_retrieve as ce -> (
            match Changeset.on_error cs ce with
            | Some cs -> Error (`Invalid cs)
            | None -> Error e)
        | _ -> Error e)

  let insert_changeset db (cs : _ Changeset.t) =
    if Option.is_some cs.data then
      invalid_arg
        "Qb.insert_changeset: the changeset is for_update; use update_changeset";
    if not (Changeset.is_valid cs) then B.return (Error (`Invalid cs))
    else insert_values db (values cs.table cs.changes) |> B.map (refused cs)

  let update_changeset db (cs : _ Changeset.t) =
    match cs.data with
    | None ->
        invalid_arg
          "Qb.update_changeset: the changeset is for_insert; use \
           insert_changeset"
    | Some row -> (
        let (Any pk) = Changeset.primary_key cs in
        if not (Changeset.is_valid cs) then B.return (Error (`Invalid cs))
        else
          match cs.changes with
          | [] -> B.return (Ok row)
          | changes ->
              let q = from cs.table |> where Ops.(pk = pk.get row) in
              update_returning db changes q cs.table.row
              |> B.map (function
                | Ok (r :: _) -> Ok r
                | Ok [] -> Error `Stale
                | Error _ as e -> refused cs e))

  (* The connections now inside [transaction]. Caqti doesn't nest them: a
     second BEGIN is ignored by the database and the inner COMMIT then ends
     the outer transaction early, so that is refused here. It takes
     [G.finally] to know when a transaction is over, however it ends. *)
  let in_transaction : Obj.t list ref = ref []

  let transaction ((module Db : B.CONNECTION) as db) f =
    match G.finally with
    | None -> Db.with_transaction f
    | Some { finally } ->
        let key = Obj.repr db in
        if List.memq key !in_transaction then
          invalid_arg
            "Qb.transaction: this connection is already in a transaction, and \
             transactions don't nest";
        in_transaction := key :: !in_transaction;
        finally
          (fun () -> Db.with_transaction f)
          (fun () ->
            in_transaction := List.filter (fun k -> k != key) !in_transaction)

  let create_table db t =
    let o = out_for db in
    str o (create_table_sql ~dialect:o.dialect t);
    exec db o

  let fetch_all db s =
    match (s.row, pack_args (out_for db).dialect s) with
    | Row_dec (rt, decode), (sql, ps, Pack (pt, pv)) ->
        collect_list db ~oneshot:(variadic s) sql ps pt pv rt decode

  let fetch_opt db s =
    match (s.row, pack_args (out_for db).dialect s) with
    | Row_dec (rt, decode), (sql, ps, Pack (pt, pv)) ->
        find_opt db ~oneshot:(variadic s) sql ps pt pv rt decode

  let fetch_one db s =
    match (s.row, pack_args (out_for db).dialect s) with
    | Row_dec (rt, decode), (sql, ps, Pack (pt, pv)) ->
        find db ~oneshot:(variadic s) sql ps pt pv rt decode

  let execute ((module Db : B.CONNECTION) as db) s =
    match pack_args (out_for db).dialect s with
    | sql, ps, Pack (pt, pv) ->
        logged sql ps
          (fun () -> "done")
          (fun () ->
            Db.exec
              (Cache.request ~oneshot:(variadic s) Cache.zero pt Caqti_type.unit
                 sql)
              pv)

  let execute_count db s =
    match pack_args (out_for db).dialect s with
    | sql, ps, Pack (pt, pv) -> counted db ~oneshot:(variadic s) sql ps pt pv
end

module Make (B : BACKEND) =
  Make_with
    (B)
    (struct
      type finally = {
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      let finally = None
    end)

module Make_guarded (B : GUARDED_BACKEND) =
  Make_with
    (B)
    (struct
      type finally = {
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      let finally = Some { finally = B.finally }
    end)

module Blocking_backend = struct
  type 'a fiber = 'a
  type ('a, 'e) stream = ('a, 'e) Caqti_blocking.Stream.t

  module type CONNECTION = Caqti_blocking.CONNECTION

  let return x = x
  let map f x = f x

  let finally f after =
    match f () with
    | x ->
        after ();
        x
    | exception e ->
        after ();
        raise e
end

module Blocking = Make_guarded (Blocking_backend)
