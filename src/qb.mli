(** A Drizzle-style query builder for single-table CRUD on Caqti.

    A table is defined once: a record type, one {!column} per field, and a
    {!val-table} value that says how columns make a row. From that one
    definition you get typed select / insert / update / delete and the CREATE
    TABLE.

    Every column and condition carries the row type ['r], so a column of one
    table cannot be used in a query on another.

    {[
    module Users = struct
      type t = { id : int; name : string; email : string option }

      let id =
        Qb.column "id" Qb.int
          (fun u -> u.id)
          ~primary_key:true ~auto_increment:true

      let name = Qb.column "name" Qb.text (fun u -> u.name)
      let email = Qb.column "email" (Qb.nullable Qb.text) (fun u -> u.email)

      let table =
        Qb.(
          table "users"
            (let+ id = col id and+ name = col name and+ email = col email in
             { id; name; email }))
    end

    let not_bob db =
      Qb.(
        from Users.table |> where Ops.(Users.name <> "Bob") |> Blocking.all db)
    ]}

    Queries run on blocking connections through {!Blocking}, on Lwt through the
    [qb-lwt] package, or on any other Caqti backend through {!Make}. *)

(** SQL dialects qb renders for. Running a query picks the dialect from the
    connection; {!to_sql} and {!create_table_sql} take it as an argument. *)
type dialect = Postgres | Sqlite | Mysql

(** {1 Column types} *)

type 'a ty
(** How an OCaml value of type ['a] is stored: its Caqti type and SQL type. *)

val ty :
  ?sqlite:string ->
  ?mysql:string ->
  ?of_string:(string -> 'a option) ->
  ?to_string:('a -> string) ->
  'a Caqti_type.t ->
  string ->
  'a ty
(** [ty caqti ddl] is a non-null column type. [ddl] is its SQL type in Postgres,
    and in SQLite and MySQL unless [?sqlite] / [?mysql] say otherwise.

    [?of_string] reads a value from text, [None] when the text isn't one, and
    [?to_string] writes one: {!Changeset.cast} and {!Changeset.input} use them.
    Both are found for you when [caqti] is a type qb knows ([Caqti_type.string],
    [int], [int32], [int64], [float], [bool], [ptime], [pdate], [octets]); give
    them for anything else that will be cast. *)

val int : int ty
(** [BIGINT] ([INTEGER] in SQLite), so all 63 bits of an OCaml [int] fit. *)

val int64 : int64 ty
val float : float ty

(** [DOUBLE PRECISION] ([REAL] in SQLite, [DOUBLE] in MySQL). *)

val text : string ty
val bool : bool ty

val octets : string ty
(** Binary data: [BYTEA] ([BLOB] in SQLite, [LONGBLOB] in MySQL). *)

val ptime : Ptime.t ty
(** A point in time: [TIMESTAMP WITH TIME ZONE] ([TEXT] in SQLite, [DATETIME(6)]
    in MySQL). *)

val date : Ptime.t ty
(** A calendar date: [DATE] ([TEXT] in SQLite). *)

val uuid : string ty
(** A UUID in its text form: [UUID] ([TEXT] in SQLite, [CHAR(36)] in MySQL). *)

val json : string ty
(** A JSON document as text: [JSONB] ([TEXT] in SQLite, [JSON] in MySQL). *)

val nullable : 'a ty -> 'a option ty
(** A column that may be NULL, read as [None]. Comparing it [= None] renders
    [IS NULL]. *)

(** {1 Columns} *)

type ('r, 'a) col
(** A column of a table whose rows are ['r], holding values of type ['a]. *)

val column :
  ?primary_key:bool ->
  ?auto_increment:bool ->
  string ->
  'a ty ->
  ('r -> 'a) ->
  ('r, 'a) col
(** [column name ty get]: [get] reads the field out of a row (used by
    {!S.insert}). [~auto_increment:true] lets the database assign the value when
    an insert leaves it out (an identity column in Postgres, [AUTO_INCREMENT] in
    MySQL; in SQLite an [int] primary key already does this). *)

(** {1 Selections} *)

(** A row read with list syntax, matched with list syntax:
    [fun Row.[ id; name ] -> ...]. The [Row.] prefix is needed in patterns;
    without it, [[ id; name ]] is an ordinary list. *)
module Row : sig
  type _ t = [] : unit t | ( :: ) : 'a * 'b t -> ('a * 'b) t
end

(** Selections. Write them with list syntax or with {!val-col}, [let+] and
    [and+]; the constructors live here so that [open Qb] leaves ordinary lists
    alone. *)
module Sel : sig
  type ('r, 'a) expr
  (** A selection built with {!val-col}, [let+] and [and+]. *)

  type ('r, _) t =
    | [] : ('r, unit Row.t) t
    | ( :: ) : ('r, 'a) col * ('r, 'b Row.t) t -> ('r, ('a * 'b) Row.t) t
    | Expr : ('r, 'a) expr -> ('r, 'a) t
end

type ('r, 'a) sel = ('r, 'a) Sel.t
(** Which columns to read from a table of ['r] rows and how to build an ['a]
    from them. Two ways to write one:

    - A list of columns: [select [ Users.id; Users.name ]]. Rows come back as
      {!Row.t}, read with [fun Row.[ id; name ] -> ...]. Adding a column only
      changes the list and the pattern. An empty list raises [Invalid_argument].
    - An applicative, combined with [let+] and [and+], when you want a record or
      labeled tuple:
      [let+ id = col Users.id and+ name = col Users.name in (~id, ~name)]. *)

val col : ('r, 'a) col -> ('r, 'a) sel
val ( let+ ) : ('r, 'a) sel -> ('a -> 'b) -> ('r, 'b) sel
val ( and+ ) : ('r, 'a) sel -> ('r, 'b) sel -> ('r, 'a * 'b) sel

(** {1 Tables} *)

type 'r table

val table : ?schema:string -> string -> ('r, 'r) sel -> 'r table
(** [table name row]: [row] reads every column and builds a whole row. [~schema]
    qualifies the name: [table ~schema:"billing" "invoices"] renders
    ["billing"."invoices"] (a schema in Postgres, an attached database in
    SQLite, a database in MySQL). *)

val create_table_sql : ?dialect:dialect -> 'r table -> string
(** [dialect] defaults to [Postgres]. Table and column names are quoted. *)

(** {1 Conditions and assignments} *)

type 'r cond
type 'r assign

val all_rows : 'r cond
(** Matches every row. {!S.update} and {!S.delete} only accept a query that has
    been through {!where}, so acting on the whole table must be spelled out:
    [from t |> where all_rows |> Blocking.delete db]. *)

val set : ('r, 'a) col -> 'a -> 'r assign

(** {1 SQL expressions and raw fragments}

    Typed SQL expressions for {!Query} and expression-based writes
    ({!set_expr}). Unlike single-table {!type-cond}, expressions carry their SQL
    result type ['a] and compose across joins, aggregates, subqueries, and
    dialect-specific {!Raw} fragments. *)

module Expr : sig
  type 'a t
  (** A SQL expression producing values of type ['a]. *)

  type any
  (** An expression of any type, for {!Query.group_by}, {!Query.distinct_on},
      and window [PARTITION BY]. *)

  val any : _ t -> any

  type order
  (** An ordering item, shared by {!Query.order_by} and window [ORDER BY]. *)

  val asc : _ t -> order
  val desc : _ t -> order
  val nulls_first : order -> order
  val nulls_last : order -> order

  (** {2 Columns and literals} *)

  val col : ('r, 'a) col -> 'a t
  (** A column of the target table in a single-table {!set_expr} assignment. *)

  val excluded : ('r, 'a) col -> 'a t
  (** [EXCLUDED."col"], for {!On_conflict.do_update} [~set] expressions. *)

  val lit : 'a ty -> 'a -> 'a t
  (** A bound parameter value of type ['a]. *)

  val int : int -> int t
  val int64 : int64 -> int64 t
  val float : float -> float t
  val text : string -> string t
  val bool : bool -> bool t
  val ptime : Ptime.t -> Ptime.t t
  val date : Ptime.t -> Ptime.t t
  val uuid : string -> string t
  val json : string -> string t
  val null : 'a ty -> 'a option t
  val some : 'a t -> 'a option t

  (** {2 Scalar SQL functions} *)

  val lower : string t -> string t
  val upper : string t -> string t
  val trim : string t -> string t
  val length : string t -> int t
  val concat : string t -> string t -> string t
  val coalesce : 'a option t -> 'a t -> 'a t
  val coalesce_opt : 'a option t -> 'a option t -> 'a option t
  val nullif : 'a t -> 'a t -> 'a option t
  val case_when : (bool t * 'a t) list -> else_:'a t -> 'a t
  val case_when_opt : (bool t * 'a t) list -> 'a option t
  val now : unit -> Ptime.t t
  val cast : 'b ty -> _ t -> 'b t

  (** {2 Aggregates} *)

  val count_all : int t
  (** Count of all matching rows. *)

  val count : _ t -> int t
  val count_distinct : _ t -> int t
  val sum_int : int t -> int option t
  val sum_int64 : int64 t -> int64 option t
  val sum_float : float t -> float option t
  val avg : _ t -> float option t
  val min : 'a t -> 'a option t
  val max : 'a t -> 'a option t
  val min_opt : 'a option t -> 'a option t
  val max_opt : 'a option t -> 'a option t

  val filter : bool t -> 'a t -> 'a t
  (** SQL [FILTER (WHERE cond)] on an aggregate (Postgres and SQLite 3.30+):
      [Expr.count_all |> Expr.filter Expr.Ops.(u.@(Users.age) >= Expr.int 18)].
  *)

  (** {2 Window functions} *)

  val over : ?partition_by:any list -> ?order_by:order list -> 'a t -> 'a t
  (** Turns any aggregate (including a {!filter}ed aggregate) into a window
      function ([OVER (PARTITION BY ... ORDER BY ...)]). For aggregates that
      emit a Postgres [CAST] ([sum_int], [avg]), [OVER (...)] is placed inside
      the cast automatically. *)

  val row_number :
    ?partition_by:any list -> ?order_by:order list -> unit -> int t

  val rank : ?partition_by:any list -> ?order_by:order list -> unit -> int t

  val dense_rank :
    ?partition_by:any list -> ?order_by:order list -> unit -> int t

  val lag :
    ?offset:int ->
    ?partition_by:any list ->
    ?order_by:order list ->
    'a t ->
    'a option t

  val lag_default :
    ?offset:int ->
    default:'a t ->
    ?partition_by:any list ->
    ?order_by:order list ->
    'a t ->
    'a t

  val lead :
    ?offset:int ->
    ?partition_by:any list ->
    ?order_by:order list ->
    'a t ->
    'a option t

  val lead_default :
    ?offset:int ->
    default:'a t ->
    ?partition_by:any list ->
    ?order_by:order list ->
    'a t ->
    'a t

  val first_value :
    ?partition_by:any list -> ?order_by:order list -> 'a t -> 'a option t

  val last_value :
    ?partition_by:any list -> ?order_by:order list -> 'a t -> 'a option t

  (** {2 Keyset / cursor pagination (tuple comparisons)}

      Renders row-value comparisons like [(a, b) < (?, ?)], supported by
      Postgres, SQLite, and MySQL. *)
  module Keyset : sig
    val gt2 : 'a t * 'b t -> 'a t * 'b t -> bool t
    val ge2 : 'a t * 'b t -> 'a t * 'b t -> bool t
    val lt2 : 'a t * 'b t -> 'a t * 'b t -> bool t
    val le2 : 'a t * 'b t -> 'a t * 'b t -> bool t
    val gt3 : 'a t * 'b t * 'c t -> 'a t * 'b t * 'c t -> bool t
    val ge3 : 'a t * 'b t * 'c t -> 'a t * 'b t * 'c t -> bool t
    val lt3 : 'a t * 'b t * 'c t -> 'a t * 'b t * 'c t -> bool t
    val le3 : 'a t * 'b t * 'c t -> 'a t * 'b t * 'c t -> bool t
  end

  (** {2 JSON / JSONB helpers}

      [get] ([->]) and [get_text] ([->>]) work on Postgres and SQLite 3.38+.
      [contains] ([@>]), [has_key] ([jsonb_exists]), and [path_text]
      ([jsonb_extract_path_text]) are Postgres JSONB operations. *)
  module Json : sig
    val get : string t -> string t -> string option t
    val get_text : string t -> string t -> string option t
    val get_opt : string option t -> string t -> string option t
    val get_text_opt : string option t -> string t -> string option t
    val contains : string t -> string t -> bool t
    val contains_opt : string option t -> string t -> bool t
    val has_key : string t -> string t -> bool t
    val has_key_opt : string option t -> string t -> bool t
    val path_text : string t -> string list -> string option t
    val path_text_opt : string option t -> string list -> string option t
  end

  (** {2 Postgres-specific expression helpers} *)
  module Pg : sig
    val any_array : 'a t -> string t -> bool t
    (** [e = ANY(arr)] *)

    val date_trunc : string -> Ptime.t t -> Ptime.t t
    (** [date_trunc(unit, ts)] *)
  end

  (** Expression operators, meant to be opened locally:
      [Expr.Ops.(u.@(Users.age) > Expr.int 18 && p.@(Posts.published) =
       Expr.bool true)]. *)
  module Ops : sig
    val ( = ) : 'a t -> 'a t -> bool t
    val ( <> ) : 'a t -> 'a t -> bool t
    val ( > ) : 'a t -> 'a t -> bool t
    val ( >= ) : 'a t -> 'a t -> bool t
    val ( < ) : 'a t -> 'a t -> bool t
    val ( <= ) : 'a t -> 'a t -> bool t

    (** Compare a nullable expression with a non-null expression. *)

    val ( =? ) : 'a option t -> 'a t -> bool t
    val ( <>? ) : 'a option t -> 'a t -> bool t
    val ( >? ) : 'a option t -> 'a t -> bool t
    val ( >=? ) : 'a option t -> 'a t -> bool t
    val ( <? ) : 'a option t -> 'a t -> bool t
    val ( <=? ) : 'a option t -> 'a t -> bool t
    val ( && ) : bool t -> bool t -> bool t
    val ( || ) : bool t -> bool t -> bool t
    val not : bool t -> bool t
    val all_of : bool t list -> bool t option
    val any_of : bool t list -> bool t option
    val is_null : 'a option t -> bool t
    val is_not_null : 'a option t -> bool t
    val is_in : 'a t -> 'a list -> bool t
    val not_in : 'a t -> 'a list -> bool t
    val between : 'a t -> 'a t -> 'a t -> bool t
    val like : string t -> string t -> bool t
    val ilike : string t -> string t -> bool t
    val like_opt : string option t -> string t -> bool t
    val ilike_opt : string option t -> string t -> bool t
    val ( + ) : int t -> int t -> int t
    val ( - ) : int t -> int t -> int t
    val ( * ) : int t -> int t -> int t
    val ( / ) : int t -> int t -> int t
    val ( +. ) : float t -> float t -> float t
    val ( -. ) : float t -> float t -> float t
    val ( *. ) : float t -> float t -> float t
    val ( /. ) : float t -> float t -> float t
    val ( ^ ) : string t -> string t -> string t
  end
end

(** Typed raw SQL fragments that compose into any {!Expr.t} (conditions,
    projections, [ORDER BY], [GROUP BY], [SET]) with safely bound parameters and
    alias-qualified columns.

    {[
    let matches_ci col pattern =
      Qb.Raw.bool
        [ Qb.Raw.expr col; Qb.Raw.sql " ~* "; Qb.Raw.param Qb.text pattern ]
    ]}

    Always pass untrusted values via {!Raw.param}, never by string concatenation
    into {!Raw.sql}. *)
module Raw : sig
  type frag

  val sql : string -> frag
  (** Literal SQL text emitted verbatim. *)

  val param : 'a ty -> 'a -> frag
  (** A bound Caqti parameter ([?]). *)

  val expr : _ Expr.t -> frag
  (** Embeds any {!Expr.t} with its table alias qualification intact. *)

  val col : ('r, _) col -> frag
  (** Embeds a single-table column (for {!set_expr}). *)

  val make : 'a ty -> frag list -> 'a Expr.t
  (** Combines fragments into an expression decoded with the given {!type-ty}.
  *)

  val bool : frag list -> bool Expr.t
  (** Shorthand for [make bool frags]. *)
end

val set_expr : ('r, 'a) col -> 'a Expr.t -> 'r assign
(** Assign a SQL expression to a column in {!S.update},
    {!On_conflict.do_update}, or {!val-values}:
    [set_expr Users.age Expr.Ops.(Expr.col Users.age + Expr.int 1)]. *)

(** {1 Relations}

    Typed foreign-key descriptors for relation-driven joins ({!Query.join_many},
    {!Query.left_join_many}, {!Query.join_one}, {!Query.left_join_one}) and
    batched eager loading ({!S.load_many}, {!S.load_one}). [qb-pg-parse gen]
    generates a [Relations] module automatically from [REFERENCES] constraints
    in your migrations.

    {[
    let user_comments =
      Qb.Rel.has_many ~parent_key:Users.id ~child:Comments.table
        ~child_key:Comments.author_id

    let users_with_comments db =
      let* users = Qb.(from Users.table |> Blocking.all db) in
      Qb.Blocking.load_many db user_comments users
        ~order_by:[ Qb.asc Comments.id ]
    ]} *)
module Rel : sig
  type ('parent, 'child) has_many
  type ('parent, 'child) has_one
  type ('child, 'parent) belongs_to
  type ('source, 'target) many_to_many

  val has_many :
    parent_key:('parent, 'k) col ->
    child:'child table ->
    child_key:('child, 'k) col ->
    ('parent, 'child) has_many

  val has_many_opt :
    parent_key:('parent, 'k) col ->
    child:'child table ->
    child_key:('child, 'k option) col ->
    ('parent, 'child) has_many

  val has_one :
    parent_key:('parent, 'k) col ->
    child:'child table ->
    child_key:('child, 'k) col ->
    ('parent, 'child) has_one

  val has_one_opt :
    parent_key:('parent, 'k) col ->
    child:'child table ->
    child_key:('child, 'k option) col ->
    ('parent, 'child) has_one

  val belongs_to :
    child_key:('child, 'k) col ->
    parent:'parent table ->
    parent_key:('parent, 'k) col ->
    ('child, 'parent) belongs_to

  val belongs_to_opt :
    child_key:('child, 'k option) col ->
    parent:'parent table ->
    parent_key:('parent, 'k) col ->
    ('child, 'parent) belongs_to

  val many_to_many :
    source_key:('source, 'sk) col ->
    through:'through table ->
    through_source_key:('through, 'sk) col ->
    through_target_key:('through, 'tk) col ->
    target:'target table ->
    target_key:('target, 'tk) col ->
    ('source, 'target) many_to_many

  val flatten :
    ('parent * 'child list) list ->
    'child list * ('enriched list -> ('parent * 'enriched list) list)
  (** Composes multi-level batched loading without N+1:
      {[
      let* users_posts = Blocking.load_many db Users.posts users in
      let flat_posts, reattach = Qb.Rel.flatten users_posts in
      let* posts_comments = Blocking.load_many db Posts.comments flat_posts in
      let nested = reattach posts_comments
      ]} *)

  val flatten_opt :
    ('parent * 'child option) list ->
    'child list * ('enriched list -> ('parent * 'enriched option) list)
  (** Like {!flatten} for 1-to-1 / optional child results. *)
end

type 'r values
(** A new row of a table of ['r] rows, given as some of its columns; the
    database fills in the rest (identities, defaults, NULL). Run it with
    {!S.insert_values}, which returns the whole row as stored. *)

val values : 'r table -> 'r assign list -> 'r values
(** [values Users.table Ops.[ Users.name := "Ann"; Users.age := 34 ]]. Generated
    tables (the [qb-pg-parse] package) have a [values] function that takes each
    column as an argument, required unless the database can fill it in. *)

type 'r on_conflict
(** What to do when {!S.upsert} or {!S.upsert_many} hits a unique or primary-key
    conflict. *)

(** Conflict handling for {!S.upsert} and {!S.upsert_many}.

    {[
    Qb.Blocking.upsert db
      Qb.On_conflict.(
        do_update
          ~target:[ col Users.email ]
          ~excluded:[ col Users.name; col Users.age ]
          ())
      (Users.values ~email:"ann@x.org" ~name:"Ann" ~age:34 ())
    ]} *)
module On_conflict : sig
  type 'r column
  (** A column of any type, to list conflict target or [EXCLUDED] columns:
      [[ col Users.email; col Users.org_id ]]. *)

  val col : ('r, _) col -> 'r column

  val do_nothing : ?target:'r column list -> unit -> 'r on_conflict
  (** Skips conflicting rows ([ON CONFLICT DO NOTHING], or
      [ON CONFLICT (target) DO NOTHING]). *)

  val do_update :
    target:'r column list ->
    ?set:'r assign list ->
    ?excluded:'r column list ->
    ?where:'r cond ->
    unit ->
    'r on_conflict
  (** Updates conflicting rows ([ON CONFLICT (target) DO UPDATE SET ...]).
      [~excluded] sets each listed column to its proposed [EXCLUDED] value;
      [~set] sets explicit values; [~where] filters which existing rows are
      updated (table columns are qualified to avoid clashing with [EXCLUDED]).
      Raises [Invalid_argument] if [target] is empty or if both [~set] and
      [~excluded] are empty. *)
end

(** Operators, meant to be opened locally:
    [Ops.(Users.age > 18 && Users.name <> "Bob")]. *)
module Ops : sig
  val ( = ) : ('r, 'a) col -> 'a -> 'r cond
  val ( <> ) : ('r, 'a) col -> 'a -> 'r cond
  val ( > ) : ('r, 'a) col -> 'a -> 'r cond
  val ( >= ) : ('r, 'a) col -> 'a -> 'r cond
  val ( < ) : ('r, 'a) col -> 'a -> 'r cond
  val ( <= ) : ('r, 'a) col -> 'a -> 'r cond

  val ( == ) : ('a, 'x) col -> ('b, 'x) col -> ('a * 'b) cond
  (** The ON of a {!join}: a column of the left table equals one of the right
      table, each written as its table's own column:
      [Ops.(Comments.author_id == Users.id)]. Shadows physical equality inside
      [Ops.( ... )]. *)

  val ( === ) : ('r, 'a) col -> ('r, 'a) col -> 'r cond
  (** Two columns of the same table are equal. *)

  val ( && ) : 'r cond -> 'r cond -> 'r cond
  val ( || ) : 'r cond -> 'r cond -> 'r cond
  val not : 'r cond -> 'r cond

  val all_of : 'r cond list -> 'r cond option
  (** Combines conditions with [AND], or [None] for [[]]. Pairs with
      {!where_opt}. *)

  val any_of : 'r cond list -> 'r cond option
  (** Combines conditions with [OR], or [None] for [[]]. Pairs with
      {!where_opt}. *)

  val is_in : ('r, 'a) col -> 'a list -> 'r cond
  (** [is_in c []] matches nothing. *)

  val not_in : ('r, 'a) col -> 'a list -> 'r cond
  (** [not_in c []] matches every row. *)

  val between : ('r, 'a) col -> 'a -> 'a -> 'r cond
  (** [between c lo hi]: [lo <= c && c <= hi], both ends included. *)

  val like : ('r, string) col -> string -> 'r cond

  val ilike : ('r, string) col -> string -> 'r cond
  (** [like] ignoring case: [ILIKE] in Postgres, [LOWER(c) LIKE LOWER(?)]
      elsewhere. *)

  val like_opt : ('r, string option) col -> string -> 'r cond
  (** {!like} on a nullable column. A row whose column is NULL doesn't match. *)

  val ilike_opt : ('r, string option) col -> string -> 'r cond

  val is_null : ('r, 'a option) col -> 'r cond
  (** The same as [c = None]. *)

  val is_not_null : ('r, 'a option) col -> 'r cond

  (** A nullable column against a plain value: [Books.pages >? 300] instead of
      [Books.pages > Some 300]. As in SQL, a row whose column is NULL matches
      none of these, [<>?] included. *)

  val ( =? ) : ('r, 'a option) col -> 'a -> 'r cond
  val ( <>? ) : ('r, 'a option) col -> 'a -> 'r cond
  val ( >? ) : ('r, 'a option) col -> 'a -> 'r cond
  val ( >=? ) : ('r, 'a option) col -> 'a -> 'r cond
  val ( <? ) : ('r, 'a option) col -> 'a -> 'r cond
  val ( <=? ) : ('r, 'a option) col -> 'a -> 'r cond

  val ( := ) : ('r, 'a) col -> 'a -> 'r assign
  (** [Ops.[ Users.name := "Ann"; Users.age := 35 ]] *)
end

val some : ('r, 'a) col -> ('r, 'a option) col
(** A column seen as nullable, to join a nullable column on a non-null one:
    [Ops.(Actions.approved_by == some Users.id)]. *)

(** {1 Queries} *)

type filtered
(** A query that has been through {!where}. *)

type unfiltered
(** A query straight from {!from}. *)

type plain
(** A query on one table: it can be selected from, joined, updated and deleted
    from. *)

type joined
(** A query with a {!join}: it can be filtered, ordered and read, nothing more.
*)

type ('r, 'a, 'w) query
(** A query on a table of ['r] rows returning ['a] values.

    ['w] is two facts the type checker keeps, as a pair ['f * 'j]: whether
    {!where} was called ({!filtered} or {!unfiltered}), and whether the query
    has a join ({!plain} or {!joined}). They make these mistakes type errors
    instead of runtime ones:

    - {!S.update} or {!S.delete} without a [where] (to act on every row, say so:
      [where all_rows]);
    - {!select} after a {!join};
    - a second {!join};
    - {!S.update} or {!S.delete} through a join.

    The type error names the fact: e.g. "Type joined is not compatible with type
    plain" for a [select] after a join. *)

type 'r order

val from : 'r table -> ('r, 'r, unfiltered * plain) query
(** Every row, whole. *)

val where : 'r cond -> ('r, 'a, _ * 'j) query -> ('r, 'a, filtered * 'j) query
(** Adds a condition; several [where]s are joined with AND. On a joined query it
    still takes the left table's columns (see {!section-joins}). *)

val where_opt :
  'r cond option -> ('r, 'a, 'f * 'j) query -> ('r, 'a, 'f * 'j) query
(** Adds a condition when [Some c], and leaves the query unchanged when [None].
    Preserves whether the query is {!filtered} so [where_opt None] cannot bypass
    {!S.update} or {!S.delete}'s [where] check. *)

val select :
  ('r, 'b) sel -> ('r, 'a, 'f * plain) query -> ('r, 'b, 'f * plain) query
(** Which columns to read. On a query that will be joined, call it before
    {!join}: it picks the left table's columns. After a join it doesn't
    type-check, since replacing the result would drop the right table. *)

val distinct : ('r, 'a, 'w) query -> ('r, 'a, 'w) query
(** Keeps only distinct result rows ([SELECT DISTINCT]). Applies to {!S.all} and
    {!S.first}; {!S.count} still counts all matching rows. *)

val asc : ('r, _) col -> 'r order
val desc : ('r, _) col -> 'r order

val nulls_first : 'r order -> 'r order
(** Sorts NULL values before non-null values ([NULLS FIRST] on Postgres and
    SQLite; [col IS NULL DESC, col ...] on MySQL). *)

val nulls_last : 'r order -> 'r order
(** Sorts NULL values after non-null values ([NULLS LAST] on Postgres and
    SQLite; [col IS NULL ASC, col ...] on MySQL). *)

val order_by : 'r order list -> ('r, 'a, 'w) query -> ('r, 'a, 'w) query
(** Replaces the order. On a joined query it takes the left table's columns. *)

val limit : int -> ('r, 'a, 'w) query -> ('r, 'a, 'w) query
(** At most [n] rows; on a joined query, [n] joined rows. *)

val offset : int -> ('r, 'a, 'w) query -> ('r, 'a, 'w) query

(** {2:joins Joins}

    {b When to use this.} On Postgres, the usual way to join is SQL, with the
    [qb-ppx] package's [[%sql "SELECT ... JOIN ..."]]: it has none of the limits
    below. This join is for a query whose conditions are decided at run time,
    and for SQLite and MySQL.

    A join reads two tables at once: each row of a query on one table (the
    {e left} table) with the matching rows of another (the {e right} table).
    Rows come back as OCaml pairs.

    {[
    Qb.(
      from Comments.table
      |> left_join Users.table ~on:Ops.(Comments.author_id == Users.id)
      |> where Ops.(Comments.report_date = day)
      |> order_by [ asc Comments.created_at ]
      |> Qb_lwt.all db)
    (* : (Comments.t * Users.t option) list *)
    ]}

    renders

    {v
    SELECT "l"."id", ..., "r"."id", ... FROM "comments" AS "l"
    LEFT JOIN "users" AS "r" ON "l"."author_id" = "r"."id"
    WHERE "l"."report_date" = ? ORDER BY "l"."created_at" ASC
    v}

    {3 A joined query is still a query on the left table}

    Joining doesn't change what the query is about: its conditions and order are
    still written with the left table's own columns, and qb qualifies them ([l.]
    and [r.]) when it makes the SQL. So:

    - {!where} and {!order_by} take the left table's columns,
      {e before or after} the join: both orders give the same SQL.
    - {!limit} and {!offset} work on either side too. They count joined rows:
      with several right rows per left row, [limit 10] is 10 pairs, not 10 left
      rows.
    - {!select} goes {e before} the join. It picks the left table's columns ([s]
      in the result [(s * right row)]); the right table is always read whole.
      After a join, [select] doesn't type-check, since replacing the result
      would drop the right table.

    {[
    from Runs.table
    |> select [ Runs.id; Runs.report_date ] (* skip the big columns *)
    |> join Stores.table ~on:Ops.(Runs.distribution_store_id == Stores.id)
    |> where Ops.(Runs.superseded_at = None)
    (* rows : Row.[ id; date ] * Stores.t *)
    ]}

    {3 The ON, and the right table's condition}

    [~on] says which rows go together. Write it with {!Ops.( == )}, each side
    its own table's column, left first; join several with [&&]. A nullable
    column on one side is compared through {!some}:
    [Ops.(Actions.approved_by == some Users.id)].

    A condition on the right table alone is given as [~where] on the join:

    {[
    |> join Stores.table
         ~on:Ops.(Runs.distribution_store_id == Stores.id)
         ~where:Ops.(Stores.organization_id = Some org)
    ]}

    qb adds it to the ON. For an inner join that is the same as filtering the
    result. For a {!left_join} it says which right rows may match, and every
    left row is still returned, with [None] when none did: [~where] on a left
    join never drops left rows.

    {3 Inner or left}

    - {!join} keeps only the pairs that match: [(s * 'b)] rows.
    - {!left_join} keeps every left row, with [Some] right row or [None]:
      [(s * 'b option)] rows. Use it when the right row may be missing (a
      deleted user, a nullable foreign key).

    Each table gets its own alias in the SQL, so a table can be joined with
    itself:
    [from Users.table |> join Users.table ~on:Ops.(Users.manager_id == some
     Users.id)] pairs each user with their manager.

    {3 What joins don't do}

    These are left to statements written as SQL (see {!type-stmt}):

    - a condition or an order on the right table's columns after the join, or
      one that compares columns of both tables beyond the ON;
    - "left rows with no match" (an anti-join);
    - three tables or more: a query takes one join, and a second doesn't
      type-check;
    - updating or deleting through a join, which doesn't type-check either.

    Those type errors come from the query's ['w] (see {!type-query}): a joined
    query is [_ * joined], and [select], [join], [update] and [delete] want
    [_ * plain]. *)

val join :
  ?where:'b cond ->
  'b table ->
  on:('a * 'b) cond ->
  ('a, 's, 'f * plain) query ->
  ('a, 's * 'b, 'f * joined) query
(** [q |> join right ~on ?where]: each row of [q] paired with each row of
    [right] matching [on] and [?where] (an inner join). Rows of [q] with no
    match are left out. See {!section-joins}. *)

val left_join :
  ?where:'b cond ->
  'b table ->
  on:('a * 'b) cond ->
  ('a, 's, 'f * plain) query ->
  ('a, 's * 'b option, 'f * joined) query
(** [q |> left_join right ~on ?where]: every row of [q], paired with each
    matching row of [right], or with [None] when no row of [right] matches [on]
    and [?where]. [?where] never drops rows of [q]. See {!section-joins}. *)

val to_sql : ?dialect:dialect -> ('r, 'a, 'w) query -> string
(** The SELECT statement, with [?] placeholders. [dialect] defaults to
    [Postgres]. *)

(** The text of the other statements {!S} runs, with [?] placeholders, for
    logging or checking them (the [qb-pg-parse] package checks them with
    Postgres's parser). [dialect] defaults to [Postgres]. *)

val count_sql : ?dialect:dialect -> ('r, _, _) query -> string
val insert_sql : ?dialect:dialect -> 'r table -> 'r -> string

val insert_returning_sql :
  ?dialect:dialect -> 'r table -> 'r assign list -> ('r, _) sel -> string

val insert_values_sql : ?dialect:dialect -> 'r values -> string

val insert_many_values_sql : ?dialect:dialect -> 'r values list -> string
(** Raises [Invalid_argument] with no rows; {!S.insert_many_values} runs nothing
    then. *)

val upsert_sql : ?dialect:dialect -> 'r on_conflict -> 'r values -> string

val upsert_many_sql :
  ?dialect:dialect -> 'r on_conflict -> 'r values list -> string
(** Raises [Invalid_argument] with no rows; {!S.upsert_many} runs nothing then.
*)

val insert_many_sql : ?dialect:dialect -> 'r table -> 'r list -> string
(** Raises [Invalid_argument] with no rows; {!S.insert_many} runs nothing then.
*)

val update_sql :
  ?dialect:dialect ->
  'r assign list ->
  ('r, _, filtered * plain) query ->
  string
(** Raises [Invalid_argument] with no assignments; {!S.update} runs nothing
    then. *)

val delete_sql : ?dialect:dialect -> ('r, _, filtered * plain) query -> string

val update_returning_sql :
  ?dialect:dialect ->
  'r assign list ->
  ('r, _, filtered * plain) query ->
  ('r, _) sel ->
  string

val delete_returning_sql :
  ?dialect:dialect -> ('r, _, filtered * plain) query -> ('r, _) sel -> string

(** {1 Multi-table query builder}

    A scoped, composable query builder for arbitrary multi-table joins,
    aggregates, [GROUP BY] / [HAVING], correlated subqueries, CTEs ([WITH]), set
    operations ([UNION] / [INTERSECT] / [EXCEPT]), row locking, and multi-table
    writes.

    Each {!Query.from} or join binds a handle (['r ref] for inner/base tables,
    ['r opt_ref] for outer joins). Access columns with [u.@(Users.name)] on a
    non-null handle, [c.@?(Comments.body)] on an outer-joined handle (wrapping a
    non-null column in [option]), or [c.@??(Comments.deleted_at)] for a column
    that is already nullable (avoiding [option option]).

    {[
    let q =
      Qb.Query.(
        build
          (let* u = from Users.table in
           let* c =
             left_join Comments.table ~on:(fun c ->
                 Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id)))
           in
           let* () = where Expr.Ops.(u.@(Users.age) > Expr.int 18) in
           let* () = order_by [ asc u.@(Users.name) ] in
           select
             (let+ name = expr u.@(Users.name)
              and+ body = expr c.@?(Comments.body) in
              (name, body))))
    ]} *)
module Query : sig
  type 'a t
  (** A multi-table [SELECT] query returning ['a] rows. Run with {!S.query_all},
      {!S.query_first}, {!S.query_one}, or {!S.query_count}. *)

  type 'r ref
  (** A non-null table, CTE, or derived-table handle in scope. *)

  type 'r opt_ref
  (** A nullable handle from {!left_join}, {!right_join}, or {!full_join}. *)

  type 's cte
  (** A CTE declared with {!with_cte}, usable in {!from_cte}, {!join_cte}, and
      {!left_join_cte}. *)

  val ( .@() ) : 'r ref -> ('r, 'a) col -> 'a Expr.t
  val ( .@?() ) : 'r opt_ref -> ('r, 'a) col -> 'a option Expr.t
  val ( .@??() ) : 'r opt_ref -> ('r, 'a option) col -> 'a option Expr.t

  val to_opt : 'r ref -> 'r opt_ref
  (** Views a non-null handle as nullable, e.g. to downgrade additional
      previously bound left handles after a {!right_join} or {!full_join}. *)

  (** {2 Projections} *)

  type 'a proj

  val expr : 'a Expr.t -> 'a proj
  val whole : 'r ref -> 'r proj
  val whole_opt : 'r opt_ref -> 'r option proj
  val unit : unit proj
  val map : ('a -> 'b) -> 'a proj -> 'b proj
  val both : 'a proj -> 'b proj -> ('a * 'b) proj
  val ( let+ ) : 'a proj -> ('a -> 'b) -> 'b proj
  val ( and+ ) : 'a proj -> 'b proj -> ('a * 'b) proj

  (** {2 Named projections for CTEs and derived tables} *)

  type ('s, 'a) named
  type ('s, 'exports) sub_query

  val field : string -> 'a Expr.t -> ('s, ('s, 'a) col) named

  module Named : sig
    val map : ('a -> 'b) -> ('s, 'a) named -> ('s, 'b) named
    val both : ('s, 'a) named -> ('s, 'b) named -> ('s, 'a * 'b) named
    val ( let+ ) : ('s, 'a) named -> ('a -> 'b) -> ('s, 'b) named
    val ( and+ ) : ('s, 'a) named -> ('s, 'b) named -> ('s, 'a * 'b) named
  end

  (** {2 Ordering} *)

  type order = Expr.order

  val asc : _ Expr.t -> order
  val desc : _ Expr.t -> order
  val nulls_first : order -> order
  val nulls_last : order -> order

  (** {2 Building queries} *)

  type 'a build

  val return : 'a -> 'a build
  val ( let* ) : 'a build -> ('a -> 'b build) -> 'b build
  val from : 'r table -> 'r ref build
  val join : 'r table -> on:('r ref -> bool Expr.t) -> 'r ref build
  val left_join : 'r table -> on:('r ref -> bool Expr.t) -> 'r opt_ref build

  val right_join :
    'a ref ->
    'b table ->
    on:('b ref -> bool Expr.t) ->
    ('a opt_ref * 'b ref) build
  (** [let* u, c = right_join u Comments.table ~on:(fun c -> ...)] rebinds the
      left handle as nullable (['a opt_ref]) and binds the right table as
      non-null (['b ref]). If other left handles were bound earlier, downgrade
      them with {!to_opt}. *)

  val full_join :
    'a ref ->
    'b table ->
    on:('b ref -> bool Expr.t) ->
    ('a opt_ref * 'b opt_ref) build
  (** [let* u, c = full_join u Comments.table ~on:(fun c -> ...)] rebinds the
      left handle as nullable (['a opt_ref]) and binds the right table as
      nullable (['b opt_ref]). *)

  val cross_join : 'r table -> 'r ref build

  (** {3 Relation-driven joins} *)

  val join_many : 'a ref -> ('a, 'b) Rel.has_many -> 'b ref build
  val left_join_many : 'a ref -> ('a, 'b) Rel.has_many -> 'b opt_ref build

  val left_join_many_opt :
    'a opt_ref -> ('a, 'b) Rel.has_many -> 'b opt_ref build

  val join_one : 'a ref -> ('a, 'b) Rel.belongs_to -> 'b ref build
  val left_join_one : 'a ref -> ('a, 'b) Rel.belongs_to -> 'b opt_ref build

  val left_join_one_opt :
    'a opt_ref -> ('a, 'b) Rel.belongs_to -> 'b opt_ref build

  val join_has_one : 'a ref -> ('a, 'b) Rel.has_one -> 'b ref build
  val left_join_has_one : 'a ref -> ('a, 'b) Rel.has_one -> 'b opt_ref build

  val left_join_has_one_opt :
    'a opt_ref -> ('a, 'b) Rel.has_one -> 'b opt_ref build

  val join_m2m : 'a ref -> ('a, 'b) Rel.many_to_many -> 'b ref build
  val left_join_m2m : 'a ref -> ('a, 'b) Rel.many_to_many -> 'b opt_ref build

  val left_join_m2m_opt :
    'a opt_ref -> ('a, 'b) Rel.many_to_many -> 'b opt_ref build

  (** {3 CTEs and derived tables} *)

  val select_named : ('s, 'exports) named -> ('s, 'exports) sub_query build

  val with_cte :
    string -> ('s, 'exports) sub_query build -> ('s cte * 'exports) build

  val with_recursive_cte :
    ?union_all:bool ->
    string ->
    base:('s, 'exports) sub_query build ->
    step:('s cte -> 'exports -> ('s, _) sub_query build) ->
    ('s cte * 'exports) build
  (** [WITH RECURSIVE name AS (base UNION [ALL] step)]. [?union_all] defaults to
      [true]. *)

  val from_cte : 's cte -> 's ref build
  val join_cte : 's cte -> on:('s ref -> bool Expr.t) -> 's ref build
  val left_join_cte : 's cte -> on:('s ref -> bool Expr.t) -> 's opt_ref build

  val from_subquery :
    ('s, 'exports) sub_query build -> ('s ref * 'exports) build

  val join_subquery :
    ('s, 'exports) sub_query build ->
    on:('s ref -> 'exports -> bool Expr.t) ->
    ('s ref * 'exports) build

  val left_join_subquery :
    ('s, 'exports) sub_query build ->
    on:('s ref -> 'exports -> bool Expr.t) ->
    ('s opt_ref * 'exports) build

  (** {3 Clauses} *)

  val where : bool Expr.t -> unit build
  val where_opt : bool Expr.t option -> unit build

  val when_ : bool -> (unit -> unit build) -> unit build
  (** Runs a builder clause conditionally, for dynamic queries. *)

  val opt : 'a option -> ('a -> unit build) -> unit build
  (** Runs a builder clause when [Some x], leaving the query unchanged on
      [None]. *)

  val distinct : unit -> unit build

  val distinct_on : Expr.any list -> unit build
  (** Postgres [SELECT DISTINCT ON (expr, ...)]. *)

  val group_by : Expr.any list -> unit build
  val having : bool Expr.t -> unit build
  val having_opt : bool Expr.t option -> unit build
  val order_by : order list -> unit build
  val limit : int -> unit build
  val offset : int -> unit build
  val for_update : ?nowait:bool -> ?skip_locked:bool -> unit -> unit build
  val for_share : ?nowait:bool -> ?skip_locked:bool -> unit -> unit build
  val select : 'a proj -> 'a t build

  val select_one : 'a Expr.t -> 'a t build
  (** Selects a single expression; required for {!in_query}, {!not_in_query},
      and {!scalar} subqueries. *)

  val build : 'a t build -> 'a t

  val sub : unit build -> unit t
  (** Shorthand [SELECT 1 FROM ...] subquery for {!exists} / {!not_exists}. *)

  (** {2 Set operations} *)

  val union : 'a t -> 'a t -> 'a t
  val union_all : 'a t -> 'a t -> 'a t
  val intersect : 'a t -> 'a t -> 'a t
  val except : 'a t -> 'a t -> 'a t
  val with_limit : int -> 'a t -> 'a t
  val with_offset : int -> 'a t -> 'a t

  (** {2 Subquery expressions} *)

  val exists : _ t -> bool Expr.t
  val not_exists : _ t -> bool Expr.t
  val in_query : 'a Expr.t -> 'a t -> bool Expr.t
  val not_in_query : 'a Expr.t -> 'a t -> bool Expr.t
  val scalar : 'a t -> 'a option Expr.t
  val scalar_opt : 'a option t -> 'a option Expr.t

  (** {2 Multi-table writes} *)

  type 'r update_assign
  type 'r update_plan
  type 'r delete_plan

  val set : ('r, 'a) col -> 'a Expr.t -> 'r update_assign
  val set_val : ('r, 'a) col -> 'a -> 'r update_assign

  val update_from :
    ?all_rows:bool ->
    'r table ->
    ('r ref -> 'r update_assign list build) ->
    'r update_plan
  (** [UPDATE target AS t0 SET ... [FROM ...] WHERE ...]. Requires at least one
      {!where} unless [~all_rows:true] is passed. *)

  val delete_using :
    ?all_rows:bool -> 'r table -> ('r ref -> unit build) -> 'r delete_plan
  (** [DELETE FROM target AS t0 [USING ...] WHERE ...]. Requires at least one
      {!where} unless [~all_rows:true] is passed. *)

  (** {2 SQL rendering} *)

  val to_sql : ?dialect:dialect -> _ t -> string
  val count_sql : ?dialect:dialect -> _ t -> string

  val insert_select_sql :
    ?dialect:dialect -> 'r table -> ('r, 'a) sel -> 'a t -> string

  val update_from_sql : ?dialect:dialect -> _ update_plan -> string

  val update_from_returning_sql :
    ?dialect:dialect -> 'r update_plan -> ('r, _) sel -> string

  val delete_using_sql : ?dialect:dialect -> _ delete_plan -> string

  val delete_using_returning_sql :
    ?dialect:dialect -> 'r delete_plan -> ('r, _) sel -> string
end

(** {1 Statements written as SQL}

    For what the builder doesn't do (three tables, aggregates, a condition
    across a join), write the SQL and say what goes in and what comes out:

    {[
    let by_author name : (int * string) Qb.stmt =
      Qb.Stmt.(
        make
          "SELECT c.id, c.body FROM comments c JOIN users u ON u.id = \
           c.author_id WHERE u.name = $1"
          [ arg Qb.text name ]
          (both (ret Qb.int) (ret Qb.text)))
        Qb.Blocking.fetch_all db (by_author "Ann")
    ]}

    The [qb-pg-parse] package writes these from [.sql] files, working out the
    types from your schema. *)

type 'a stmt
(** A statement written as SQL, with its parameters, whose rows are read as
    ['a]. Run it with {!S.fetch_all}, {!S.fetch_opt}, {!S.fetch_one} or
    {!S.execute}. *)

module Stmt : sig
  type arg
  (** A parameter's value and how to send it. *)

  val arg : 'a ty -> 'a -> arg

  val arg_list : 'a ty -> 'a list -> arg
  (** A list of values for one [$n], written [x IN ($1)]: the list is spelled
      out when the statement runs, as [x IN ($1, $2, $3)], and the parameters
      after it renumbered. An empty list makes [IN] false and [NOT IN] true. The
      SQL must use [$n] placeholders, not [?]. *)

  type 'a row
  (** How to read a result row as an ['a]. Columns are read in order. *)

  val ret : 'a ty -> 'a row
  (** One column. *)

  val whole : 'r table -> 'r row
  (** Every column of a table, in the order the table lists them: what
      [SELECT t.*] returns. *)

  val opt : 'a row -> 'a option row
  (** [None] when every column is NULL: the missing side of a left join. *)

  val unit : unit row
  (** No columns, for a statement that returns no rows. *)

  val map : ('a -> 'b) -> 'a row -> 'b row
  val both : 'a row -> 'b row -> ('a * 'b) row
  val ( let+ ) : 'a row -> ('a -> 'b) -> 'b row
  val ( and+ ) : 'a row -> 'b row -> ('a * 'b) row

  val make : ?name:string -> string -> arg list -> 'a row -> 'a stmt
  (** [make sql args row]. Parameters are written [$1], [$2], ... (or all as
      [?]), and [args] gives them in order. The text is sent as it is, whatever
      the connection's dialect.

      Nothing inside a string, a quoted name, a comment or a [$$]-quoted body is
      a parameter. With [$n] parameters, a [?] is left alone, so Postgres's [?]
      operators on JSON can be written. *)

  val sql : _ stmt -> string
  (** The text as given to {!make}. *)
end

val stmt_name : _ stmt -> string option
(** The [?name] given to {!Stmt.make}: what the statement is called in logs and
    traces. [qb-pg-parse queries] names each statement after its [-- name:]. *)

val stmt_sql : ?dialect:dialect -> _ stmt -> string
(** The text a statement is sent as, with its lists spelled out. [dialect]
    defaults to [Postgres]. *)

(** {1 Errors}

    Running a query gives Caqti's errors. These say whether one is a constraint
    the database refused, without matching on the driver's message:

    {[
    match Qb.Blocking.insert_values db (Users.values ~email ()) with
    | Ok user -> Ok user
    | Error e when Qb.is_unique_violation e -> Error `Email_taken
    | Error e -> Error (`Db e)
    ]} *)

type violation = [ `Unique | `Foreign_key | `Not_null | `Check | `Other ]

val violation :
  [> `Request_failed of Caqti_error.query_error
  | `Response_failed of Caqti_error.query_error ] ->
  violation option
(** The constraint an error reports, or [None] when it is some other error.
    [`Other] is a constraint Caqti doesn't classify: an exclusion, or in SQLite
    a duplicate primary key (a [UNIQUE] column there is [`Unique]). *)

val is_unique_violation :
  [> `Request_failed of Caqti_error.query_error
  | `Response_failed of Caqti_error.query_error ] ->
  bool

(** {1 Changesets}

    A changeset is a write that hasn't happened yet: the row it starts from (if
    any), the changes to make, and what is wrong with them. Build one from
    untrusted input (a form, a JSON body), check it, and run it; you get back
    either the row as stored or the changeset with its errors, ready to show
    next to the fields that caused them.

    {[
    let user_changeset cs params =
      Qb.Changeset.(
        cs
        |> cast params [ field Users.name; field Users.age; field Users.email ]
        |> validate_required [ field Users.name; field Users.age ]
        |> validate Users.name [ length ~min:2 ~max:80 () ]
        |> validate Users.age [ range ~ge:0 ~lt:150 () ]
        |> validate_opt Users.email [ format (String.contains '@') ]
        |> unique_constraint Users.email)

    let sign_up db params =
      match
        Qb.Blocking.insert_changeset db
          (user_changeset (Qb.Changeset.for_insert Users.table) params)
      with
      | Ok user -> `Created user
      | Error (`Invalid cs) -> `Show_form_again cs
      | Error (#Caqti_error.t as e) -> failwith (Caqti_error.show e)
    ]}

    {3 What is checked where}

    - {!Changeset.cast} reads text into typed values. Text that isn't one (["x"]
      for an [int]) is a [`Cast] error, ["is invalid"]. Blank text clears a
      nullable column ([None]) and is a [`Required] error on any other column,
      text included.
    - {!Changeset.validate_required} and the [validate] functions run in OCaml,
      before anything is sent. Like Ecto, [validate] looks at the change only: a
      field the changeset doesn't change, or that failed to cast, isn't checked
      again.
    - Constraints ({!Changeset.unique_constraint} and the like) are checked by
      the database. Declaring one says which error the database's refusal
      becomes; a refusal no declared constraint matches is returned as the Caqti
      error it was.

    Messages are Ecto's, in English; match on {!Changeset.type-kind} to word
    them your own way. *)

module Changeset : sig
  type 'r t
  (** A write to a table of ['r] rows. *)

  val for_insert : 'r table -> 'r t
  (** A new row. Run it with {!S.insert_changeset}. *)

  val for_update : 'r table -> 'r -> 'r t
  (** Changes to a row read before. Run it with {!S.update_changeset}, which
      finds the row by the table's primary key (one column). *)

  (** {2 Changes} *)

  type 'r field
  (** A column of any type, to list several of different types:
      [[ field Users.name; field Users.age ]]. *)

  val field : ('r, 'a) col -> 'r field

  val cast : (string * string) list -> 'r field list -> 'r t -> 'r t
  (** [cast params fields cs] reads each of [fields] from [params], keyed by
      column name, as a form or query string gives them. A field missing from
      [params] is left as it is; only the [fields] listed are read, so other
      columns can't be set from outside. Raises [Invalid_argument] for a column
      whose type has no [of_string] (see {!val-ty}). *)

  val put : ('r, 'a) col -> 'a -> 'r t -> 'r t
  (** A change made by the program, trusted and already typed. Setting a column
      of {!for_update}'s row to the value it has is no change. *)

  val update_change : ('r, 'a) col -> ('a -> 'a) -> 'r t -> 'r t
  (** Transforms a column's pending change in place (e.g. [String.trim] or
      [String.lowercase_ascii] before validation). Does nothing when the column
      has no change or already has an error. *)

  val update_change_opt : ('r, 'a option) col -> ('a -> 'a) -> 'r t -> 'r t
  (** {!update_change} for a nullable column: leaves [None] alone. *)

  val get : ('r, 'a) col -> 'r t -> 'a option
  (** The column's value after the changes: the change, or else the row's value,
      or [None] when a new row doesn't have it yet. *)

  val get_change : ('r, 'a) col -> 'r t -> 'a option
  (** The change to the column, if there is one. *)

  val changes : 'r t -> 'r assign list

  val data : 'r t -> 'r option
  (** The row {!for_update} was given. *)

  (** {2 Validations} *)

  val validate_required : 'r field list -> 'r t -> 'r t
  (** Each field must have a value after the changes, and not a blank one
      ([None], or text that is only whitespace). Error: [`Required],
      ["can't be blank"]. *)

  type 'a check
  (** A rule a value must follow. *)

  val validate : ('r, 'a) col -> 'a check list -> 'r t -> 'r t
  (** Checks the column's change against each rule; every rule broken is an
      error. *)

  val validate_opt : ('r, 'a option) col -> 'a check list -> 'r t -> 'r t
  (** {!validate} for a nullable column: a change to [None] isn't checked. *)

  val length : ?min:int -> ?max:int -> ?is:int -> unit -> string check
  (** Counted in characters (UTF-8), not bytes. [`Length]. *)

  val range : ?gt:'a -> ?ge:'a -> ?lt:'a -> ?le:'a -> unit -> 'a check
  (** Bounds, compared with [compare]: [range ~ge:0 ~lt:150 ()]. [`Range]. *)

  val one_of : 'a list -> 'a check
  (** [`Inclusion], ["is invalid"]. *)

  val none_of : 'a list -> 'a check
  (** [`Exclusion], ["is reserved"]. *)

  val format : ?message:string -> (string -> bool) -> string check
  (** The text must satisfy the predicate (a regular expression of your own
      choosing, say). [`Format], ["has invalid format"]. *)

  val check : string -> ('a -> bool) -> 'a check
  (** Any rule, with its message. [`Custom]. *)

  (** {2 Constraints} *)

  val unique_constraint :
    ?name:string -> ?message:string -> ('r, 'a) col -> 'r t -> 'r t
  (** If the database refuses the write as a duplicate, the error goes on this
      column: [`Unique], ["has already been taken"].

      [?name] is the constraint's name, by default Postgres's for a [UNIQUE] on
      one column: [<table>_<column>_key]. Give it for a constraint over several
      columns, or one named otherwise. In SQLite, whose message gives columns
      and not names, the column is matched instead; in MySQL, the index name,
      which is the column's own when not given. *)

  val foreign_key_constraint :
    ?name:string -> ?message:string -> ('r, 'a) col -> 'r t -> 'r t
  (** [`Foreign_key], ["does not exist"]. Default name: [<table>_<column>_fkey].
      SQLite doesn't say which foreign key failed, so there it matches only when
      the changeset declares exactly one. *)

  val check_constraint :
    ?name:string -> ?message:string -> ('r, 'a) col -> 'r t -> 'r t
  (** [`Check], ["is invalid"]. Default name: [<table>_<column>_check]. *)

  (** {2 Errors} *)

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
  (** Which rule an error comes from. *)

  type error = {
    field : string;  (** The column's name. *)
    message : string;
    kind : kind;
  }

  val add_error : ?kind:kind -> ('r, 'a) col -> string -> 'r t -> 'r t
  (** An error of your own. [kind] defaults to [`Custom]. *)

  val is_valid : 'r t -> bool
  (** No errors. A changeset with errors is never sent to the database. *)

  val errors : 'r t -> error list
  (** In the order they were found. *)

  val errors_on : ('r, 'a) col -> 'r t -> string list
  (** The messages for one column. *)

  val input : ('r, 'a) col -> 'r t -> string
  (** What a form should show in the column's field: the text that was cast, as
      typed (so a wrong value can be corrected), or else the value as text, or
      [""]. *)
end

(** {1 Running on Caqti} *)

val src : Logs.src
(** Every statement is logged at debug level on this source once it has run: its
    text, its parameters, what came of it and how long it took.

    {v
    SELECT "id" FROM "users" WHERE "age" > ? [18] -> 2 rows, 412us
    UPDATE "users" SET "name" = ? WHERE "id" = ? ["Ann", 7] -> 1 row changed, 1.03ms
    v}

    Show them with [Logs.Src.set_level Qb.src (Some Logs.Debug)] and a reporter.
    The parameters are your data, passwords and all: keep this level for
    development. At any other level nothing is timed or formatted. A count of
    rows changed is given by the functions that return one ({!S.update_count}
    and the like); the others say [done]. *)

(** Statements are prepared once per connection and reused: qb caches each Caqti
    request by its SQL text, reusing it only when its parameter and row types
    match. The cache holds up to 512 statements; past that, new statements run
    unprepared. A statement whose text depends on the length of a list ([is_in],
    [insert_many], a list argument) is never kept: there is no end of those. The
    cache is safe to share across threads and domains. *)

(** The queries, for one kind of Caqti connection. *)
module type S = sig
  type 'a fiber
  (** ['a] for blocking connections, ['a Lwt.t] for Lwt. *)

  type db
  (** A Caqti connection, e.g. [Caqti_blocking.connection]. *)

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
  (** Rows matching the query's [where]. *)

  val insert :
    db ->
    'r table ->
    'r ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Inserts a whole row. *)

  val insert_returning :
    db ->
    'r table ->
    'r assign list ->
    ('r, 'a) sel ->
    ('a, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Inserts some columns (e.g. leaving out an auto-increment id) and reads
      back what the database filled in. With no assignments, every column takes
      its default. Not supported by MySQL. *)

  val insert_values :
    db -> 'r values -> ('r, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Inserts a new row and returns it whole, as the database stored it: ids,
      defaults and all. Not supported by MySQL. *)

  val insert_many_values :
    db ->
    'r values list ->
    ('r list, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Inserts several new rows in one statement, leaving out columns the
      database fills in (identities, defaults, NULL), and returns the stored
      rows. With no rows it does nothing and returns [[]]. On Postgres, rows may
      set different subsets of columns (omitted ones use [DEFAULT]); on SQLite,
      every row must set the same columns. Not supported by MySQL. *)

  val upsert :
    db ->
    'r on_conflict ->
    'r values ->
    ('r option, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Inserts a new row with an [ON CONFLICT] clause, returning [Some row] when
      the row was inserted or updated, or [None] when a conflict was skipped by
      {!On_conflict.do_nothing} or filtered out by {!On_conflict.do_update}'s
      [?where]. Not supported by MySQL. *)

  val upsert_many :
    db ->
    'r on_conflict ->
    'r values list ->
    ('r list, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Batch {!upsert} in one statement, returning all inserted or updated rows.
      With no rows it does nothing and returns [[]]. Not supported by MySQL. *)

  val update :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Updates the rows the query's [where] matches. With no assignments it does
      nothing. *)

  val delete :
    db ->
    ('r, _, filtered * plain) query ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Deletes the rows the query's [where] matches. *)

  val insert_many :
    db ->
    'r table ->
    'r list ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Inserts whole rows in one statement; with no rows it does nothing. The
      statement has a parameter per column per row, and databases cap those
      (65535 in Postgres, 32766 in SQLite): split a very long list. *)

  val update_count :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber
  (** {!update}, returning how many rows it changed: [0] says no row matched. *)

  val delete_count :
    db ->
    ('r, _, filtered * plain) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber
  (** {!delete}, returning how many rows it removed. *)

  val update_returning :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Updates the matching rows and reads back the columns asked for, as they
      now are: [update_returning db assigns q [ Users.id; Users.name ]]. Raises
      [Invalid_argument] with no assignments. Not supported by MySQL. *)

  val delete_returning :
    db ->
    ('r, _, filtered * plain) query ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Deletes the matching rows and returns what they held. Not supported by
      MySQL. *)

  val transaction :
    db ->
    (unit -> ('a, 'e) result fiber) ->
    ('a, ([> Caqti_error.transact ] as 'e)) result fiber
  (** [transaction db f] runs [f] in a transaction, committed when [f] returns
      [Ok] and rolled back when it returns [Error] or raises. Run [f]'s queries
      on the same [db].

      Nested [transaction] calls on the same [db] use SQL [SAVEPOINT] /
      [RELEASE SAVEPOINT] / [ROLLBACK TO SAVEPOINT] in [Qb.Blocking], [Qb_lwt]
      and any [Qb.Make_guarded], so transactional helpers compose cleanly: an
      inner [Error] rolls back only the inner savepoint (leaving the outer
      transaction usable if it handles the error), while an uncaught exception
      unwinds both. A plain [Qb.Make] delegates directly to Caqti and doesn't
      nest. *)

  val create_table :
    db -> 'r table -> (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  (** {2 Changesets} *)

  val insert_changeset :
    db ->
    'r Changeset.t ->
    ('r, [> Caqti_error.call_or_retrieve | `Invalid of 'r Changeset.t ]) result
    fiber
  (** Inserts the changes of a {!Qb.Changeset.for_insert} and returns the row as
      stored. [`Invalid cs]: the changeset had errors, and nothing was sent; or
      the database refused the row by a constraint the changeset declared, now
      an error on [cs]. Not supported by MySQL. Raises [Invalid_argument] for a
      {!Qb.Changeset.for_update}. *)

  val update_changeset :
    db ->
    'r Changeset.t ->
    ( 'r,
      [> Caqti_error.call_or_retrieve | `Invalid of 'r Changeset.t | `Stale ]
    )
    result
    fiber
  (** Updates the row of a {!Qb.Changeset.for_update}, found by its primary key,
      and returns it as it now is. With no changes, nothing is sent and the row
      comes back as given. [`Invalid cs] as for {!insert_changeset}; [`Stale]:
      the row is no longer in the table. Not supported by MySQL. Raises
      [Invalid_argument] for a {!Qb.Changeset.for_insert}, or for a table
      without exactly one primary key column. *)

  (** {2 Statements written as SQL} *)

  val fetch_all :
    db -> 'a stmt -> ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val fetch_opt :
    db -> 'a stmt -> ('a option, [> Caqti_error.call_or_retrieve ]) result fiber
  (** The row, or [None]. More than one row is an error. *)

  val fetch_one :
    db -> 'a stmt -> ('a, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Exactly one row; none, or several, is an error. *)

  val execute :
    db -> unit stmt -> (unit, [> Caqti_error.call_or_retrieve ]) result fiber
  (** Runs a statement that returns no rows. *)

  val execute_count :
    db -> unit stmt -> (int, [> Caqti_error.call_or_retrieve ]) result fiber
  (** {!execute}, returning how many rows the statement changed. *)

  (** {2 Multi-table queries and relational loading} *)

  val query_all :
    db ->
    'a Query.t ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val query_first :
    db ->
    'a Query.t ->
    ('a option, [> Caqti_error.call_or_retrieve ]) result fiber

  val query_one :
    db -> 'a Query.t -> ('a, [> Caqti_error.call_or_retrieve ]) result fiber

  val query_count :
    db -> _ Query.t -> (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_select :
    db ->
    'r table ->
    ('r, 'a) sel ->
    'a Query.t ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_from :
    db ->
    _ Query.update_plan ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_from_count :
    db ->
    _ Query.update_plan ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_from_returning :
    db ->
    'r Query.update_plan ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_using :
    db ->
    _ Query.delete_plan ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_using_count :
    db ->
    _ Query.delete_plan ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_using_returning :
    db ->
    'r Query.delete_plan ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val load_many :
    ?where:'child cond ->
    ?order_by:'child order list ->
    db ->
    ('parent, 'child) Rel.has_many ->
    'parent list ->
    (('parent * 'child list) list, [> Caqti_error.call_or_retrieve ]) result
    fiber
  (** Batched 1-to-many eager loader: fetches all children for [parents] in a
      single [IN] query (zero queries when [parents] is empty) and pairs each
      parent with its children. *)

  val load_one :
    ?where:'parent cond ->
    db ->
    ('child, 'parent) Rel.belongs_to ->
    'child list ->
    (('child * 'parent option) list, [> Caqti_error.call_or_retrieve ]) result
    fiber
  (** Batched many-to-1 eager loader: fetches referenced parents in a single
      [IN] query and pairs each child with its parent. *)

  val load_has_one :
    ?where:'child cond ->
    db ->
    ('parent, 'child) Rel.has_one ->
    'parent list ->
    (('parent * 'child option) list, [> Caqti_error.call_or_retrieve ]) result
    fiber
  (** Batched 1-to-1 eager loader: fetches child rows in a single [IN] query and
      pairs each parent with its matching child (if any). *)

  val load_many_to_many :
    ?where:('target Query.ref -> bool Expr.t) ->
    ?order_by:('target Query.ref -> Expr.order list) ->
    db ->
    ('source, 'target) Rel.many_to_many ->
    'source list ->
    (('source * 'target list) list, [> Caqti_error.call_or_retrieve ]) result
    fiber
  (** Batched many-to-many eager loader: fetches target rows through the join
      table in a single [INNER JOIN ... WHERE IN (...)] query and groups them
      per source row. *)
end

(** What qb needs to know about a Caqti backend. *)
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

module Make (B : BACKEND) :
  S with type 'a fiber = 'a B.fiber and type db = (module B.CONNECTION)

(** A backend that can sequence and guard fibers, enabling nested savepoint
    transactions. *)
module type GUARDED_BACKEND = sig
  include BACKEND

  val bind : 'a fiber -> ('a -> 'b fiber) -> 'b fiber
  val catch : (unit -> 'a fiber) -> (exn -> 'a fiber) -> 'a fiber

  val finally : (unit -> 'a fiber) -> (unit -> unit) -> 'a fiber
  (** [finally f after] runs [f ()], then [after ()] once its fiber has a result
      or has failed, and gives [f ()]'s outcome. *)
end

(** {!Make}, whose {!S.transaction} also supports nested savepoints: {!Blocking}
    and [Qb_lwt] are made with it. *)
module Make_guarded (B : GUARDED_BACKEND) :
  S with type 'a fiber = 'a B.fiber and type db = (module B.CONNECTION)

(** Queries on [Caqti_blocking] connections. *)
module Blocking :
  S with type 'a fiber = 'a and type db = Caqti_blocking.connection
