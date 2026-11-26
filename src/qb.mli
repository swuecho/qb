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

type 'r values
(** A new row of a table of ['r] rows, given as some of its columns; the
    database fills in the rest (identities, defaults, NULL). Run it with
    {!S.insert_values}, which returns the whole row as stored. *)

val values : 'r table -> 'r assign list -> 'r values
(** [values Users.table Ops.[ Users.name := "Ann"; Users.age := 34 ]]. Generated
    tables (the [qb-pg-parse] package) have a [values] function that takes each
    column as an argument, required unless the database can fill it in. *)

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

val select :
  ('r, 'b) sel -> ('r, 'a, 'f * plain) query -> ('r, 'b, 'f * plain) query
(** Which columns to read. On a query that will be joined, call it before
    {!join}: it picks the left table's columns. After a join it doesn't
    type-check, since replacing the result would drop the right table. *)

val asc : ('r, _) col -> 'r order
val desc : ('r, _) col -> 'r order

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
(** The [?name] given to {!Stmt.make}: what the statement is called in logs
    and traces. [qb-pg-parse queries] names each statement after its
    [-- name:]. *)

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

    Messages are Ecto's, in English; match on {!Changeset.type-kind} to word them
    your own way. *)

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
    [insert_many], a list argument) is never kept: there is no end of those.
    Don't run queries from several domains at once. *)

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

      Transactions don't nest: the database would take the inner one's commit as
      the outer one's. Calling [transaction] inside [f] on the same [db] raises
      [Invalid_argument] (and so rolls the outer one back) in [Qb.Blocking],
      [Qb_lwt] and any [Qb.Make_guarded]; a plain [Qb.Make] can't tell, so
      don't. *)

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
      [Invalid_argument] for a {!Qb.Changeset.for_insert}, or for a table without
      exactly one primary key column. *)

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

(** A backend that can also run something when a fiber ends, however it ends. *)
module type GUARDED_BACKEND = sig
  include BACKEND

  val finally : (unit -> 'a fiber) -> (unit -> unit) -> 'a fiber
  (** [finally f after] runs [f ()], then [after ()] once its fiber has a result
      or has failed, and gives [f ()]'s outcome. *)
end

(** {!Make}, whose {!S.transaction} also refuses to nest: {!Blocking} and
    [Qb_lwt] are made with it. *)
module Make_guarded (B : GUARDED_BACKEND) :
  S with type 'a fiber = 'a B.fiber and type db = (module B.CONNECTION)

(** Queries on [Caqti_blocking] connections. *)
module Blocking :
  S with type 'a fiber = 'a and type db = Caqti_blocking.connection
