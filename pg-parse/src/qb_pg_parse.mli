(** Postgres's own parser, for qb.

    Binds {{:https://github.com/pganalyze/libpg_query} libpg_query}, the parser
    extracted from the Postgres source (the same one sqlc uses), to:

    - {b check} SQL text, including the statements qb renders ({!Pg.check}),
    - {b read a schema} by replaying CREATE / ALTER / DROP TABLE migrations
      ({!Schema}),
    - {b generate} qb table modules from that schema ({!Codegen}).

    The [qb-pg-parse] command wraps the last two for use in dune rules. *)

(** Parsing and checking SQL. *)
module Pg : sig
  type error = {
    message : string;
        (** Postgres's message, e.g. [syntax error at or near "FROM"] *)
    position : int option;  (** 0-based byte offset into the input, if known *)
  }

  val error_to_string : ?sql:string -> error -> string
  (** With [~sql], the message names the line and points a caret at the error.
  *)

  val parse : string -> (Yojson.Safe.t, error) result
  (** The parse tree as libpg_query's JSON: [{"version": ..., "stmts": [...]}].
      Parameters must be written [$1], [$2], ... *)

  val statements : string -> (Yojson.Safe.t list, error) result
  (** Each statement's node, e.g. [{"SelectStmt": {...}}]. *)

  val split : string -> (string list, error) result
  (** Each statement's own text, in order, without the [;] between them. A
      semicolon inside a string, a comment or a [$$]-quoted body doesn't split.
  *)

  val check : ?caqti:bool -> string -> (unit, error) result
  (** [Ok ()] when Postgres can parse the text. With [~caqti:true], [?]
      placeholders (as qb and Caqti write them) are read as [$1], [$2], ...
      first; error positions still point into the original text.

      Only syntax is checked: tables, columns and types aren't looked up. *)

  val strip_psql_meta : string -> string
  (** Blanks out lines starting with a backslash, the psql meta-commands (such
      as [\restrict]) that pg_dump writes. Byte offsets don't move. *)

  val of_caqti : string -> string * int * (int -> int)
  (** [of_caqti sql] is [(pg_sql, n, back)]: [sql] with its [n] [?] placeholders
      numbered [$1] to [$n], and [back] mapping an offset in [pg_sql] to the one
      in [sql]. *)
end

(** Tables, as built by replaying DDL in order. *)
module Schema : sig
  type pg_type = {
    name : string;
        (** Postgres's internal name, without [pg_catalog]: [int8], [varchar],
            [timestamptz], or e.g. [public.mood] for a user type. [serial] types
            become their integer type, with [generated] set. *)
    mods : int list;  (** e.g. [[10]] for [VARCHAR(10)] *)
    array_dims : int;  (** [1] for [TEXT[]] *)
  }

  type column = {
    name : string;
    ty : pg_type;
    not_null : bool;  (** Also true for primary keys, serials and identities. *)
    generated : bool;
        (** An identity or serial column, or one defaulting to [nextval(...)] (a
            serial as pg_dump writes it). *)
    has_default : bool;
  }

  type table = {
    schema : string option;
    name : string;
    columns : column list;
    primary_key : string list;
  }

  type t = {
    tables : table list;  (** In creation order. *)
    warnings : string list;
        (** Statements or clauses that were understood but not modelled, e.g.
            [DROP CONSTRAINT], or that named a table that didn't exist. *)
  }

  val empty : t

  val of_sql : string -> (t, Pg.error) result
  (** Replays every statement. Understood: CREATE TABLE; ALTER TABLE ADD / DROP
      COLUMN, SET / DROP NOT NULL, ALTER COLUMN TYPE, SET / DROP DEFAULT, ADD /
      DROP IDENTITY, ADD PRIMARY KEY; RENAME of tables and columns; DROP TABLE.
      Anything else (indexes, data, functions) is skipped. *)

  val apply_sql : ?init:t -> string -> (t, Pg.error) result
  (** Replays more statements on top of [init], e.g. one migration file at a
      time. *)

  val find : ?schema:string -> t -> string -> table option

  val type_sql : pg_type -> string
  (** The type as SQL, e.g. [BIGINT], [VARCHAR(10)], [TEXT[]]. *)
end

(** Queries written as SQL, checked against a {!Schema}: the tables and columns
    they name must exist, and the types of their parameters and result columns
    are worked out from the schema. *)
module Query : sig
  type param = {
    name : string;
    ty : Schema.pg_type;
    nullable : bool;  (** Written [:name?]: the value may be NULL. *)
    list : bool;  (** Written [IN :name]: a list of values. *)
    at : int;  (** Where [:name] is first written, as a byte offset. *)
  }

  type column = { name : string; ty : Schema.pg_type; nullable : bool }

  type item =
    | Column of column
    | Whole of { table : Schema.table; nullable : bool }
        (** [t.*]; [nullable] on the outer side of a join. *)

  type t = {
    name : string;
    sql : string;  (** With parameters numbered [$1], [$2], ... *)
    params : param list;  (** In the order of their numbers. *)
    row : item list;
  }

  val of_sql : Schema.t -> string -> (t list, Pg.error) result
  (** Reads a file of queries, each started by a [-- name: get_user] line.
      Parameters are written [:name]; the same name may be used several times.

      A parameter takes its type from the column it is compared with ([=], [<],
      [IN], [LIKE], [BETWEEN], ...), from [LIMIT] / [OFFSET], or from a cast
      ([:name::text]). A result column takes the type of its table's column, and
      can be NULL when the column can or when its table is on the outer side of
      a join. Aggregates, the usual text functions, [now()], conditions, [CASE],
      [COALESCE], constants and casts are understood too; for any other
      expression, write a cast. An INSERT must give every column that is NOT
      NULL with no default.

      Read: a single SELECT, INSERT, UPDATE or DELETE over tables, with joins
      written with ON. Subqueries, WITH, UNION and JOIN ... USING are out of
      scope and rejected with a message: those statements are written with
      [Qb.Stmt.make] or Caqti. Errors point into the text given. *)

  val one : Schema.t -> string -> (t, Pg.error) result
  (** A single query with no name line, as the [qb-ppx] package reads them from
      [[%sql "..."]]. Its {!t.name} is ["query"]. *)
end

(** OCaml source for qb table modules. *)
module Codegen : sig
  val ocaml :
    ?bigint:[ `Int | `Int64 ] -> ?only:string list -> Schema.t -> string
  (** One module per table, named after it ([user_accounts] becomes
      [User_accounts]), holding the row type [t], one value per column and
      [table]. Tables outside [public] go in a module for their schema
      ([gea.reports] becomes [Gea.Reports], with [Qb.table ~schema:"gea"]).
      [~only] keeps just the named tables, given as [name] or [schema.name].

      Postgres types map to qb's when they match exactly ([BIGINT] to [Qb.int],
      [TEXT] to [Qb.text], ...). Others get their own [Qb.ty], so
      [Qb.create_table_sql] gives back the same column types; types with no
      OCaml counterpart, such as [NUMERIC], enums or arrays, are read as text.

      [~bigint:`Int64] reads [BIGINT] (and [BIGSERIAL]) as [int64] with
      [Qb.int64], for code whose ids are already [int64]; the default is [int].
  *)

  val queries :
    ?bigint:[ `Int | `Int64 ] ->
    ?tables:string ->
    Schema.t ->
    Query.t list ->
    (string, string) result
  (** One function per query, named after it, taking each parameter as a
      labelled argument and giving a [Qb.stmt] to run with [fetch_all],
      [fetch_opt] or [fetch_one]. A query with no parameters is a plain value.

      Rows are a record, [<query>_row], with a field per column, or the bare
      value when there is one column. Queries whose columns have the same names
      and types, in the same order, share the first one's record. With
      [~tables:"Schema"], naming the module holding {!ocaml}'s output for the
      same schema, a query whose columns are all [t.*] gives those tables' own
      rows instead: [SELECT c.*, u.* FROM comments c LEFT JOIN users u ...] is a
      [(Schema.Comments.t * Schema.Users.t option) Qb.stmt].

      Each statement is named after its query ({!Qb.stmt_name}). [~bigint] is as
      for {!ocaml}.

      [Error] when two columns of a record would have the same name. *)

  val query_expr :
    ?tables:string ->
    ?rows:[ `Tuple | `Object ] ->
    Schema.t ->
    Query.t ->
    string
  (** The query as one OCaml expression of type [_ Qb.stmt], for use where it is
      written ([qb-ppx]). Each parameter [:name] is read from the variable
      [name]. Rows are tuples with an element per column, or the bare value when
      there is one; with [~tables], a [t.*] is one element, that table's row.

      With [~rows:`Object], a row of two columns or more is an object with a
      method per column, named after the column or its [AS]; a [t.*] read as a
      table's row is named after the table.

      Raises [Invalid_argument] if [~tables] has no module for a table, or if
      two methods of an object would have the same name. *)

  val ident : string -> string
  (** The OCaml identifier used for a SQL name. *)

  val module_name : string -> string
  (** The OCaml module name used for a table or schema. *)

  val quoted : string -> string
  (** An OCaml quoted string literal [{sql|...|sql}] whose delimiter doesn't
      appear in the text. *)
end
