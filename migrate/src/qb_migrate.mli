(** Applies migrations to a Postgres database: the same [migrations/*.sql] files
    [qb-ppx] and [qb-pg-parse gen] take their types from.

    Migrations only go forward. Each is applied once, in order, in a transaction
    of its own, and recorded in a [qb_migrations] table. To undo one, write
    another.

    {[
    let () =
      let db = connect () in
      match
        Result.bind (Qb_migrate.of_dir "migrations") (fun ms ->
            Result.map_error Qb_migrate.error_to_string (Qb_migrate.run db ms))
      with
      | Ok applied -> List.iter (Printf.printf "applied %s\n") applied
      | Error m -> failwith m
    ]}

    The [qb-migrate] command does the same from a shell. *)

type migration = { name : string;  (** The file's name. *) sql : string }

val of_dir : string -> (migration list, string) result
(** The [*.sql] files of a folder, in name order: the order they are applied in,
    and the one [-qb-schema] replays them in. *)

type db = Caqti_blocking.connection

type error =
  [ `Changed of string
    (** An applied migration's text is no longer what was applied. *)
  | `Missing of string  (** An applied migration isn't in the list. *)
  | `Out_of_order of string * string
    (** [(pending, applied)]: a migration not yet applied comes before one that
        is. *)
  | `Invalid of string * string
    (** [(name, message)]: Postgres can't parse the migration. *)
  | `Failed of string * string
    (** [(name, message)]: a statement of the migration failed. *)
  | `Not_verified of string list
    (** {!baseline} only: the differences {!verify} found. *)
  | `Db of Caqti_error.t ]

val error_to_string : error -> string

type status =
  | Applied of Ptime.t
  | Pending
  | Changed of Ptime.t
      (** Applied, and its text has changed since: {!run} refuses to go on. *)
  | Missing of Ptime.t  (** Applied, and no longer in the list. *)

val status : db -> migration list -> ((string * status) list, error) result
(** Every migration, in order, then those applied but missing from the list.
    Writes nothing. *)

val plan :
  ?out_of_order:bool -> db -> migration list -> (string list, error) result
(** Validates the pending migrations as {!run} would (checking for changed,
    missing, out-of-order, or unparsable migrations and stray [BEGIN]/[COMMIT]),
    and returns the names that would be applied in order. Writes nothing and
    does not create [qb_migrations]. *)

val run :
  ?out_of_order:bool -> db -> migration list -> (string list, error) result
(** Applies the pending migrations in order and returns their names. Nothing is
    applied if an applied migration has changed or is missing, if one can't be
    parsed, or (unless [~out_of_order:true]) if a pending one comes before an
    applied one.

    Each migration runs in a transaction, with the record that it was applied,
    so one that fails leaves nothing behind; those before it stay applied. A
    migration whose first line is [-- qb:no-transaction] runs without one, for
    statements such as [CREATE INDEX CONCURRENTLY]: if it fails part-way, what
    it had done stays.

    An advisory lock is held for the run, so two processes starting together
    don't both apply. *)

val verify : db -> migration list -> (string list, error) result
(** What differs between the database and the schema the migrations describe
    (the one [[%sql]] is typed from). [[]] when they agree. Compared:

    - tables and columns one has and the other doesn't;
    - each column's type, with the length of a [VARCHAR(n)] or [CHAR(n)] and the
      precision of a [NUMERIC(p, s)] or [TIMESTAMP(p)] where the migration gives
      one;
    - each column's NOT NULL;
    - whether each column has something to fill it in (a default, an identity, a
      generated column), though not what;
    - each table's primary key.

    Indexes, and unique, foreign-key and check constraints, are not compared:
    the migrations aren't read for those. Tables are looked for in the
    connection's current schema, or the schema a migration names. *)

val baseline : db -> migration list -> (string list, error) result
(** Records every pending migration as applied without running it, for a
    database that already has the schema. Refuses when {!verify} finds a
    difference. Returns the names recorded. *)

val embed : migration list -> string
(** OCaml source defining [migrations : Qb_migrate.migration list], to build the
    files into a program instead of reading them when it starts. *)
