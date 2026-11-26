# Reference: applying migrations with `qb-migrate`

`qb-migrate` runs the `migrations/*.sql` files that `[%sql]` and
`qb-pg-parse gen` take their types from against a Postgres database, and
checks that the database matches them. One folder is then the source of both
the types your program was compiled with and the tables it runs on.

For a walk through, see step 16 of the [tutorial](tutorial.md). This page has
the rules.

- [Commands](#commands)
- [Files](#files)
- [How a run goes](#how-a-run-goes)
- [What stops a run](#what-stops-a-run)
- [verify](#verify)
- [An existing database](#an-existing-database)
- [From a program](#from-a-program)
- [Limits](#limits)

## Commands

| Command | Does |
| --- | --- |
| `qb-migrate new NAME` | Creates the next numbered, empty file: `003_NAME.sql` |
| `qb-migrate up` | Applies the pending files in order, then runs `verify` |
| `qb-migrate status` | Lists each file: `applied`, `pending`, `CHANGED` or `MISSING` |
| `qb-migrate verify` | Compares the database with the migrations |
| `qb-migrate baseline` | Records every file as applied without running it |
| `qb-migrate embed -o FILE` | Writes the files as an OCaml list |

The database is `--db URL` or the `DATABASE_URL` variable. The folder is
`-m DIR`, `migrations` by default.

Each command exits with 0 when all is well. `up` and `verify` exit with 1
when the database differs from the migrations; `status` exits with 1 when a
file is `CHANGED` or `MISSING`, or with `--check` when any file is `pending`
too (handy for CI or deploy readiness checks).

`up` takes three flags: `--out-of-order` (see
[What stops a run](#what-stops-a-run)), `--no-verify`, and `--dry-run`
(validates pending migrations and prints what would run without touching the
database).

## Files

- A migration is a `.sql` file. Files are applied in name order, which is the
  order `-qb-schema` replays them in: number them, `001_init.sql`,
  `002_reviews.sql`.
- Migrations only go forward. To undo one, write another. A "down" file in
  the folder would be read, by the runner and by `[%sql]`, as one more
  migration.
- A file may hold any statements: tables, indexes, functions, data. Only the
  ones that shape tables matter to the types.
- A file from `pg_dump`, psql meta-commands and all, can be applied as it is.

## How a run goes

1. An advisory lock is taken, so two processes starting together don't both
   apply. It is released when the run ends, and by Postgres if the
   connection is lost.
2. The `qb_migrations` table is created if it isn't there: each applied
   file's name, a checksum of its text, and when it was applied.
3. Every pending file is parsed. If one can't be, nothing is applied.
4. Each pending file runs in a transaction of its own, with the record that
   it was applied. If a statement fails, that file leaves nothing behind;
   the files before it stay applied, and the run stops there.

A file whose first line is

```sql
-- qb:no-transaction
```

runs without a transaction, for statements that can't be in one, such as
`CREATE INDEX CONCURRENTLY`. If it fails part-way, what it had done stays,
and it is not recorded: fix the database by hand before running it again.

## What stops a run

Each of these stops `up` before anything is applied, naming the file:

| Message | Cause | What to do |
| --- | --- | --- |
| `... was changed after it was applied` | The file's text differs from what was applied. | Put it back, and make the change in a new migration. |
| `... was applied, and is no longer among the migrations` | An applied file was deleted or renamed. | Put it back. |
| `... is not applied, and comes before ..., which is` | A pending file sorts before an applied one, usually after merging a branch. | Rename it to come last, or pass `--out-of-order`. |
| `...: syntax error ...` | Postgres can't parse the file. | Fix the SQL. |
| `... has its own BEGIN or COMMIT` | The file opens or ends a transaction itself; its `COMMIT` would end the runner's early. | Take them out, or make `-- qb:no-transaction` the first line. |

The checksum is of the file's bytes, so reformatting an applied file, or
changing its line endings, counts as a change.

## verify

`verify` replays the folder into the schema `[%sql]` uses and compares it
with the live tables. `up` runs it afterwards. It reports each difference
on a line:

```
the database doesn't match the migrations:
  books.pages: not in the database
  books.title: VARCHAR(80) in the migrations, varchar(40) in the database
  authors.name: NOT NULL in the migrations, nullable in the database
  reviews.stars: a default in the migrations, none in the database
  reviews: primary key (id) in the migrations, none in the database
  table drafts: not in the migrations
```

Compared:

| What | How |
| --- | --- |
| Tables | One has it and the other doesn't. |
| Columns | One has it and the other doesn't. |
| A column's type | By name; and the length of `VARCHAR(n)` / `CHAR(n)`, the precision and scale of `NUMERIC(p, s)`, and the precision of `TIMESTAMP(p)` / `TIME(p)` where the migration gives one. |
| `NOT NULL` | Whether the column can be NULL. |
| Defaults | Whether the database fills the column in (a default, an identity, a generated column), not what with: this is what decides which columns an `INSERT` may leave out. |
| Primary key | Its columns, in order. |

Not compared: indexes, and unique, foreign-key and check constraints. The
migrations aren't read for those, since no type depends on them.

Tables are looked for in the connection's current schema and in any schema a
migration names (`CREATE TABLE audit.events`). Tables elsewhere are not
reported.

## An existing database

For a database that already has the tables and no `qb_migrations` table:

```sh
qb-migrate baseline
```

records every file as applied without running it. It refuses unless
`verify` passes, so the files are only recorded for a database that really
has what they describe.

## From a program

The library does what the command does, on a `Caqti_blocking` connection.
Run it when the program starts, before it serves anything:

```ocaml
match Qb_migrate.of_dir "migrations" with
| Error m -> failwith m
| Ok migrations -> (
    match Qb_migrate.run db migrations with
    | Ok applied -> List.iter (Printf.printf "applied %s\n") applied
    | Error e -> failwith (Qb_migrate.error_to_string e))
```

| Function | Result |
| --- | --- |
| `of_dir dir` | The folder's `*.sql` files, in name order |
| `plan db migrations` | The pending names that would run (validated, writes nothing); `~out_of_order:true` as the flag |
| `run db migrations` | The names applied; `~out_of_order:true` as the flag |
| `status db migrations` | Each migration's status; writes nothing |
| `verify db migrations` | The differences; `[]` when there are none |
| `baseline db migrations` | The names recorded |
| `embed migrations` | OCaml source for the list |

Errors are a variant (`` `Changed ``, `` `Missing ``, `` `Out_of_order ``,
`` `Invalid ``, `` `Failed ``, `` `Not_verified ``, `` `Db ``) to match on,
and `error_to_string` for a message.

To ship one binary with no folder beside it, build the files in:

```dune
(rule
 (targets migrations.ml)
 (deps
  (glob_files migrations/*.sql))
 (action
  (run qb-migrate embed -m migrations -o %{targets})))
```

and pass `Migrations.migrations` to `Qb_migrate.run`.

## Limits

- Postgres only. The files are split and read with Postgres's parser.
- Blocking connections only. An Lwt program runs its migrations before it
  starts serving.
- No "down" migrations, and no migration written for you from a change to
  the schema.
- `verify` doesn't compare indexes or constraints other than the primary
  key, nor what a default is.
