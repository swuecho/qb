# qb

[![CI](https://github.com/swuecho/qb/actions/workflows/ci.yml/badge.svg)](https://github.com/swuecho/qb/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![OCaml](https://img.shields.io/badge/OCaml-%E2%89%A5%204.14-orange)](https://ocaml.org)

A typed, Drizzle-style query builder for single-table CRUD on
[Caqti](https://github.com/paurkedal/ocaml-caqti).

> **Status:** pre-1.0. The API may still change; questions, issues and pull
> requests are welcome.

Define a table once (a record type and one column per field) and get typed
select, insert, update, delete and `CREATE TABLE`. Every column and condition
carries its row type, so a column of one table can't be used in a query on
another, and comparing an `int` column to a string won't compile. Neither will
an `update` or `delete` with no `where`.

```ocaml
module Users = struct
  type t = { id : int; name : string; age : int; email : string option }

  let id =
    Qb.column "id" Qb.int (fun u -> u.id) ~primary_key:true ~auto_increment:true

  let name = Qb.column "name" Qb.text (fun u -> u.name)
  let age = Qb.column "age" Qb.int (fun u -> u.age)
  let email = Qb.column "email" (Qb.nullable Qb.text) (fun u -> u.email)

  let table =
    Qb.(
      table "users"
        (let+ id = col id and+ name = col name and+ age = col age
         and+ email = col email in
         { id; name; age; email }))
end

let adults db =
  Qb.(
    from Users.table
    |> where Ops.(Users.age > 18 && Users.email <> None)
    |> order_by [ asc Users.name ]
    |> Blocking.all db)
```

To read just some columns, list them. Rows come back as `Row.t`, matched with
list syntax; adding a column changes only the list and the pattern:

```ocaml
from Users.table
|> select [ Users.id; Users.name ]
|> Blocking.all db
|> Result.map (List.map (fun Qb.Row.[ id; name ] -> (id, name)))
```

For a record or labeled tuple, use `let+` / `and+` instead:
`select (let+ id = col Users.id and+ name = col Users.name in (~id, ~name))`.

To insert a new row and let the database fill in ids and defaults, give some
columns with `values`; `insert_values` returns the row as stored:

```ocaml
let ann = Qb.Blocking.insert_values db Qb.(values Users.table Ops.[ Users.name := "Ann"; Users.age := 34 ])
```

`dune exec ./example/users.exe` runs a full walkthrough on in-memory SQLite
(it uses labeled tuples, so it builds on OCaml 5.4 and later only).

## Documentation

- [Tutorial](doc/tutorial.md): `[%sql]` from an empty project to three-table
  queries with aggregates.
- [SQL reference](doc/sql.md): every typing rule, what is rejected, and each
  error message.
- [Migrations reference](doc/migrate.md): how `qb-migrate` applies and verifies
  migrations.
- [`example/`](example): runnable examples. `dune exec ./example/join.exe`,
  `dune exec ./example/users.exe` and `dune exec ./example/sql/main.exe`.
- The API reference is in the `.mli` files; `dune build @doc` renders it with
  odoc.

## Packages

- **`qb`**: the builder, plus `Qb.Blocking` for `Caqti_blocking` connections.
  `Qb.Make` adapts it to any other Caqti backend.
- **`qb-lwt`**: `Qb_lwt`, the same queries on `Caqti_lwt` connections, such as
  the ones `Dream.sql` hands out.
- **`qb-pg-parse`**: Postgres's own parser
  ([libpg_query](https://github.com/pganalyze/libpg_query), the one sqlc uses)
  for checking SQL, generating qb tables from a schema, and typing queries
  written as SQL. See below.
- **`qb-ppx`**: `[%sql "SELECT ..."]`, SQL written inline and typed from your
  schema at compile time.
- **`qb-migrate`**: applies those same migrations to a Postgres database,
  and checks that the database matches what your queries were compiled
  against. See below.

## Writes from a form: `Qb.Changeset`

For input you don't trust (a form, a JSON body), build a changeset: Ecto's
idea, typed by your columns. `cast` reads text into each listed column,
the validations run in OCaml, and the constraints you declare turn the
database's refusals into errors on the right field:

```ocaml
let changeset cs params =
  Qb.Changeset.(
    cs
    |> cast params [ field Users.name; field Users.age; field Users.email ]
    |> validate_required [ field Users.name; field Users.age ]
    |> validate Users.name [ length ~min:2 ~max:80 () ]
    |> validate Users.age [ range ~ge:0 ~lt:150 () ]
    |> unique_constraint Users.email)

match Qb_lwt.insert_changeset db (changeset (Qb.Changeset.for_insert Users.table) params) with
| Ok user -> ...                    (* as stored, id and all *)
| Error (`Invalid cs) -> ...        (* Changeset.errors cs: [("email", "has already been taken")] *)
| Error (#Caqti_error.t as e) -> ...
```

`update_changeset` does the same for `Changeset.for_update Users.table user`,
finding the row by its primary key; with nothing changed it sends nothing.
`Changeset.input col cs` is what a form field should show again: the text
the user typed, or the current value. Built-in column types know how to read
text (an HTML checkbox's `on`, a `datetime-local`, ...); give `Qb.ty`
`~of_string` for your own. Not on MySQL, which has no `RETURNING`.

## Tables from a schema: `qb-pg-parse`

`qb-pg-parse gen` replays your migrations (CREATE / ALTER / DROP TABLE, in
the order given) and writes one qb module per table, like `Users` above:

```sh
qb-pg-parse gen migrations/*.sql -o schema.ml
qb-pg-parse check --caqti queries/*.sql   # syntax only; ? read as $1, $2, ...
```

A `pg_dump --schema-only` of a live database works too, `\restrict` lines and
all. Tables outside `public` go in a module per schema: `gea.reports` becomes
`Gea.Reports`, built with `Qb.table ~schema:"gea" "reports"`.

Or keep the modules in step with the migrations from a dune rule:

```dune
(rule
 (targets schema.ml)
 (deps (glob_files migrations/*.sql))
 (action (run qb-pg-parse gen %{deps} -o %{targets})))
```

Each module also gets the CRUD helpers:

- `values`: a new row, one labelled argument per column. Columns the database
  can fill in (identity, serial, `DEFAULT`, nullable) are optional, so leaving
  out a required one is a compile error:
  `Qb.Blocking.insert_values db (Users.values ~email:"ann@x.org" ())`.
- `by_pk`: for a single-column primary key, the query for one row, to pass to
  `first`, `update` or `delete`:
  `Qb.Blocking.update db Qb.Ops.[ Users.name := "Ann" ] (Users.by_pk id)`.

Column types map to qb's exactly where they can (`BIGINT` to `Qb.int`,
`TIMESTAMPTZ` to `Qb.ptime`, ...); others such as `INTEGER` or `VARCHAR(80)`
get their own `Qb.ty`, so `Qb.create_table_sql` gives back the same table.
`NUMERIC`, arrays and other types with no OCaml counterpart are read as text.

From OCaml, `Qb_pg_parse.Pg.check ~caqti:true` checks the text of any
statement qb renders (`Qb.to_sql`, `Qb.insert_sql`, `Qb.update_sql`, ...); the
package's tests run it over every statement shape.

It needs libpg_query installed: `brew install libpg_query`, or build it from
source. If it isn't found, set `LIBPG_QUERY_PREFIX` to the directory holding
its `include/` and `lib/`.

## SQL written inline: `qb-ppx`

The rule of thumb: **one table, use the builder; more than one, write SQL.**
Joins, aggregates and anything grouped go in `[%sql]`, written where you use
it. `[%sql "..."]` is checked against
your migrations when the file is compiled, and its types are worked out for
you:

```ocaml
let by_author name =
  [%sql
    {|SELECT c.id, c.body
      FROM comments c JOIN users u ON u.id = c.author_id
      WHERE u.name = :name|}]
(* : string -> (int * string) Qb.stmt *)

Qb_lwt.fetch_all db (by_author "Ann")
```

`:name` is the OCaml variable `name`, and must be a `string` because
`users.name` is `TEXT`. Nothing is generated into a file. Set it up once in
dune, pointing at the migrations (`qb-pg-parse init` prints the stanzas for
your folder names):

```dune
(executable
 (name main)
 (preprocess
  (pps qb-ppx -- -qb-schema migrations -qb-tables Schema))
 (preprocessor_deps
  (source_tree migrations)))
```

Mistakes are compile errors at the place in the SQL:

```
File "bin/main.ml", line 5, characters 12-13:
5 |       WHERE emial = :email|}]
                ^
Error: unknown column emial (did you mean email?)
```

A `:name` with no such variable, or one of the wrong type, is reported at
the `:name`.

Parameters take their types from the columns they are compared with, and
rows are tuples (or objects, with `[%sql.obj]`) whose elements are options
where a column can be NULL or sits on the outer side of a join. `SELECT`,
`INSERT`, `UPDATE` and `DELETE` over joined tables are read, with
aggregates, the usual functions, `CASE` and casts; subqueries, `WITH` and
`UNION` are rejected with a message, and written with `Qb.Stmt.make`
instead, types given by hand.

Types come from the migrations alone, with no database at build time. The
SQL is sent as written, so these queries are Postgres's.

- The [tutorial](doc/tutorial.md) builds this up step by step, from
  migrations to three-table queries with aggregates; `dune exec
  ./example/sql/main.exe` runs it.
- The [reference](doc/sql.md) has every rule: how each parameter and column
  gets its type, what is rejected, each error, queries kept in `.sql` files,
  and troubleshooting.

## Applying migrations: `qb-migrate`

The folder `[%sql]` takes its types from is also what you run against the
database:

```sh
qb-migrate new add_reviews     # migrations/003_add_reviews.sql, empty
qb-migrate up                  # apply what is pending, then verify
qb-migrate status              # applied / pending / CHANGED / MISSING
qb-migrate verify              # compare the database with the migrations
```

Each file is applied once, in name order, in a transaction of its own.
`verify` then compares the live tables with the schema your queries were
compiled against (columns, types, `NOT NULL`, defaults, primary keys) and
says where they differ. A program can apply its migrations itself when it
starts, with `Qb_migrate.run`. Postgres only.

The [reference](doc/migrate.md) has the rules: what stops a run, what
`verify` compares, adopting an existing database, and building the
migrations into a binary.

## What it does for you

- **Dialects.** SQL is rendered for the connection's dialect (Postgres,
  SQLite or MySQL). Identifiers are always quoted, so tables and columns can be
  called `user` or `order`.
- **Column types.** `int` and `int64` are `BIGINT`, `float` is
  `DOUBLE PRECISION`, so OCaml values never overflow or get rounded. Also:
  `text`, `bool`, `octets`, `ptime`, `date`, `uuid`, `json`, and `nullable` of
  any of them. `Qb.ty` defines your own.
- **Prepared statements.** Each statement is prepared once per connection and
  reused. The cache is keyed by SQL text and type-checked by Caqti before
  reuse.
- **Logging.** At debug level on the `Qb.src` Logs source, each statement
  is one line once it has run: its text, its parameters, what came of it and
  how long it took (`... WHERE "age" > ? [18] -> 2 rows, 412us`). The
  parameters are your data, so keep that level for development.
- **Row counts and rows back.** `update_count` and `delete_count` say how
  many rows changed (`0`: no such row); `update_returning` and
  `delete_returning` read them back. `execute_count` does the same for a
  statement written as SQL.
- **Many rows.** `insert_many db Users.table rows` is one statement.
- **Transactions.** `transaction db (fun () -> ...)` commits on `Ok` and
  rolls back on `Error` or an exception. They don't nest, and a nested call
  raises rather than quietly committing the outer one.
- **Errors.** `Qb.is_unique_violation e` and `Qb.violation e` say which
  constraint the database refused, whatever the driver.
- **Conditions.** Besides the comparisons: `is_in`, `not_in`, `between`,
  `like`, `ilike`, `is_null`, and for a nullable column against a plain
  value, `>?`, `=?`, ... and `like_opt` (`Ops.(Books.pages >? 300)`).

## Install

The packages are not on opam yet; pin them from GitHub:

```sh
opam pin add qb git+https://github.com/swuecho/qb.git
opam pin add qb-lwt git+https://github.com/swuecho/qb.git        # for Lwt
opam pin add qb-pg-parse git+https://github.com/swuecho/qb.git   # tables from migrations
opam pin add qb-ppx git+https://github.com/swuecho/qb.git        # [%sql "..."]
opam pin add qb-migrate git+https://github.com/swuecho/qb.git    # apply the migrations
```

From a checkout, pass `.` instead of the URL. The packages need OCaml 4.14 or
later and dune 3.0; `qb` currently targets Caqti 2.x (`>= 2.3` and `< 3.0`).
You also need a Caqti driver, such as `caqti-driver-sqlite3` or
`caqti-driver-postgresql`.

`qb-pg-parse` and `qb-ppx` additionally need
[libpg_query](https://github.com/pganalyze/libpg_query) at build time:
`brew install libpg_query` on macOS, or build it from source. If it is not in
`/usr/local`, set `LIBPG_QUERY_PREFIX` to the directory holding its `include/`
and `lib/`. `qb-migrate` also needs a PostgreSQL database at run time.

## Develop

```sh
dune build
dune test                 # SQL rendering and SQLite
dune build @fmt           # check formatting (ocamlformat 0.29.0)
dune build @doc           # build the odoc documentation
scripts/test-pg.sh        # the same, plus Postgres in a throwaway Docker container
```

Postgres tests run whenever `QB_TEST_PG_URL` is set, so you can also point them
at a database you already have:

```sh
QB_TEST_PG_URL=postgresql://user:pass@localhost/db dune test --force
```

## Scope

- The builder is for one table. Its two-table `join` is narrow on purpose
  (one join, conditions and order on the left table, read only): use it when
  a join's conditions are decided at run time or the database isn't
  Postgres, and `[%sql]` for every other join.
- `insert_returning` needs `RETURNING`, which MySQL lacks.

## Contributing

Contributions are welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for the
development setup, test commands and release process.

## License

qb is released under the [MIT License](LICENSE).

`qb-pg-parse` links against
[libpg_query](https://github.com/pganalyze/libpg_query), which is distributed
under its own BSD-style license. Caqti and the database drivers have their own
licenses.
