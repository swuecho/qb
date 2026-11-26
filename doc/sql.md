# Reference: queries written as SQL

How qb types a query written as SQL: what it reads, how each type is worked
out, and what it refuses. For a walkthrough, start with the
[tutorial](tutorial.md).

- [Builder or SQL](#builder-or-sql)
- [Three ways to write a statement](#three-ways-to-write-a-statement)
- [Setup](#setup)
- [Parameters](#parameters)
- [Result columns](#result-columns)
- [Rows](#rows)
- [Types](#types)
- [What is read, and what isn't](#what-is-read-and-what-isnt)
- [Errors](#errors)
- [Running statements](#running-statements)
- [Queries in .sql files](#queries-in-sql-files)
- [Statements by hand](#statements-by-hand)
- [How it works](#how-it-works)
- [Troubleshooting](#troubleshooting)

## Builder or SQL

| Query | Use |
| --- | --- |
| One table: insert, read, update, delete | The builder |
| Two or more tables, aggregates, anything grouped | SQL: `[%sql]` |
| A write with an expression, an upsert, or a second table | SQL: `[%sql]` |
| A join whose conditions are decided at run time, or a database that isn't Postgres | The builder's `join` |
| Anything more: a subquery, `WITH`, `UNION` | `Qb.Stmt.make`, or Caqti directly |

The builder's two-table join is deliberately narrow: one join, conditions
and order on the left table only, no aggregates, read only. It is there for
the two cases in the table, not as the usual way to join. The
[tutorial](tutorial.md#6-the-builders-join-and-when-to-use-it) lists its
limits.

## Three ways to write a statement

All three give a `'a Qb.stmt`, run by the same functions.

| | Where the SQL is | Types | Rows |
| --- | --- | --- | --- |
| `[%sql "..."]` (`qb-ppx`) | Inline in OCaml | Inferred | Tuples, table rows |
| `qb-pg-parse queries` | `.sql` files | Inferred | Records, table rows |
| `Qb.Stmt.make` (`qb`) | Inline in OCaml | You give them | Whatever you build |

The first two share one analysis, so everything below about parameters,
columns and types holds for both. Where they differ, it says so.

`[%sql]` has three spellings, all the same thing:

```ocaml
[%sql "SELECT title FROM books WHERE id = :id"]
{%sql|SELECT title FROM books WHERE id = :id|}
[%qb.sql "SELECT title FROM books WHERE id = :id"]
```

The second needs no escaping for `"` in the SQL. The third is the full
name, for a file where another ppx also answers to `%sql`.

## Setup

### dune

`qb-pg-parse init` prints these stanzas for your folder and module names
(`qb-pg-parse init -m db/migrations --module Db_schema`), so the three
places that must agree do.

```dune
(rule
 (targets schema.ml)
 (deps
  (glob_files migrations/*.sql))
 (action
  (run qb-pg-parse gen %{deps} -o %{targets})))

(executable
 (name main)
 (preprocess
  (pps qb-ppx -- -qb-schema migrations -qb-tables Schema))
 (preprocessor_deps
  (source_tree migrations))
 (libraries qb))
```

The same `preprocess` and `preprocessor_deps` fields go in a `library`
stanza.

### Flags

| Flag | Meaning |
| --- | --- |
| `-qb-schema PATH` | A `.sql` file, or a directory whose `*.sql` files are replayed in file-name order. Repeat it to replay several, in the order given. Required. |
| `-qb-tables MODULE` | The module holding `qb-pg-parse gen`'s output for the same schema. Optional; see [Rows](#rows). |
| `-qb-bigint TYPE` | The OCaml type `BIGINT` is read as: `int` (the default) or `int64`. Match `--bigint` if you pass it to `qb-pg-parse gen`. |

A relative `PATH` is looked for in the folder of the file being compiled,
then in each folder above it, up to the project root (the folder with
`dune-project`). The nearest match wins. So `migrations` works when the
directory sits beside your dune file or in any folder above it, and
`db/migrations` works from anywhere in the project. The same path works in
`dune build` and in the editor, which runs the ppx from a different folder.

`preprocessor_deps` must cover the schema files. It is what makes dune copy
them where the ppx can read them, and recompile when one changes.

### The schema

Files are replayed statement by statement, as Postgres's parser reads them:

- `CREATE TABLE`, `DROP TABLE`
- `ALTER TABLE`: add / drop column, set / drop `NOT NULL`, change type,
  set / drop default, add / drop identity, add primary key
- `RENAME` of tables and columns

Everything else (indexes, functions, data, grants) is skipped. A
`pg_dump --schema-only` file is read as it is, psql meta-commands and all.

There is no database at build time. The schema is what the files say.

## Parameters

### Syntax

A parameter is `:name`. In `[%sql]`, it reads the OCaml variable `name` in
scope where the `[%sql]` is written. In a `.sql` file, it becomes the
labelled argument `~name`.

- `:name?` says the value may be NULL. Its type is then an `option`.
- `IN :name`, with no parentheses, is a list: see
  [Lists](#lists).
- A name used several times is one parameter, sent once. One `?` on any use
  marks it.
- A name that is an OCaml keyword gets a trailing underscore: `:type` reads
  the variable `type_`.
- `$1` is rejected: write `:name`.
- `::` casts are left alone, so `:id::bigint` is the parameter `id`, cast.
- Parameters inside `'strings'`, `"identifiers"` and comments are not
  parameters.

One thing to avoid: an array slice written `a[1:n]` would be read as the
parameter `:n`. Write `a[1 : n]`.

### How a parameter gets its type

From how it is used, in this order of appearance in the query:

| Use | Type |
| --- | --- |
| `col = :p`, `<>`, `!=`, `<`, `>`, `<=`, `>=` (either side) | The column's |
| `col IN (:a, :b)` | The column's, each |
| `col IN :list`, `NOT IN :list` | A list of the column's |
| `col + :p`, `-`, `*`, `/`, `%`, `\|\|` (either side) | The column's |
| `INSERT INTO t (col) VALUES (:p)` | The column's |
| `UPDATE t SET col = :p`, `ON CONFLICT ... DO UPDATE SET col = :p` | The column's |
| `col LIKE :p`, `ILIKE` | The column's |
| `col BETWEEN :a AND :b`, `NOT BETWEEN` | The column's, each |
| `col IS DISTINCT FROM :p`, `IS NOT DISTINCT FROM` | The column's |
| `COALESCE(col, :p)` | The first typed argument's |
| `col > :p + 1`, `col = COALESCE(:p?, col)`, `col LIKE '%' \|\| :p \|\| '%'` | The column's: a comparison reaches into the other side's arithmetic, `COALESCE` and concatenation |
| `lower(:p)`, `upper`, `trim`, `length`, `substr`, ... (the first argument) | `TEXT` |
| `'%' \|\| :p` | `TEXT` |
| `col @> :p`, `<@`, `&&` | The column's |
| `:p = ANY(col)` | The type of the array's elements |
| `:p::type`, `CAST(:p AS type)` | That type |
| `LIMIT :p`, `OFFSET :p` | `BIGINT` |

"Column" above is anything whose type is known: a column, a cast, a
constant, `count(...)`. So `count(*) > :n` types `:n`.

Not typed, so they need a cast:

- arithmetic with only constants around it: `:n + 1 > 2` says little about
  `:n`, since a constant fits many types. Write `:n::int + 1`. Next to a
  column (`year + :n`, `year > :n + 1`) it is typed.
- arguments of functions other than the text ones above:
  `my_function(:s)`. Write `my_function(:s::text)`.
- `= ANY(:ids)`: a parameter can't be an array. Use `IN :ids`, which the
  error says.
- `:p IS NULL` alone. Any other use of `:p` types it.

A parameter used as two types that can't be the same OCaml value is an
error. Types of one family are fine together: the integer types, the float
types, the text types (`TEXT`, `VARCHAR`, `CHAR`, `CITEXT`), and
`TIMESTAMP` with `TIMESTAMPTZ`.

### Lists

`col IN :name` takes an OCaml list of any length:

```ocaml
let titles_in years =
  [%sql "SELECT title FROM books WHERE year IN :years"]
(* int list -> string Qb.stmt *)
```

- Write it without parentheses. `IN (:a)` is the ordinary SQL form, a
  single value.
- The list is spelled out when the statement runs: `IN ($1, $2, $3)`. Each
  length is its own prepared statement.
- An empty list makes `IN` false and `NOT IN` true, as you would expect; it
  is sent as a subquery with no rows.
- A list can't be `:name?`, and one name can't be a list in one place and a
  single value in another.

### NULL

A parameter without `?` is never NULL: its OCaml type has no `None`.

`col = :p?` with `None` sends `col = NULL`, which in SQL matches nothing.
To match NULLs, say so:

```sql
WHERE col IS NOT DISTINCT FROM :p?
```

and for "no filter when empty":

```sql
WHERE :p? IS NULL OR col = :p
```

Postgres can't tell the type of a parameter from `IS NULL`, even when the
parameter is typed elsewhere, and would reject this. qb knows the type, so
it sends `CAST($1 AS TEXT) IS NULL` for you.

## Result columns

### Type

| Expression | Type | Can be NULL |
| --- | --- | --- |
| `t.col`, `col` | The column's | If the column can, or `t` is on the outer side of a join |
| `count(...)` | `BIGINT` | No |
| `min(x)`, `max(x)` | `x`'s | Yes (no rows) |
| `sum(x)` of `INTEGER` / `SMALLINT` | `BIGINT` | Yes (no rows) |
| `sum(x)`, `avg(x)` of floats | The float type; `DOUBLE PRECISION` for `avg` | Yes |
| `sum(x)` of `BIGINT`, `avg(x)` of integers | `NUMERIC` in Postgres: write a cast (`::bigint`, `::float8`) | |
| `bool_and`, `bool_or`, `string_agg` | `BOOLEAN`, `TEXT` | Yes |
| `lower`, `upper`, `trim`, `replace`, `substr`, `left`, `right`, `lpad`, `rpad`, `md5`, `to_char`, ... | `TEXT` | If an argument can |
| `length`, `char_length`, `position`, `strpos` | `INTEGER` | If an argument can |
| `concat(...)` | `TEXT` | No |
| `now()`, `CURRENT_TIMESTAMP` | `TIMESTAMPTZ` | No |
| `CURRENT_DATE` | `DATE` | No |
| `abs(x)`, `date_trunc(unit, x)` | `x`'s | As `x` |
| `random()`, `gen_random_uuid()` | `DOUBLE PRECISION`, `UUID` | No |
| `a = b`, `<`, `IN`, `LIKE`, `BETWEEN`, `AND`, `OR`, `NOT` | `BOOLEAN` | If a side can |
| `x IS NULL`, `x IS TRUE`, `a IS DISTINCT FROM b` | `BOOLEAN` | No |
| `CASE WHEN ... THEN a ELSE b END` | The branches' (the wider number) | With no `ELSE`, or if a branch can |
| `NULLIF(a, b)` | `a`'s | Yes |
| `GREATEST(a, b)`, `LEAST(a, b)` | The wider of the arguments' | Only if every argument can |
| `COALESCE(a, b, ...)` | The first typed argument's | Only if every argument can |
| `x::type`, `CAST(x AS type)` | `type` | As `x`; yes when `x` isn't understood |
| `a + b`, `-`, `*`, `/`, `%` on integers, or on floats | The wider of the two | If either can |
| The same on an integer and a float | `DOUBLE PRECISION` | If either can |
| `-x` | `x`'s | As `x` |
| `j ->> 'k'`, `j #>> path` | `TEXT` | Yes |
| `j -> 'k'`, `j #> path` | `j`'s (`JSON` / `JSONB`) | Yes |
| `date_part(unit, x)` | `DOUBLE PRECISION` | If an argument can |
| `extract(unit FROM x)` | `NUMERIC` in Postgres: write a cast (`::int`, `::float8`) | |
| `a \|\| b` on text | `TEXT` | If either can |
| `'text'`, `42`, `true` | `TEXT`, `INTEGER`, `BOOLEAN` | No |
| `1.5` | `NUMERIC` (a `string` in OCaml: cast it, `1.5::float8`) | No |
| `:p` | The parameter's | If written `:p?` |
| Anything else | Unknown: write a cast | |

A cast is how you tell qb a type it can't work out:

```sql
SELECT avg(b.pages)::float8 AS pages, soundex(a.name)::text AS code
```

The error says which cast when it can: for `avg` of an integer it suggests
`::float8`.

A cast column is treated as nullable unless what is inside is known not to
be, since qb can't see through the expression. To say it is never NULL,
wrap it: `COALESCE(soundex(a.name)::text, '')`.

### Nullability from joins

| Join | Columns that become nullable |
| --- | --- |
| `a JOIN b`, `a, b` | None |
| `a LEFT JOIN b` | `b`'s |
| `a RIGHT JOIN b` | `a`'s |
| `a FULL JOIN b` | Both |

This carries through nested joins: in `a LEFT JOIN (b JOIN c ON ...) ON ...`
both `b` and `c` are nullable.

Conditions don't narrow types. `WHERE r.note IS NOT NULL` leaves `r.note` an
option; use `COALESCE` or match on it.

### Names

Columns are checked wherever they appear: the select list, `ON`, `WHERE`,
`GROUP BY`, `HAVING`, `ORDER BY`. An unqualified name must be in exactly one
table of the `FROM`. `GROUP BY` and `ORDER BY` may also use an output name
(`... AS n ... ORDER BY n`).

A table is known by its alias if it has one, else by its name. Tables in
other schemas are written `schema.table`.

## Rows

### `[%sql]`: tuples

Each item of the select list is one element, in order:

| Select list | Row |
| --- | --- |
| `b.title` | `string` |
| `b.title, b.year` | `string * int` |
| `a.*` | `Schema.Authors.t` |
| `a.*, b.title` | `Schema.Authors.t * string` |
| `a.*, r.*` with `LEFT JOIN reviews r` | `Schema.Authors.t * Schema.Reviews.t option` |
| `*` | One element per table of the `FROM`, in order |

One element is the bare value, not a tuple of one.

`t.*` is a table row only with `-qb-tables`. Without it, `t.*` is spelled
out as that table's columns in the tuple.

A whole row on the outer side of a join is `None` when all its columns are
NULL.

### `[%sql.obj]`: objects

A tuple of five columns is easy to read in the wrong order, and two columns
of the same type can be swapped without a type error. `[%sql.obj "..."]` is
the same statement with each row as an object, a method per column:

```ocaml
let by_year year =
  [%sql.obj "SELECT id, title, pages AS length FROM books WHERE year = :year"]
(* : int -> < id : int; title : string; length : int option > Qb.stmt *)

List.iter (fun b -> print_endline b#title) rows
```

A method is named after the column, its `AS` name, or the function
(`count`); with `-qb-tables`, a `t.*` is one method named after the table
(`SELECT a.*, b.title` gives `< authors : Schema.Authors.t; title : string
>`). Two methods with the same name are an error: rename one with `AS`. One
column is still the bare value.

### `qb-pg-parse queries`: records

A record `<query>_row` with a field per column, named after the column, its
`AS` name, or the function (`count`, `coalesce`); or the bare value for one
column. Two fields with the same name are an error: rename one with `AS`.

With `--tables`, a query whose select list is only `t.*` items gives the
table rows instead, as above.

## Types

How a Postgres type is sent and read:

| Postgres | OCaml |
| --- | --- |
| `BIGINT`, `INTEGER`, `SMALLINT`, serials | `int` |
| `DOUBLE PRECISION`, `REAL` | `float` |
| `TEXT`, `VARCHAR(n)`, `CHAR(n)`, `CITEXT`, `NAME` | `string` |
| `BOOLEAN` | `bool` |
| `BYTEA` | `string` |
| `TIMESTAMPTZ`, `TIMESTAMP` | `Ptime.t` |
| `DATE` | `Ptime.t` |
| `UUID` | `string` |
| `JSONB`, `JSON` | `string` |
| `NUMERIC` | `string`, its text form, to keep the precision |
| Arrays, enums, anything else | `string`, its text form |

These are the same mappings the table modules use, so a column has one
OCaml type whether you read it through the builder or through SQL.

## What is read, and what isn't

One statement per query.

**`SELECT`** with

- tables in `FROM`, with or without aliases, in any schema;
- `JOIN`, `LEFT` / `RIGHT` / `FULL JOIN` with `ON`, nested or chained, and
  comma joins;
- `WHERE`, `GROUP BY`, `HAVING`, `ORDER BY`, `LIMIT`, `OFFSET`, `DISTINCT`;
- any expression, as long as the result columns and parameters can be typed
  by the rules above.

**`INSERT`** with `VALUES` (one row or several) or `DEFAULT VALUES`, with or
without a column list, and `ON CONFLICT ... DO NOTHING` / `DO UPDATE SET`,
where `excluded.col` is the row that was to be inserted. A column that is
`NOT NULL` with no default must be given a value: leaving one out is an
error here rather than when the statement runs.

**`UPDATE`** with `SET col = value, ...`, `WHERE`, and `FROM` other tables.

**`DELETE`** with `WHERE`, and `USING` other tables.

All three take `RETURNING`, typed like a select list: `RETURNING *` is the
table's row, `RETURNING id` an `int`. Without it the statement returns no
rows and is a `unit Qb.stmt`, for `execute`.

A parameter going into a column takes the column's type. It is not made an
option because the column can be NULL: write `:name?` to be able to pass
`None`.

Rejected, each with its own message. This is the intended scope, not a
list of things to come:

- subqueries, in `FROM` or anywhere else
- `WITH`
- `UNION`, `INTERSECT`, `EXCEPT`
- `JOIN ... USING`, `NATURAL JOIN`
- `INSERT ... SELECT`
- `VALUES` on its own, `SELECT INTO`
- functions in `FROM` (`generate_series(...)`)
- several statements in one query
- anything that isn't one of the four statements (`CREATE`, `TRUNCATE`, ...)

For these, write [a statement by hand](#statements-by-hand) with
`Qb.Stmt.make`, or use Caqti directly: the `db` every qb function takes is
an ordinary Caqti connection, so a `Caqti_request` runs on it alongside qb's
own queries.

The SQL is sent as you wrote it, with these changes: `:name` becomes `$1`,
`$2`, ...; comments are removed; a parameter tested with `IS NULL` gets a
cast; and a list is spelled out when the statement runs. It is Postgres's
SQL: nothing translates it for another database, though what SQLite also
understands will run there.

## Errors

In `[%sql]` these are compile errors at the place in the string; from
`qb-pg-parse queries` they name the file and line.

| Message | Cause |
| --- | --- |
| `syntax error at or near "..."` | Postgres's parser rejected the SQL. |
| `unknown table t` | Not in the schema, or an alias that isn't in `FROM`. A near miss adds `(did you mean ...?)`. |
| `unknown column c`, `unknown column t.c` | Not a column of the tables in `FROM`. A near miss adds `(did you mean ...?)`. |
| `column c is in several tables: write table.c` | Qualify it. |
| `table t is named twice: give one an alias` | A self-join needs aliases. |
| `:p is a list in one place (IN :p) and a single value in another` | Use two names. |
| `a list can't be NULL: write IN :p` | Drop the `?`. |
| `INSERT has more values than columns` | |
| `INSERT leaves out c: it is NOT NULL and has no default` | Give the column a value. |
| `can't work out the type of column c: write a cast`, `... of this expression` | See [Result columns](#result-columns). |
| `qb doesn't know what f(...) returns: say so with a cast` | A function outside the table in [Result columns](#result-columns). |
| `avg of a whole number is NUMERIC ...`, `sum of a BIGINT or NUMERIC is NUMERIC ...` | Write the cast the message gives. |
| `two columns are named c: rename one with AS` | In `[%sql.obj]`, each method needs its own name. |
| `can't work out the type of :p: write a cast` | See [Parameters](#how-a-parameter-gets-its-type). |
| `parameter :p is used as both A and B` | Use two parameters, or cast one use. |
| `parameters are written :name here, not $1` | |
| `only SELECT, INSERT, UPDATE and DELETE are read` | See above. |
| `... isn't supported` | See above. |
| `no schema to check against` | No `-qb-schema` flag. |
| `schema: ...: No such file or directory (is it in preprocessor_deps?)` | See [Troubleshooting](#troubleshooting). |

Two come from OCaml rather than qb, and point at the `:x` in the SQL:

- `Unbound value x`: the query has `:x` and no variable `x` is in scope.
- `The value x has type int but an expression was expected of type string`:
  the variable isn't the type the column needs.

Positions are exact in `{|quoted strings|}`. In a `"plain string"` with
escapes or line continuations they can be off by the escapes' width.

## Running statements

On every runner (`Qb.Blocking`, `Qb_lwt`, or `Qb.Make` of your own):

| Function | Result |
| --- | --- |
| `fetch_all db stmt` | `'a list` |
| `fetch_opt db stmt` | `'a option`; more than one row is an error |
| `fetch_one db stmt` | `'a`; no row, or several, is an error |
| `execute db stmt` | `unit`, for a `unit Qb.stmt` |
| `execute_count db stmt` | `int`: how many rows the statement changed |

Each returns a `result` with Caqti's error, in the runner's fiber, like the
builder's `all` and `first`.

Statements are prepared once per connection and reused, logged at debug
level on the `Qb.src` source, and counted in the same 512-statement cache
as builder queries.

## Queries in .sql files

To keep SQL out of OCaml files, write queries in a file, each under a name:

```sql
-- name: titles_by
SELECT b.title, b.year
FROM books b JOIN authors a ON a.id = b.author_id
WHERE a.name = :author
ORDER BY b.year;

-- name: author_count
SELECT count(*) FROM authors;
```

```sh
qb-pg-parse queries migrations/*.sql -q queries.sql --tables Schema -o queries.ml
```

```ocaml
type titles_by_row = { title : string; year : int }

val titles_by : author:string -> titles_by_row Qb.stmt
val author_count : int Qb.stmt
```

As a dune rule:

```dune
(rule
 (targets queries.ml)
 (deps
  queries.sql
  (glob_files migrations/*.sql))
 (action
  (run qb-pg-parse queries %{dep:migrations/001_init.sql}
   %{dep:migrations/002_reviews.sql} -q queries.sql --tables Schema -o
   %{targets})))
```

| | |
| --- | --- |
| `FILE...` | The schema files, replayed in the order given. |
| `-q FILE` | A file of queries. Repeat for several. |
| `--tables MODULE` | As `-qb-tables`. |
| `--bigint TYPE` | `int` (the default) or `int64` for `BIGINT` columns. |
| `-o FILE` | Where to write; standard output by default. |

Compared with `[%sql]`: parameters are labelled arguments rather than
variables in scope, rows are records rather than tuples, and there is a
generated file. The analysis is the same.

## Statements by hand

`Qb.Stmt` builds a statement from its parts. Nothing is checked against the
schema.

```ocaml
let authors_without_books min_year : string Qb.stmt =
  Qb.Stmt.(
    make
      "SELECT name FROM authors a WHERE NOT EXISTS (SELECT 1 FROM books b \
       WHERE b.author_id = a.id AND b.year >= $1)"
      [ arg Qb.int min_year ]
      (ret Qb.text))
```

`make sql args row`:

- `sql` has `$1`, `$2`, ... (or all `?`).
- `args` are the values in order: `arg ty value`, with `ty` a `Qb.ty` such
  as `Qb.int`, `Qb.text`, `Qb.nullable Qb.text`. `arg_list ty values` is a
  list for `x IN ($n)`, spelled out when the statement runs.
- `row` says how to read a result row:

| | Reads |
| --- | --- |
| `unit` | Nothing: for `execute` |
| `ret ty` | One column |
| `whole Table.table` | All of a table's columns, as `SELECT t.*` returns them |
| `opt row` | `None` when every column is NULL |
| `both a b` | `a` then `b`, as a pair |
| `map f row` | `row`, through `f` |
| `let+` / `and+` | The same, to build a record: `let+ id = ret Qb.int and+ name = ret Qb.text in { id; name }` |

## How it works

1. **Schema.** The migration files are parsed by libpg_query (Postgres's
   own parser, as a library) and replayed into a list of tables with their
   columns, types and `NOT NULL`s.
2. **Parameters.** `:name` is rewritten to `$n`, keeping a map from each
   output byte to where it came from, so errors point into what you wrote.
3. **Parse.** The query goes through the same parser. A syntax error is
   Postgres's own.
4. **Scope.** The `FROM` clause (or the table being written to) gives the
   tables in scope, each marked if it is on the outer side of a join.
5. **Check and type.** Every column reference is resolved against the
   scope. Parameters take types from the comparisons they are in; result
   columns from the rules above.
6. **Code.** The result is an ordinary `Qb.Stmt.make` expression with a
   type annotation, which OCaml then type-checks against your code.

`[%sql]` does this inside the compiler's preprocessing step; `qb-pg-parse
queries` does it as a command. Nothing connects to a database.

What it deliberately doesn't have is Postgres's catalog of functions and
operators. That is why `avg(x)` or `soundex(s)` need a cast: a short, visible
annotation rather than a table of thousands of signatures to keep right.

## Troubleshooting

**`no schema to check against`.** The `preprocess` field has no
`-qb-schema`. Flags go after `--`: `(pps qb-ppx -- -qb-schema migrations)`.

**`schema: migrations/001.sql: No such file or directory`.** Dune only
shows the ppx the files it was told about. Add
`(preprocessor_deps (source_tree migrations))`. If the directory isn't
beside the dune file, give the flag the path from the project root and
dune the same: `-qb-schema db/migrations` and
`(source_tree %{workspace_root}/db/migrations)`.

**`Unbound module Schema`.** `-qb-tables Schema` names a module that isn't
there. Add the `qb-pg-parse gen` rule that writes `schema.ml`, or change
the flag to the module you have.

**A migration changed and nothing recompiled.** The file isn't covered by
`preprocessor_deps`.

**The editor still shows an error after I fixed the migration.** The editor
reads the migrations afresh each time it checks a file, with no build
needed, but it only re-checks a file when that file is edited or reopened.

**A column has type `string` and I expected a number.** `NUMERIC` and
decimal constants are read as text. Cast to `float8` or `bigint`.

**A column is an `option` and I know it isn't NULL.** It can be NULL in the
table, sits on the outer side of a join, or is a cast of something qb can't
see into. `COALESCE(x, default)` makes it non-null.

**`pg_query.h` not found when installing.** Install libpg_query, or set
`LIBPG_QUERY_PREFIX` to the directory holding its `include/` and `lib/`.
