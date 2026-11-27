# Tutorial: SQL in OCaml, typed from your migrations

This walks through `[%sql "..."]` from an empty directory to queries over
three tables with aggregates. Every snippet is from
[example/sql](../example/sql), which you can run:

```sh
dune exec ./example/sql/main.exe
```

It runs on an in-memory SQLite database so there is nothing to set up. The
types still come from Postgres migrations; the SQL in the example is the
subset both databases share.

For the rules behind each step, see the [reference](sql.md).

## What you will have at the end

```ocaml
let titles_by author =
  [%sql
    {|SELECT b.title, b.year
      FROM books b JOIN authors a ON a.id = b.author_id
      WHERE a.name = :author
      ORDER BY b.year|}]
```

No type is written, and none is needed: the compiler knows this is
`string -> (string * int) Qb.stmt`, because `authors.name` is `TEXT`,
`books.title` is `TEXT NOT NULL` and `books.year` is `INTEGER NOT NULL`.
Misspell a column and the build fails, pointing at the column.

## 1. Install

You need [libpg_query](https://github.com/pganalyze/libpg_query), Postgres's
parser as a library:

```sh
brew install libpg_query
```

Then the packages, from a checkout of this repository:

```sh
opam pin add qb . --kind=path
opam pin add qb-pg-parse . --kind=path
opam pin add qb-ppx . --kind=path
```

and a Caqti driver, such as `caqti-driver-postgresql`.

libpg_query is only used while compiling. Your program doesn't link it.

## 2. Migrations

The schema is whatever your migrations build. Here there are two, in
`migrations/`:

```sql
-- 001_init.sql
CREATE TABLE authors (
  id BIGSERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  country TEXT
);

CREATE TABLE books (
  id BIGSERIAL PRIMARY KEY,
  author_id BIGINT NOT NULL REFERENCES authors (id),
  title TEXT NOT NULL,
  year INTEGER NOT NULL,
  pages INTEGER
);
```

```sql
-- 002_reviews.sql
CREATE TABLE reviews (
  id BIGSERIAL PRIMARY KEY,
  book_id BIGINT NOT NULL REFERENCES books (id),
  stars INTEGER NOT NULL,
  note TEXT
);
```

They are replayed in file-name order, so `ALTER TABLE`, renames and drops in
later files are followed. A `pg_dump --schema-only` file works as well.

## 3. dune

Two things read the migrations: a rule that writes a module per table, and
the ppx that checks your SQL.

```dune
; Table modules from the migrations.
(rule
 (targets schema.ml)
 (deps
  (glob_files migrations/*.sql))
 (action
  (run qb-pg-parse gen %{deps} -o %{targets})))

; [%sql "..."] checked against the same migrations.
(executable
 (name main)
 (preprocess
  (pps qb-ppx -- -qb-schema migrations -qb-tables Schema))
 (preprocessor_deps
  (source_tree migrations))
 (libraries qb caqti.blocking caqti-driver-sqlite3 uri))
```

- `-qb-schema migrations` is the directory to replay.
- `-qb-tables Schema` names the module the rule writes (`schema.ml`). With
  it, `SELECT a.*` gives you a `Schema.Authors.t`.
- `preprocessor_deps` makes dune recompile your queries when a migration
  changes. Leave it out and the ppx can't find the files.

That is all the setup. From here on you only write OCaml.

## 4. What the table modules give you

`schema.ml` has a module per table. You never edit it, but it helps to know
what is in it:

```ocaml
module Authors : sig
  type t = { id : int; name : string; country : string option }

  val table : t Qb.table
  val id : (t, int) Qb.col           (* and name, country *)

  val values : ?id:int -> name:string -> ?country:string -> unit -> t Qb.values
  val by_pk : int -> ...             (* the query for one row *)
end
```

`country` is a `string option` because the column can be NULL. In `values`,
`id` and `country` are optional because the database can fill them in; `name`
is required, so forgetting it doesn't compile.

## The rule: one table, or more than one

qb gives you two ways to ask for data, and the line between them is simple:

- **One table: the builder.** Inserts, reads, updates and deletes, written
  as OCaml (step 5).
- **More than one table, or anything grouped: `[%sql]`.** Joins, aggregates,
  and whatever else you would write as SQL (step 7 onwards).

The builder can also join two tables, for the cases SQL text can't cover
(step 6). Read that step as an exception, not as the usual way to join.

## 5. The builder: one table

For a single table, you don't write SQL at all. Conditions are OCaml
expressions over the table's columns, so a wrong column or a wrong type is
an ordinary type error.

**Insert.** `values` takes each column as an argument, and `insert_values`
returns the row as the database stored it, id and all:

```ocaml
let* ursula = insert_values db (Authors.values ~name:"Ursula" ~country:"US" ()) in
(* ursula : Authors.t, with ursula.id filled in *)
```

**Read.** Start `from` a table, then narrow, order and limit:

```ocaml
let* rows =
  Qb.(
    from Books.table
    |> where Ops.(Books.year > 1970 && Books.pages <> None)
    |> order_by [ desc Books.year ]
    |> limit 10
    |> all db)
(* rows : Books.t list *)
```

```
-- builder: books after 1970 with a page count, newest first
  1972 Invisible Cities
```

`Books.pages <> None` renders `IS NOT NULL`. `all` gives every row, `first`
at most one, `count` how many match.

**Some columns.** List them, and match the row with the same list syntax:

```ocaml
let* rows =
  Qb.(
    from Books.table
    |> select [ Books.title; Books.year ]
    |> where Ops.(is_in Books.year [ 1969; 1972 ])
    |> order_by [ asc Books.title ]
    |> all db)
in
List.iter (fun Qb.Row.[ title; year ] -> Printf.printf "  %d %s\n" year title) rows;

let* n = Qb.(from Reviews.table |> where Ops.(Reviews.stars >= 5) |> count db) in
```

```
-- builder: some columns, a list of values, a count
  1972 Invisible Cities
  1969 The Left Hand
  2 reviews of 5 stars
```

`is_in` takes an OCaml list of any length.

**Update and delete.** Both take a query saying which rows. `by_pk` is the
query for one row:

```ocaml
let* () = update db Qb.Ops.[ Books.pages := Some 341 ] (Books.by_pk dispossessed.id) in
let* () = delete db Qb.(from Reviews.table |> where Ops.(Reviews.stars < 2)) in
```

An `update` or `delete` on a query with no `where` doesn't compile. To
change every row, say so: `where all_rows`.

`update_count` and `delete_count` are the same statements returning how many
rows they changed, so `0` tells you the row wasn't there;
`update_returning` and `delete_returning` give the rows back. To do several
writes or none, wrap them: `transaction db (fun () -> ...)` commits when the
function returns `Ok` and rolls back on `Error`.

Because a condition is a value, a filter can be chosen at run time:

```ocaml
let books_of_year year =
  Qb.(
    from Books.table
    |> where
         (match year with
         | Some y -> Ops.(Books.year = y)
         | None -> all_rows)
    |> count db)
```

## 6. The builder's join, and when to use it

Joins belong in `[%sql]`. The builder has a two-table join for two cases
where SQL text doesn't fit:

- **The shape of the query is decided at run time**: a condition that is
  there or not, built up from what a form sent.
- **The database isn't Postgres.** The builder renders SQLite and MySQL too;
  `[%sql]` SQL is Postgres's, sent as written.

Rows come back as pairs of the two tables' records:

```ocaml
let* rows =
  Qb.(
    from Books.table
    |> join Authors.table
         ~on:Ops.(Books.author_id == Authors.id)
         ~where:Ops.(Authors.country = Some "US")
    |> where Ops.(Books.year > 1960)
    |> order_by [ asc Books.year ]
    |> all db)
(* rows : (Books.t * Authors.t) list *)
```

```
-- builder: books with their author (US authors only)
  1969 The Left Hand, by Ursula
  1974 The Dispossessed, by Ursula
```

- `~on` says which rows go together, written with `==`, the left table's
  column first.
- `~where` on the join is the condition on the right table (`authors`).
- The plain `where` and `order_by` are about the left table (`books`).

`left_join` keeps every left row, with `None` when nothing matched:

```ocaml
let* rows =
  Qb.(
    from Books.table
    |> select [ Books.title ]
    |> left_join Reviews.table ~on:Ops.(Books.id == Reviews.book_id)
    |> order_by [ asc Books.title ]
    |> all db)
(* each row is matched as (Qb.Row.[ title ], (review : Reviews.t option)) *)
```

Here `select` picked one column of `books`; the right table is always read
whole.

### Where it stops

A joined query is still a query on the left table. That keeps its types
simple, and it is why the join is narrow:

| Limit | What it means |
| --- | --- |
| Two tables | A query takes one join. A second `join` doesn't compile. |
| Left table's columns only, after the join | `where` and `order_by` can't name a right-table column. A condition on the right table goes in the join's `~where`; ordering by a right-table column isn't possible. |
| No condition across both tables beyond `~on` | A `where` comparing a `books` column with an `authors` column can't be written. |
| `select` picks left columns only, before the join | The right table always comes back whole. `select` after a `join` doesn't compile. |
| No rows-without-a-match | "Books with no review" can't be written. |
| No aggregates | No `GROUP BY`, `count` per group, `max`, `avg`. |
| `limit` counts pairs | With several reviews per book, `limit 10` is 10 pairs, not 10 books. |
| Read only | `update` and `delete` through a join don't compile. |
| `~where` on a `left_join` filters matches, not rows | It says which right rows may match. Left rows are all kept. |

Most of these are compile errors, such as "Type joined is not compatible
with type plain". None of them applies to `[%sql]`, which is where the rest
of this tutorial goes.

## 7. A first query

```ocaml
let titles_by author =
  [%sql
    {|SELECT b.title, b.year
      FROM books b JOIN authors a ON a.id = b.author_id
      WHERE a.name = :author
      ORDER BY b.year|}]
```

Three things happened:

- **`:author` is the OCaml variable `author`.** A parameter is a colon and a
  name, and it reads the variable of that name wherever the `[%sql]` is
  written. Here that is the function's argument.
- **Its type came from the comparison.** `a.name = :author` and `a.name` is
  `TEXT`, so `author` must be a `string`.
- **The row is a tuple of the columns**, `(string * int)`, in the order you
  selected them.

Run it with `fetch_all`, `fetch_opt` (zero or one row) or `fetch_one`
(exactly one):

```ocaml
let* rows = fetch_all db (titles_by "Ursula") in
List.iter (fun (title, year) -> Printf.printf "  %d %s\n" year title) rows
```

```
-- titles by Ursula
  1969 The Left Hand
  1974 The Dispossessed
```

These come from `Qb.Blocking` here; `Qb_lwt` has the same functions for Lwt.

Use a `{|quoted string|}` for SQL. It can span lines, needs no escaping, and
error positions are exact in it.

## 8. Whole rows, and three tables

Selecting `t.*` gives the table's own record instead of loose columns:

```ocaml
let reviews_with_at_least stars =
  [%sql
    {|SELECT r.*, b.*, a.*
      FROM reviews r
      JOIN books b ON b.id = r.book_id
      JOIN authors a ON a.id = b.author_id
      WHERE r.stars >= :stars
      ORDER BY r.id|}]
(* int -> (Reviews.t * Books.t * Authors.t) Qb.stmt *)
```

```ocaml
let* rows = fetch_all db (reviews_with_at_least 5) in
List.iter
  (fun ((r : Reviews.t), (b : Books.t), (a : Authors.t)) ->
    Printf.printf "  %d* %s, by %s\n" r.stars b.title a.name)
  rows
```

```
-- reviews of 5 stars, with book and author
  5* The Left Hand, by Ursula
  5* Invisible Cities, by Italo
```

You can mix the two: `SELECT a.*, b.title` is `(Authors.t * string)`. A wide
tuple is hard to read, so prefer `t.*` once you want more than a few columns
of a table.

## 9. Left joins make options

```ocaml
let books_and_reviews =
  [%sql
    {|SELECT b.title, r.stars, r.note
      FROM books b LEFT JOIN reviews r ON r.book_id = b.id
      ORDER BY b.title, r.id|}]
(* (string * int option * string option) Qb.stmt *)
```

`reviews.stars` is `NOT NULL` in the table, but a book with no review still
gets a row, with NULL there. So `r.stars` is an `int option`: the type
follows the query, not just the column.

```
-- every book, with its reviews if any
  Invisible Cities   5 short
  The Dispossessed   - -
  The Left Hand      5 cold
  The Left Hand      4 -
```

There are no parameters, so `books_and_reviews` is a plain value, not a
function.

With whole rows, `SELECT b.*, r.*` here would be
`(Books.t * Reviews.t option)`.

The same join finds the rows with no match, which the builder can't ask
for:

```ocaml
let unreviewed =
  [%sql
    {|SELECT b.title
      FROM books b LEFT JOIN reviews r ON r.book_id = b.id
      WHERE r.id IS NULL
      ORDER BY b.title|}]
(* string Qb.stmt *)
```

```
-- books with no review
  The Dispossessed
```

## 10. Aggregates, and saying a type yourself

```ocaml
let author_stats =
  [%sql
    {|SELECT a.name,
             count(DISTINCT b.id) AS books,
             max(b.year) AS latest,
             CAST(avg(r.stars) AS REAL) AS stars
      FROM authors a
      LEFT JOIN books b ON b.author_id = a.id
      LEFT JOIN reviews r ON r.book_id = b.id
      GROUP BY a.name
      ORDER BY books DESC, a.name|}]
(* (string * int * int option * float option) Qb.stmt *)
```

- `count` is an `int` and never NULL.
- `max(b.year)` has the column's type, and is an option: over no rows it is
  NULL.
- `avg` of an integer is `NUMERIC` in Postgres, which has no OCaml number to
  go into. Rather than guess, qb asks you to say the type with a cast.
  Without the `CAST`, the build stops with:

  ```
  Error: avg of a whole number is NUMERIC, which qb reads as text: for a
         float, write avg(...)::float8
  ```

  Most functions need no cast: `sum` of an integer, `lower`, `length`,
  `now()`, comparisons and `CASE` all have types qb knows. The
  [reference](sql.md#result-columns) has the list.

In Postgres you would write `avg(r.stars)::float8`. `CAST(... AS ...)` is
the same thing spelled so that SQLite reads it too.

```
-- per author
  Ursula  2 books, latest 1974, 4.5 stars
  Italo   1 books, latest 1972, 5.0 stars
  Anon    0 books, latest -, - stars
```

## 11. Optional filters

A search form has filters the user may leave empty. Write `:name?` for a
parameter that may be NULL, and it becomes an `option`:

```ocaml
let authors ~country ~limit =
  [%sql
    {|SELECT * FROM authors
      WHERE :country? IS NULL OR country = :country
      ORDER BY name
      LIMIT :limit|}]
(* country:string option -> limit:int -> Authors.t Qb.stmt *)
```

```ocaml
fetch_all db (authors ~country:None ~limit:10)          (* everyone *)
fetch_all db (authors ~country:(Some "IT") ~limit:10)   (* only IT *)
```

- The `?` is needed once; every `:country` is the same parameter.
- `:limit` is an `int` because it is in `LIMIT`.
- Parameters read variables, so labelled arguments work as you'd expect:
  the labels are yours, not qb's.
- `SELECT *` over one table is that table's row.

Without the `?`, a parameter can't be NULL at all: its type is not an
option.

## 12. LIKE, BETWEEN, IN

They type their parameters the way `=` does:

```ocaml
let search pattern from until =
  [%sql
    {|SELECT title FROM books
      WHERE title LIKE :pattern AND year BETWEEN :from AND :until
      ORDER BY title|}]
(* string -> int -> int -> string Qb.stmt *)
```

One column, so rows are the bare `string`, not a tuple of one.

`IN (:a, :b)` compares with two values. For a list whose length you only
know at run time, write `IN :name` with no parentheses, and pass an OCaml
list:

```ocaml
let titles_in years =
  [%sql "SELECT title FROM books WHERE year IN :years ORDER BY title"]
(* int list -> string Qb.stmt *)

fetch_all db (titles_in [ 1969; 1972 ])
```

```
-- titles from 1969 or 1972, then from no year at all
  Invisible Cities, The Left Hand
  (0 rows)
```

The list is spelled out when the statement runs (`IN ($1, $2)`). An empty
list is fine: `IN` then matches nothing, and `NOT IN` everything.

## 13. Writes

`INSERT`, `UPDATE` and `DELETE` are read too. A parameter takes the type of
the column it goes into, and `RETURNING` is typed like a select list:

```ocaml
let add_review ~book ~stars ~note =
  [%sql
    {|INSERT INTO reviews (book_id, stars, note)
      VALUES (:book, :stars, :note?)
      RETURNING *|}]
(* book:int -> stars:int -> note:string option -> Reviews.t Qb.stmt *)

let add_pages book n =
  [%sql "UPDATE books SET pages = pages + :n WHERE id = :book RETURNING pages"]
(* int -> int -> int option Qb.stmt *)

let drop_reviews_below stars = [%sql "DELETE FROM reviews WHERE stars < :stars"]
(* int -> unit Qb.stmt *)
```

- `:note?` because `reviews.note` can be NULL and you want to pass `None`.
  Without the `?` it would be a plain `string`.
- `pages + :n` types `:n` from the column, and the result is an option
  because `books.pages` can be NULL.
- With no `RETURNING` there are no rows: the statement is a `unit Qb.stmt`,
  run with `execute`.

```ocaml
let* r = fetch_one db (add_review ~book:dispossessed.id ~stars:3 ~note:None) in
let* pages = fetch_one db (add_pages left_hand.id 14) in
let* () = execute db (drop_reviews_below 4) in
```

```
-- writes
  review 4: 3 stars
  The Left Hand now has 300 pages
  3 reviews left
```

For a plain insert, upsert, or update by key on one table, the builder's
`values`, `upsert`, and `by_pk` from step 5 are shorter, and `values` checks
that you gave every required column. Reach for SQL when the write needs an
expression (`pages + :n`) or another table (`UPDATE ... FROM`).

## 14. What a mistake looks like

A column that doesn't exist:

```
File "main.ml", line 5, characters 12-18:
5 |       WHERE a.nmae = :author|}]
                ^^^^^^
Error: unknown column a.nmae (did you mean name?)
```

A column two tables have:

```
1 | let q = [%sql "SELECT id FROM books b JOIN authors a ON a.id = b.author_id"]
                          ^^
Error: column id is in several tables: write b.id or a.id
```

SQL that Postgres can't parse (the message is Postgres's own):

```
1 | let q = [%sql "SELECT title FROM books WHERE year > 1970 ORDER BY"]
                                                                      ^
Error: syntax error at end of input
```

A parameter with nothing to take its type from:

```
1 | let q x = [%sql "SELECT title FROM books WHERE soundex(title) = :x"]
                                                                    ^^
Error: can't work out the type of :x: write a cast, e.g. :x::text
```

A variable of the wrong type. This one is OCaml's own type error, at the
parameter:

```
2 |   [%sql "SELECT title FROM books b JOIN authors a ON a.id = b.author_id WHERE a.name = :author"]
                                                                                           ^^^^^^^
Error: The value author has type int but an expression was expected of type
         string
```

And the one that makes this worth having: change a migration, and every
query that no longer fits fails to compile. Drop `authors.country` in a new
migration and step 11 stops building at `country = :country`.

## 15. When `[%sql]` can't read it

`[%sql]` reads one `SELECT`, `INSERT`, `UPDATE` or `DELETE` over tables
joined with `ON`. That is where it stops, on purpose. For a subquery, a
CTE or a `UNION` it says so, and you write the statement with its types
given:

```ocaml
let authors_without_books : string Qb.stmt =
  Qb.Stmt.(
    make
      "SELECT name FROM authors a WHERE NOT EXISTS (SELECT 1 FROM books b \
       WHERE b.author_id = a.id) ORDER BY name"
      [] (ret Qb.text))
```

Parameters here are `$1`, `$2`, with their values in order:
`[ arg Qb.int id; arg Qb.text name ]`. The last argument says how to read a
row: `ret Qb.text` for one column, `both (ret Qb.int) (ret Qb.text)` for
`(int * string)`, `whole Authors.table` for an `Authors.t`, `unit` for
none. It runs through the same `fetch_all` / `execute`, so by-hand and
inferred statements mix freely.

This is not checked against the schema. Keep it for what the ppx can't do.

And past that, there is Caqti itself. The `db` you pass to qb is an ordinary
Caqti connection, so a `Caqti_request` of your own runs on it next to
everything above.

## 16. Applying the migrations to Postgres

The example creates its tables on SQLite with `create_table`, so that it runs
with nothing installed. On Postgres, the tables come from the migrations
themselves, the same files the types were read from. That is `qb-migrate`:

```sh
opam pin add qb-migrate . --kind=path
export DATABASE_URL=postgresql://user:pass@localhost/library
qb-migrate up
```

```
applied 001_init.sql
applied 002_reviews.sql
the database matches the migrations
```

Each file is applied once, in name order, in a transaction of its own, and
recorded in a `qb_migrations` table. The last line is `verify`: the folder
is replayed into the schema `[%sql]` uses and compared with the live tables,
so what your queries were compiled against is what they will run against.

To change the schema, add a file; never edit one that has been applied:

```sh
qb-migrate new add_isbn        # migrations/003_add_isbn.sql
```

```sql
ALTER TABLE books ADD COLUMN isbn TEXT;
```

On the next build `Books.t` has an `isbn : string option` and `[%sql]` knows
the column; on the next `qb-migrate up` the database has it too. If the two
drift apart, because someone altered a table by hand, `verify` says where:

```
the database doesn't match the migrations:
  books.pages: not in the database
```

Migrations only go forward: to undo one, write another. A program can apply
them itself when it starts, with `Qb_migrate.run db migrations`; the
[reference](migrate.md) has that, and how to
build the files into the binary.

## Which tool for which job

| You want to | Use |
| --- | --- |
| Insert, upsert, update, delete, or read one table | The builder: `Authors.values`, `upsert`, `by_pk`, `Qb.from ... \|> where ...` (step 5) |
| Two or more tables, aggregates, rows without a match | `[%sql "..."]` (steps 7 to 12) |
| A write with an expression or another table | `[%sql "..."]` (step 13) |
| A join whose conditions are decided at run time, or on SQLite / MySQL | The builder's `join` (step 6) |
| Anything more: a subquery, a CTE, a `UNION` | `Qb.Stmt.make` by hand, or Caqti directly (step 15) |
| Create and change the tables on Postgres | `qb-migrate up` (step 16) |
| Queries kept in `.sql` files | `qb-pg-parse queries` (see the [reference](sql.md#queries-in-sql-files)) |
