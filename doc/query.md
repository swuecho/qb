# Reference: multi-table queries, expressions, and relations

How to build multi-table queries, SQL expressions, aggregates, subqueries,
CTEs, multi-table writes, and batched relational loads with `Qb.Query`,
`Qb.Expr`, `Qb.Raw`, and `Qb.Rel`.

For a quick hands-on walkthrough, start with the
[multi-table tutorial](query-tutorial.md) or run [`example/multi.ml`](../example/multi.ml)
(`dune exec ./example/multi.exe`). For inline SQL checked against migrations,
see [the SQL reference](sql.md).

- [Choosing the right API](#choosing-the-right-api)
- [Core model: scoped table handles](#core-model-scoped-table-handles)
- [Projections](#projections)
- [Joins](#joins)
- [SQL expressions (`Qb.Expr`)](#sql-expressions-qbexpr)
- [Aggregates, window functions, grouping, ordering, and locking](#aggregates-window-functions-grouping-ordering-and-locking)
- [Dynamic queries and keyset pagination](#dynamic-queries-and-keyset-pagination)
- [JSON / JSONB, Postgres helpers, and raw SQL fragments (`Qb.Raw`)](#json--jsonb-postgres-helpers-and-raw-sql-fragments-qbraw)
- [Subqueries, CTEs, recursive CTEs, and set operations](#subqueries-ctes-recursive-ctes-and-set-operations)
- [Expression writes and multi-table writes](#expression-writes-and-multi-table-writes)
- [Relations (`Qb.Rel`) and batched eager loading](#relations-qbrel-and-batched-eager-loading)
- [Generated helpers from `qb-pg-parse`](#generated-helpers-from-qb-pg-parse)
- [Running queries and inspecting SQL](#running-queries-and-inspecting-sql)
- [Dialect differences](#dialect-differences)
- [Troubleshooting](#troubleshooting)

---

## Choosing the right API

`qb` offers three complementary ways to query your database; you can mix them
freely on the same tables and connections:

| Task | Recommended API | Why |
| --- | --- | --- |
| Simple single-table CRUD, form validation, changesets, simple upserts | Single-table builder (`Qb.from`, `Qb.Changeset`) | Shortest syntax; no table handle prefix needed |
| Multi-table joins (2, 3, N tables), self-joins, anti-joins, dynamic filters, aggregates, subqueries, CTEs / recursive CTEs, multi-dialect portability | Multi-table builder (`Qb.Query`, `Qb.Expr`, `Qb.Raw`) | Full compile-time column & nullability checking, composable in OCaml, works on Postgres, SQLite, and MySQL |
| Loading nested parent/child graphs without N+1 or cartesian row duplication | `Qb.Rel` + `load_many` / `load_one` | Batches each level into 1 `IN` query; auto-generated from `REFERENCES` by `qb-pg-parse` |
| Static, Postgres-only reporting queries where raw SQL reads clearest | `[%sql "..."]` (`qb-ppx`) or `qb-pg-parse queries` | Write Postgres SQL directly, type-checked against your migrations at compile time |

---

## Core model: scoped table handles

In the single-table builder (`Qb.from Users.table`), columns belong to one
fixed row type. In `Qb.Query`, every table you bring into `FROM` or a `JOIN`
returns a **scoped handle**:

- `'r Qb.Query.ref` — a **non-null** handle, returned by `from`, `join`,
  `cross_join`, `right_join` (right side), `from_cte`, `join_cte`,
  `from_subquery`, `join_subquery`, `join_many`, and `join_one`.
- `'r Qb.Query.opt_ref` — a **nullable** handle, returned by `left_join`,
  `right_join` (rebound left side), `full_join` (both sides), `left_join_cte`,
  `left_join_subquery`, `left_join_many`, and `left_join_one`. Use `to_opt` to
  downgrade any additional earlier left handles after a `right_join` or
  `full_join`.

You turn a table's column into a typed `'a Qb.Expr.t` by indexing the handle
with OCaml's indexing operators:

| Handle type | Column type | Operator | Result expression type |
| --- | --- | --- | --- |
| `'r ref` | `('r, 'a) col` | `u.@(Users.name)` | `'a Expr.t` |
| `'r opt_ref` | `('r, 'a) col` (non-null column) | `c.@?(Comments.body)` | `'a option Expr.t` |
| `'r opt_ref` | `('r, 'a option) col` (already nullable) | `c.@??(Comments.reply_to)` | `'a option Expr.t` (no `option option`!) |

Why three operators?

1. **Compile-time table-in-scope safety:** You cannot construct an expression
   referencing `Posts` unless you have bound a `Posts.t ref` or `Posts.t opt_ref`
   from `from` or a join. Indexing `u.@(Posts.title)` where `u : Users.t ref`
   is a compile error (`Users.t` is not compatible with `Posts.t`).
2. **Automatic outer-join nullability without `option option`:**
   - On a `left_join`, a `NOT NULL` column like `Comments.body : (Comments.t, string) col`
     becomes `string option Expr.t` via `c.@?(Comments.body)`.
   - An already-nullable column like `Comments.reply_to : (Comments.t, int option) col`
     stays `int option Expr.t` via `c.@??(Comments.reply_to)` instead of turning
     into `int option option`.
   - If your schema is generated with `qb-pg-parse gen`, you don't even have to
     pick between `.@?()` and `.@??()`: use `Comments.opt_cols c` and access
     `c'#body` and `c'#reply_to` directly (see
     [Generated helpers](#generated-helpers-from-qb-pg-parse)).
3. **Deterministic, collision-free SQL aliases:** Each `from` / `join`
   allocates a unique internal handle id; when SQL is rendered, handles are
   assigned deterministic aliases `"t0"`, `"t1"`, `"t2"`, ... in traversal
   order. Self-joins and correlated subqueries never clash, and repeated runs
   produce identical SQL text for the prepared-statement cache.

---

## Projections

Finish a `Qb.Query.build` block with `select` (multi-column or arbitrary OCaml
shape) or `select_one` (single expression, required for `in_query` and `scalar`
subqueries).

```ocaml
open Qb

let rows db =
  Query.(
    build
      (let* u = from Users.table in
       let* c =
         left_join Comments.table ~on:(fun c ->
             Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id)))
       in
       select
         (let+ user = whole u                 (* Users.t *)
          and+ comment = whole_opt c          (* Comments.t option *)
          and+ upper_name = expr (Expr.upper u.@(Users.name)) (* string *)
          in
          (user, comment, upper_name))))
  |> Blocking.query_all db
```

Projection combinators:

- `expr : 'a Expr.t -> 'a proj` — reads a single SQL expression.
- `whole : 'r ref -> 'r proj` — reads all columns of a base table handle into
  its row type `'r`.
- `whole_opt : 'r opt_ref -> 'r option proj` — reads all columns of an
  outer-joined base table handle, returning `None` when every column is `NULL`.
- `let+` / `and+` (or `map` / `both`) — combine projections into tuples,
  records, or labeled tuples.
- `select_one : 'a Expr.t -> 'a t build` — selects one expression and marks the
  query as single-column so `in_query`, `not_in_query`, `scalar`, and
  `scalar_opt` accept it.
- `sub : unit build -> unit t` — shorthand `SELECT 1 FROM ...` for `exists` and
  `not_exists`.

---

## Joins

### Multi-table joins and self-joins

Chain any number of `join` (`INNER JOIN`), `left_join`, `right_join`,
`full_join`, and `cross_join` steps. Self-joins need no manual alias helper:

```ocaml
(* 4-table join: comment -> author -> parent comment -> parent's author *)
let comment_thread db =
  Query.(
    build
      (let* c = from Comments.table in
       let* author =
         join Users.table ~on:(fun u ->
             Expr.Ops.(u.@(Users.id) = c.@(Comments.author_id)))
       in
       let* parent =
         left_join Comments.table ~on:(fun p ->
             Expr.Ops.(c.@(Comments.reply_to) =? p.@(Comments.id)))
       in
       let* parent_author =
         left_join Users.table ~on:(fun pu ->
             Expr.Ops.(parent.@?(Comments.author_id) =? pu.@(Users.id)))
       in
       let* () = order_by [ asc c.@(Comments.id) ] in
       select
         (let+ cid = expr c.@(Comments.id)
          and+ author_name = expr author.@(Users.name)
          and+ parent_body = expr parent.@?(Comments.body)
          and+ parent_author_name = expr parent_author.@?(Users.name) in
          (cid, author_name, parent_body, parent_author_name))))
  |> Blocking.query_all db
```

Notice how `~on` works for `left_join`:

- Inside `~on:(fun p -> ...)`, `p` is a **non-null `'r ref`**, because `ON`
  evaluates candidate rows before outer-join null padding.
- The value bound by `let* parent = left_join ...` is an **`'r opt_ref`**, so
  subsequent `where`, `order_by`, `having`, and `select` clauses know `parent`
  may be `NULL`.

### Anti-joins ("left rows with no match")

```ocaml
let users_without_comments db =
  Query.(
    build
      (let* u = from Users.table in
       let* c =
         left_join Comments.table ~on:(fun c ->
             Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id)))
       in
       let* () = where Expr.Ops.(is_null c.@?(Comments.id)) in
       select (whole u)))
  |> Blocking.query_all db
```

### Relation-driven joins

When you have a `Qb.Rel.has_many` or `Qb.Rel.belongs_to` value (defined by hand
or generated by `qb-pg-parse gen`), you can skip writing `~on`:

```ocaml
let* u = from Users.table in
let* c = left_join_many u user_comments in
...
```

Available helpers:

- `join_many parent_ref rel` / `left_join_many parent_ref rel` /
  `left_join_many_opt parent_opt_ref rel`
- `join_one child_ref rel` / `left_join_one child_ref rel` /
  `left_join_one_opt child_opt_ref rel`

The `*_opt` variants let you chain multi-hop outer joins (e.g.
`users -> left_join_many posts -> left_join_many_opt comments`).

---

## SQL expressions (`Qb.Expr`)

### Literals and nullability

All literals are sent as bound Caqti parameters (`?`), never interpolated into
SQL text:

```ocaml
Expr.int 42           (* int Expr.t *)
Expr.int64 42L        (* int64 Expr.t *)
Expr.float 3.14       (* float Expr.t *)
Expr.text "Ann"       (* string Expr.t *)
Expr.bool true        (* bool Expr.t *)
Expr.ptime ts         (* Ptime.t Expr.t *)
Expr.date d           (* Ptime.t Expr.t *)
Expr.uuid id_str      (* string Expr.t *)
Expr.json "{}"        (* string Expr.t *)
Expr.lit custom_ty v  (* 'a Expr.t for any custom Qb.ty *)
Expr.null Qb.text     (* string option Expr.t -> SQL NULL *)
Expr.some e           (* 'a Expr.t -> 'a option Expr.t *)
```

### Operators (`Qb.Expr.Ops`)

Open `Expr.Ops` locally (`Expr.Ops.(...)`):

- **Equality & ordering (same type on both sides):** `=`, `<>`, `>`, `>=`, `<`, `<=`.
  Comparing against `Expr.null _` or `Expr.lit (nullable ty) None` automatically
  renders `IS NULL` / `IS NOT NULL`.
- **Nullable vs. non-null comparison:** `=?`, `<>?`, `>?`, `>=?`, `<?`, `<=?`
  take `'a option Expr.t` on the left and `'a Expr.t` on the right—handy when
  joining a nullable foreign key (`c.@(Comments.reply_to) =? p.@(Comments.id)`)
  or filtering an outer-joined column (`c.@?(Comments.author_id) =? u.@(Users.id)`).
- **Boolean logic:** `&&`, `||`, `not`, `all_of : bool Expr.t list -> bool Expr.t option`,
  `any_of : bool Expr.t list -> bool Expr.t option`.
- **Null checks:** `is_null`, `is_not_null`.
- **Sets & ranges:** `is_in e xs`, `not_in e xs` (`[]` renders `1 = 0` / `NOT (1 = 0)`),
  `between e lo hi`.
- **Pattern matching:** `like`, `ilike`, `like_opt`, `ilike_opt`.
- **Arithmetic & string concat:** `+`, `-`, `*`, `/` (`int Expr.t`);
  `+.`, `-.`, `*.`, `/.` (`float Expr.t`); `^` (`string Expr.t`).

### Scalar SQL functions

| Function | Type | SQL |
| --- | --- | --- |
| `Expr.lower s` | `string Expr.t -> string Expr.t` | `LOWER(s)` |
| `Expr.upper s` | `string Expr.t -> string Expr.t` | `UPPER(s)` |
| `Expr.trim s` | `string Expr.t -> string Expr.t` | `TRIM(s)` |
| `Expr.length s` | `string Expr.t -> int Expr.t` | `CHAR_LENGTH(s)` (SQLite: `LENGTH(s)`) |
| `Expr.concat a b` / `Ops.(a ^ b)` | `string Expr.t -> string Expr.t -> string Expr.t` | `(a \|\| b)` (MySQL: `CONCAT(a, b)`) |
| `Expr.coalesce a b` | `'a option Expr.t -> 'a Expr.t -> 'a Expr.t` | `COALESCE(a, b)` (narrows to non-null!) |
| `Expr.coalesce_opt a b` | `'a option Expr.t -> 'a option Expr.t -> 'a option Expr.t` | `COALESCE(a, b)` |
| `Expr.nullif a b` | `'a Expr.t -> 'a Expr.t -> 'a option Expr.t` | `NULLIF(a, b)` |
| `Expr.case_when branches ~else_` | `(bool Expr.t * 'a Expr.t) list -> else_:'a Expr.t -> 'a Expr.t` | `CASE WHEN ... THEN ... ELSE ... END` |
| `Expr.case_when_opt branches` | `(bool Expr.t * 'a Expr.t) list -> 'a option Expr.t` | `CASE WHEN ... THEN ... END` |
| `Expr.now ()` | `unit -> Ptime.t Expr.t` | `NOW()` (SQLite: `CURRENT_TIMESTAMP`) |
| `Expr.cast ty e` | `'b ty -> _ Expr.t -> 'b Expr.t` | `CAST(e AS <type>)` |

Example combining `coalesce` and `case_when`:

```ocaml
let tier =
  Expr.case_when
    [
      (Expr.Ops.(u.@(Users.age) >= Expr.int 65), Expr.text "senior");
      (Expr.Ops.(u.@(Users.age) >= Expr.int 18), Expr.text "adult");
    ]
    ~else_:(Expr.text "minor")
in
let contact =
  Expr.coalesce u.@(Users.email)
    Expr.Ops.(Expr.lower u.@(Users.name) ^ Expr.text "@local")
```

---

## Aggregates, window functions, grouping, ordering, and locking

### Aggregate functions and `FILTER (WHERE ...)`

| Function | Result type | Notes |
| --- | --- | --- |
| `Expr.count_all` | `int Expr.t` | `COUNT(*)` |
| `Expr.count e` | `int Expr.t` | `COUNT(e)` (counts non-null values) |
| `Expr.count_distinct e` | `int Expr.t` | `COUNT(DISTINCT e)` |
| `Expr.sum_int e` | `int option Expr.t` | Postgres casts `CAST(SUM(e) AS BIGINT)` so Caqti decodes `int` cleanly |
| `Expr.sum_int64 e` | `int64 option Expr.t` | Postgres casts `CAST(SUM(e) AS BIGINT)` |
| `Expr.sum_float e` | `float option Expr.t` | Postgres casts `CAST(SUM(e) AS DOUBLE PRECISION)` |
| `Expr.avg e` | `float option Expr.t` | Casts to `DOUBLE PRECISION` / `REAL` / `DOUBLE` across dialects |
| `Expr.min e` / `Expr.max e` | `'a option Expr.t` | `MIN(e)` / `MAX(e)` |
| `Expr.min_opt e` / `Expr.max_opt e` | `'a option Expr.t` | For already-nullable expressions (no `option option`) |
| `Expr.filter cond agg` | `'a Expr.t` | SQL `FILTER (WHERE cond)` (Postgres & SQLite 3.30+); sits inside `CAST` automatically |

```ocaml
let adult_count =
  Expr.count_all |> Expr.filter Expr.Ops.(u.@(Users.age) >= Expr.int 18)
```

### Window functions (`OVER (PARTITION BY ... ORDER BY ...)`)

Use `Expr.row_number`, `Expr.rank`, `Expr.dense_rank`, `Expr.lag` /
`Expr.lag_default`, `Expr.lead` / `Expr.lead_default`, `Expr.first_value`,
`Expr.last_value`, or turn any aggregate into a window function with
`Expr.over`:

```ocaml
(* Top-1 latest comment per author using a CTE + row_number *)
let latest_per_author db =
  Query.(
    build
      (let* ranked, (aid, body, rn) =
         with_cte "ranked"
           (let* c = from Comments.table in
            select_named
              Named.(
                let+ aid = field "author_id" c.@(Comments.author_id)
                and+ body = field "body" c.@(Comments.body)
                and+ rn =
                  field "rn"
                    (Expr.row_number
                       ~partition_by:[ Expr.any c.@(Comments.author_id) ]
                       ~order_by:[ desc c.@(Comments.id) ]
                       ())
                in
                (aid, body, rn)))
       in
       let* r = from_cte ranked in
       let* () = where Expr.Ops.(r.@(rn) = Expr.int 1) in
       select
         (let+ a = expr r.@(aid) and+ b = expr r.@(body) in
          (a, b))))
  |> Blocking.query_all db

(* Running sum over ordered rows *)
let running_total =
  Expr.sum_int u.@(Users.age) |> Expr.over ~order_by:[ Query.asc u.@(Users.id) ]
```

For aggregates that wrap a Postgres `CAST` (`sum_int`, `avg`), `FILTER` and
`OVER` are automatically emitted **inside** the `CAST`
(`CAST(SUM(...) FILTER (...) OVER (...) AS BIGINT)`).

### `DISTINCT ON` (Postgres)

Pick the first row per key according to `ORDER BY`:

```ocaml
let* () = distinct_on [ Expr.any c.@(Comments.author_id) ] in
let* () = order_by [ asc c.@(Comments.author_id); desc c.@(Comments.id) ] in
```

### `GROUP BY`, `HAVING`, `ORDER BY`, `LIMIT`, `OFFSET`

```ocaml
let active_authors db =
  Query.(
    build
      (let* u = from Users.table in
       let* c =
         left_join Comments.table ~on:(fun c ->
             Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id)))
       in
       let* () = group_by [ Expr.any u.@(Users.id); Expr.any u.@(Users.name) ] in
       let* () = having Expr.Ops.(Expr.count c.@?(Comments.id) >= Expr.int 2) in
       let* () =
         order_by
           [
             desc (Expr.count c.@?(Comments.id));
             nulls_last (asc u.@(Users.email));
           ]
       in
       let* () = limit 20 in
       let* () = offset 0 in
       select
         (let+ name = expr u.@(Users.name)
          and+ comments = expr (Expr.count c.@?(Comments.id)) in
          (name, comments))))
  |> Blocking.query_all db
```

`Blocking.query_count db q` automatically handles `GROUP BY`, `DISTINCT`, and
set-operation queries by wrapping them in `SELECT COUNT(*) FROM (...) AS "_qb_count"`
(stripping `ORDER BY` / `LIMIT` / `OFFSET`), while keeping plain queries as a
direct `SELECT COUNT(*) FROM ... WHERE ...`.

### Row locking (`FOR UPDATE` / `FOR SHARE`)

Useful for job queues and concurrent balance updates inside `transaction`:

```ocaml
let claim_next_job db =
  Blocking.transaction db (fun () ->
      let q =
        Query.(
          build
            (let* j = from Jobs.table in
             let* () = where Expr.Ops.(j.@(Jobs.status) = Expr.text "queued") in
             let* () = order_by [ asc j.@(Jobs.created_at) ] in
             let* () = limit 1 in
             let* () = for_update ~skip_locked:true () in
             select (whole j)))
      in
      Blocking.query_first db q)
```

Options: `for_update ?nowait ?skip_locked ()` and `for_share ?nowait ?skip_locked ()`.
On SQLite (which locks at the database level), locking clauses are omitted.

---

## Dynamic queries and keyset pagination

Because `Qb.Query.build` is a pure monad, dynamic search endpoints compose
cleanly with `where_opt`, `having_opt`, `when_`, `opt`, and `Expr.Ops.all_of` /
`any_of`:

For fast pagination on large tables (avoiding `OFFSET` scans), use
`Expr.Keyset` tuple comparisons (`gt2`, `ge2`, `lt2`, `le2`, `gt3`, `ge3`,
`lt3`, `le3`), which render standard SQL row-value comparisons
`("t0"."age", "t0"."id") < (?, ?)` supported by Postgres, SQLite, and MySQL:

```ocaml
let* () =
  opt cursor (fun (last_age, last_id) ->
      where
        Expr.Keyset.(
          lt2 (u.@(Users.age), u.@(Users.id))
            (Expr.int last_age, Expr.int last_id)))
```

```ocaml
let search_users db ?name_prefix ?min_age ?(only_commenters = false)
    ?(sort = `Newest) ~page_size ~page () =
  let q =
    Query.(
      build
        (let* u = from Users.table in
         let* () =
           opt name_prefix (fun p ->
               where Expr.Ops.(ilike u.@(Users.name) (Expr.text (p ^ "%"))))
         in
         let* () =
           opt min_age (fun a ->
               where Expr.Ops.(u.@(Users.age) >= Expr.int a))
         in
         let* () =
           when_ only_commenters (fun () ->
               where
                 (exists
                    (sub
                       (let* c = from Comments.table in
                        where Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id))))))
         in
         let* () =
           order_by
             (match sort with
             | `Newest -> [ desc u.@(Users.id) ]
             | `Name -> [ asc u.@(Users.name); asc u.@(Users.id) ])
         in
         let* () = limit page_size in
         let* () = offset (page * page_size) in
         select (whole u)))
  in
  Blocking.query_all db q
```

---

## JSON / JSONB, Postgres helpers, and raw SQL fragments (`Qb.Raw`)

### JSON / JSONB (`Qb.Expr.Json`) and Postgres helpers (`Qb.Expr.Pg`)

- `Expr.Json.get doc key` (`->`) and `Expr.Json.get_text doc key` (`->>`) work
  on **Postgres and SQLite 3.38+** (with `get_opt` / `get_text_opt` for
  nullable columns).
- Postgres JSONB helpers: `Expr.Json.contains` (`@> CAST(... AS JSONB)`),
  `Expr.Json.has_key` (`jsonb_exists(...)`—avoids `?` placeholder collisions),
  and `Expr.Json.path_text` (`jsonb_extract_path_text(...)`).
- Postgres helpers: `Expr.Pg.any_array elem arr` (`e = ANY(arr)`) and
  `Expr.Pg.date_trunc unit ts` (`date_trunc(unit, ts)`).

### Typed raw SQL fragments (`Qb.Raw`)

When you need a single dialect-specific expression (Postgres full-text search,
JSONB path extraction, regex operators, trigram similarity, `date_trunc`) without
rewriting the whole dynamic query as raw SQL, drop a `Qb.Raw` fragment into any
`where`, `select`, `order_by`, `having`, `group_by`, or `set_expr`:

```ocaml
(* Postgres regex match with safely bound parameter *)
let matches_ci col pattern : bool Expr.t =
  Raw.bool [ Raw.expr col; Raw.sql " ~* "; Raw.param text pattern ]

(* Postgres JSONB extraction returning text *)
let json_field col key : string option Expr.t =
  Raw.make (nullable text)
    [ Raw.expr col; Raw.sql " ->> "; Raw.param text key ]
```

Fragment builders:

- `Raw.sql s` — literal SQL snippet (never pass untrusted user input here).
- `Raw.param ty v` — safely bound Caqti parameter (`?`).
- `Raw.expr e` — embeds any `Qb.Expr.t` with its proper table alias.
- `Raw.col c` — embeds a single-table column (for `set_expr`).
- `Raw.make ty frags` — builds an `'a Expr.t` decoded with `ty`.
- `Raw.bool frags` — shorthand for `Raw.make Qb.bool frags`.

---

## Subqueries, CTEs, recursive CTEs, and set operations

### Correlated `EXISTS`, `IN`, and scalar subqueries

Outer table handles stay in lexical scope inside nested queries, and inner
tables automatically get fresh aliases (`t1`, `t2`, ...):

```ocaml
(* EXISTS / NOT EXISTS *)
let* u = from Users.table in
let* () =
  where
    (exists
       (sub
          (let* c = from Comments.table in
           where Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id)))))
in ...

(* IN / NOT IN subquery (requires select_one) *)
let* () =
  where
    (in_query u.@(Users.id)
       (build
          (let* c = from Comments.table in
           select_one c.@(Comments.author_id))))
in ...

(* Scalar subquery in SELECT or WHERE (returns 'a option Expr.t) *)
let comment_count =
  build
    (let* c = from Comments.table in
     let* () = where Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id)) in
     select_one Expr.count_all)
in
select
  (let+ name = expr u.@(Users.name)
   and+ cnt = expr (Expr.coalesce (scalar comment_count) (Expr.int 0)) in
   (name, cnt))
```

Use `scalar_opt` when the subquery already returns `'a option` so you avoid
`'a option option`.

### CTEs (`WITH`) and derived tables (`FROM (SELECT ...)`)

Export named columns from a subquery with `select_named` and `Named.(...)`.
Each `field "alias" expr` returns a normal `('s, 'a) col` value that works with
the exact same `.@()` / `.@?()` operators:

```ocaml
let users_with_comment_counts db =
  Query.(
    build
      (let* stats, (author_id, cnt) =
         with_cte "comment_stats"
           (let* c = from Comments.table in
            let* () = group_by [ Expr.any c.@(Comments.author_id) ] in
            select_named
              Named.(
                let+ aid = field "author_id" c.@(Comments.author_id)
                and+ cnt = field "cnt" Expr.count_all in
                (aid, cnt)))
       in
       let* u = from Users.table in
       let* s =
         left_join_cte stats ~on:(fun s ->
             Expr.Ops.(s.@(author_id) = u.@(Users.id)))
       in
       let* () = order_by [ asc u.@(Users.id) ] in
       select
         (let+ name = expr u.@(Users.name)
          and+ total = expr (Expr.coalesce s.@?(cnt) (Expr.int 0)) in
          (name, total))))
  |> Blocking.query_all db
```

The exact same `select_named` block works inline as a derived table via
`from_subquery`, `join_subquery`, and `left_join_subquery`:

```ocaml
let* s, (_aid, cnt) =
  left_join_subquery
    (let* c = from Comments.table in
     let* () = group_by [ Expr.any c.@(Comments.author_id) ] in
     select_named
       Named.(
         let+ aid = field "author_id" c.@(Comments.author_id)
         and+ cnt = field "cnt" Expr.count_all in
         (aid, cnt)))
    ~on:(fun s (aid, _) -> Expr.Ops.(s.@(aid) = u.@(Users.id)))
```

### Recursive CTEs (`WITH RECURSIVE`)

Walk trees, hierarchies, and graphs (threaded comments, org charts, category
trees) with `with_recursive_cte`:

```ocaml
let reply_tree db root_id =
  Query.(
    build
      (let* tree, (cid, depth) =
         with_recursive_cte "thread"
           ~base:
             (let* c = from Comments.table in
              let* () = where Expr.Ops.(c.@(Comments.id) = Expr.int root_id) in
              select_named
                Named.(
                  let+ cid = field "id" c.@(Comments.id)
                  and+ depth = field "depth" (Expr.int 0) in
                  (cid, depth)))
           ~step:(fun self (cid, depth) ->
             let* p = from_cte self in
             let* c =
               join Comments.table ~on:(fun c ->
                   Expr.Ops.(c.@(Comments.reply_to) =? p.@(cid)))
             in
             select_named
               Named.(
                 let+ cid = field "id" c.@(Comments.id)
                 and+ depth = field "depth" Expr.Ops.(p.@(depth) + Expr.int 1) in
                 (cid, depth)))
       in
       let* t = from_cte tree in
       let* c =
         join Comments.table ~on:(fun c -> Expr.Ops.(c.@(Comments.id) = t.@(cid)))
       in
       let* () = order_by [ asc t.@(depth); asc c.@(Comments.id) ] in
       select
         (let+ depth = expr t.@(depth) and+ body = expr c.@(Comments.body) in
          (depth, body))))
  |> Blocking.query_all db
```

Pass `~union_all:false` to use `UNION` (cycle/duplicate elimination) instead of
`UNION ALL`.

### Set operations (`UNION`, `INTERSECT`, `EXCEPT`)

Combine any two `Qb.Query.t` values returning the same OCaml type `'a`:

```ocaml
let combined =
  Query.(union adults young |> with_limit 50 |> with_offset 0)
```

Available: `union`, `union_all`, `intersect`, `except`, `with_limit`,
`with_offset`.

---

## Expression writes and multi-table writes

### Single-table expression updates & upserts (`set_expr`)

`Qb.set_expr` lets you assign any `'a Expr.t` in `update`, `update_count`,
`update_returning`, `values`, and `On_conflict.do_update`:

```ocaml
(* Atomic increment *)
from Users.table
|> where Ops.(Users.id = 1)
|> Blocking.update db
     [ set_expr Users.age Expr.Ops.(Expr.col Users.age + Expr.int 1) ]

(* Upsert combining existing column with EXCLUDED *)
Blocking.upsert db
  On_conflict.(
    do_update
      ~target:[ col Users.id ]
      ~set:
        [
          set_expr Users.age
            Expr.Ops.(Expr.col Users.age + Expr.excluded Users.age);
        ]
      ())
  (values Users.table Ops.[ Users.id := 1; Users.name := "Ann"; Users.age := 5 ])
```

Inside `On_conflict.do_update ~set:...`, `Expr.col` is automatically qualified
with the target table's name (`"users"."age"`) so it never clashes with
`EXCLUDED."age"`.

### `INSERT INTO ... SELECT ...`

The target column selection (`('r, 'a) sel`) and the source query (`'a Query.t`)
must agree on `'a`, checking column types pairwise at compile time:

```ocaml
let src =
  Query.(
    build
      (let* u = from Users.table in
       let* () = where Expr.Ops.(u.@(Users.age) >= Expr.int 50) in
       select
         (let+ email = expr (Expr.coalesce u.@(Users.email) (Expr.text "none@x.org"))
          and+ age = expr u.@(Users.age) in
          Row.[ email; age ])))
in
Blocking.insert_select db Signups.table [ Signups.email; Signups.age ] src
```

### `UPDATE ... [FROM ...] WHERE ... [RETURNING ...]`

Like single-table `update`, `Query.update_from` refuses to run without at least
one `where` unless you explicitly pass `~all_rows:true`:

```ocaml
let plan =
  Query.(
    update_from Users.table (fun u ->
        let* c = from Comments.table in
        let* () =
          where
            Expr.Ops.(
              c.@(Comments.author_id) = u.@(Users.id)
              && c.@(Comments.body) = Expr.text "promote")
        in
        return [ set Users.age Expr.Ops.(u.@(Users.age) + Expr.int 1) ]))
in
Blocking.update_from_returning db plan [ Users.id; Users.age ]
```

Use `update_from`, `update_from_count`, or `update_from_returning`. If you omit
`from` inside the callback and filter with `exists` / `in_query`, the generated
`UPDATE "t" AS "t0" SET ... WHERE ...` works across Postgres, SQLite, and MySQL.

### `DELETE FROM ... [USING ...] WHERE ... [RETURNING ...]`

Also requires at least one `where` unless `~all_rows:true` is passed:

```ocaml
(* Postgres: DELETE FROM ... USING ... *)
let pg_plan =
  Query.(
    delete_using Comments.table (fun c ->
        let* u = from Users.table in
        where
          Expr.Ops.(
            u.@(Users.id) = c.@(Comments.author_id)
            && u.@(Users.age) < Expr.int 18)))

(* Portable across Postgres, SQLite, and MySQL: omit [from] and use [exists] *)
let portable_plan =
  Query.(
    delete_using Comments.table (fun c ->
        where
          (exists
             (sub
                (let* u = from Users.table in
                 where
                   Expr.Ops.(
                     u.@(Users.id) = c.@(Comments.author_id)
                     && u.@(Users.age) < Expr.int 18))))))
```

Run with `delete_using`, `delete_using_count`, or `delete_using_returning`.

---

## Relations (`Qb.Rel`) and batched eager loading

When you want nested data (e.g. each user with their list of comments) without
duplicate parent rows or N+1 queries, define typed relations with `Qb.Rel` (or
let `qb-pg-parse gen` generate them for you):

```ocaml
let user_comments =
  Rel.has_many ~parent_key:Users.id ~child:Comments.table
    ~child_key:Comments.author_id

let comment_author =
  Rel.belongs_to ~child_key:Comments.author_id ~parent:Users.table
    ~parent_key:Users.id

(* For nullable foreign keys (e.g. Comments.reply_to : int option): *)
let comment_replies =
  Rel.has_many_opt ~parent_key:Comments.id ~child:Comments.table
    ~child_key:Comments.reply_to

let comment_parent =
  Rel.belongs_to_opt ~child_key:Comments.reply_to ~parent:Comments.table
    ~parent_key:Comments.id
```

Load related rows in a single batched `IN` query:

```ocaml
(* 1. Fetch parents (1 query) *)
let* users = from Users.table |> order_by [ asc Users.id ] |> Blocking.all db in

(* 2. Batch-load children for all parents (1 query; 0 queries if [users = []]) *)
let* users_with_comments =
  Blocking.load_many db user_comments users
    ~where:Ops.(Comments.body <> "")
    ~order_by:[ desc Comments.id ]
(* : (Users.t * Comments.t list) list *)

(* Many-to-one loading (1 query; 0 queries if all keys are None) *)
let* comments_with_author =
  Blocking.load_one db comment_author comments
(* : (Comments.t * Users.t option) list *)

(* 1-to-1 loading (1 query) *)
let* users_with_profile = Blocking.load_has_one db user_profile users

(* Many-to-many loading through a join table (1 JOIN query) *)
let* posts_with_tags =
  Blocking.load_many_to_many db posts_tags posts
    ~order_by:(fun t -> [ Expr.asc Query.(t.@(Tags.name)) ])
```

### Multi-level nested loading (`Rel.flatten` / `Rel.flatten_opt`)

Chain arbitrary nesting depths (`users -> posts -> comments`) in 1 query per
level without manual flattening or re-grouping:

```ocaml
let* users_posts = Blocking.load_many db Users.posts users in
let flat_posts, reattach = Rel.flatten users_posts in
let* posts_comments = Blocking.load_many db Posts.comments flat_posts in
let nested = reattach posts_comments
(* : (Users.t * (Posts.t * Comments.t list) list) list — 3 queries total *)
```

`Rel.flatten_opt` does the same through `load_one` / `load_has_one` results.
All relation loaders work identically on Postgres, SQLite, MySQL,
`Qb.Blocking`, and `Qb_lwt`.

When `qb-pg-parse gen` sees a join table whose composite primary key is exactly
two non-null foreign keys pointing to distinct tables (e.g. `post_tags`), it
also generates both `Qb.Rel.many_to_many` directions (`posts_tags` and
`tags_posts`) inside `module Relations`.

---

## Generated helpers from `qb-pg-parse`

When you generate your schema with `qb-pg-parse gen migrations -o schema.ml`,
two multi-table helpers are emitted automatically:

### 1. Scoped table handles (`from`, `join`, `left_join`, `cols` / `opt_cols`)

Every generated table module gets `from`, `join ~on`, `left_join ~on`, and
`cols` / `opt_cols` (exposing `#ref`, `#whole`, and one method per column):

```ocaml
open Schema

let account_articles db =
  Qb.Q.(
    build ~name:"account_articles"
      (let* a = Accounts.from in
       let* art =
         Articles.left_join ~on:(fun art -> art#account_id = a#id)
       in
       where (a#active =. true)
       &> order_by [ asc a#id; nulls_last (asc art#title) ]
       &> select3 a#email art#title art#rating))
  |> Qb.Blocking.query_all db
```

You can also compose relation joins directly with `map_build`:

```ocaml
let* a = Accounts.from in
let* art =
  left_join_many a#ref Relations.accounts_articles
  |> map_build Articles.opt_cols
in
select (let+ acct = a#whole and+ art = art#whole in (acct, art))
```

`opt_cols` inspects each column's `NOT NULL` status from your migrations and
uses `.@?()` for non-null columns and `.@??()` for nullable columns
automatically.

### 2. Automatic `Relations` module

`qb-pg-parse` tracks `FOREIGN KEY` / `REFERENCES` constraints across:

- inline column `REFERENCES target (col)` (or `REFERENCES target` targeting the
  primary key),
- table-level `FOREIGN KEY (col) REFERENCES target (col)`,
- `ALTER TABLE ... ADD CONSTRAINT ... FOREIGN KEY ...`,
- `ALTER TABLE ... RENAME TO ...`, `RENAME COLUMN`, `DROP COLUMN`, and
  `DROP CONSTRAINT`.

For every resolvable single-column foreign key between generated tables,
`qb-pg-parse gen` emits a `Relations` module at the bottom of the file (placed
after all table modules so forward, circular, and self-referential foreign keys
always compile):

```ocaml
(* Generated from: articles.account_id BIGINT NOT NULL REFERENCES accounts (id) *)
module Relations = struct
  let articles_account =
    Qb.Rel.belongs_to ~child_key:Articles.account_id ~parent:Accounts.table
      ~parent_key:Accounts.id

  let accounts_articles =
    Qb.Rel.has_many ~parent_key:Accounts.id ~child:Articles.table
      ~child_key:Articles.account_id
end
```

Nullable foreign keys emit `belongs_to_opt` and `has_many_opt`. If your schema
already has a table named `relations`, the module is named `Qb_relations`.

---

## Running queries and inspecting SQL

All execution functions live in `Qb.S`, so they work identically on
`Qb.Blocking`, `Qb_lwt`, and `Qb.Make`:

| Function | Returns | Notes |
| --- | --- | --- |
| `query_all db q` | `'a list` | All matching rows |
| `query_first db q` | `'a option` | Applies `LIMIT 1` (or `min 1 q.limit`) |
| `query_one db q` | `'a` | Expects exactly one row |
| `query_count db q` | `int` | Row count (subquery-wrapped when grouped / distinct / compound) |
| `insert_select db tbl sel q` | `unit` | `INSERT INTO tbl (...) SELECT ...` |
| `update_from db plan` | `unit` | `UPDATE ... [FROM ...] WHERE ...` |
| `update_from_count db plan` | `int` | Affected row count |
| `update_from_returning db plan sel` | `'a list` | Updated rows (Postgres & SQLite) |
| `delete_using db plan` | `unit` | `DELETE FROM ... [USING ...] WHERE ...` |
| `delete_using_count db plan` | `int` | Affected row count |
| `delete_using_returning db plan sel` | `'a list` | Deleted rows (Postgres & SQLite) |
| `load_many ?where ?order_by db rel parents` | `('p * 'c list) list` | Batched 1-to-many eager load |
| `load_one ?where db rel children` | `('c * 'p option) list` | Batched many-to-1 eager load |

Inspect rendered SQL for logging, tests, or `Qb_pg_parse.Pg.check ~caqti:true`:

- `Query.to_sql ?dialect q`
- `Query.count_sql ?dialect q`
- `Query.insert_select_sql ?dialect tbl sel q`
- `Query.update_from_sql ?dialect plan`
- `Query.update_from_returning_sql ?dialect plan sel`
- `Query.delete_using_sql ?dialect plan`
- `Query.delete_using_returning_sql ?dialect plan sel`

---

## Dialect differences

| Feature | Postgres | SQLite | MySQL |
| --- | --- | --- | --- |
| `ilike` | `ILIKE` | `LOWER(a) LIKE LOWER(b)` | `LOWER(a) LIKE LOWER(b)` |
| `concat` / `^` | `(a \|\| b)` | `(a \|\| b)` | `CONCAT(a, b)` |
| `length` | `CHAR_LENGTH(a)` | `LENGTH(a)` | `CHAR_LENGTH(a)` |
| `now ()` | `NOW()` | `CURRENT_TIMESTAMP` | `NOW()` |
| `sum_int` / `sum_int64` | `CAST(SUM(e) AS BIGINT)` | `SUM(e)` | `SUM(e)` |
| `avg` | `CAST(AVG(e) AS DOUBLE PRECISION)` | `CAST(AVG(e) AS REAL)` | `CAST(AVG(e) AS DOUBLE)` |
| `nulls_first` / `nulls_last` | `NULLS FIRST` / `NULLS LAST` | `NULLS FIRST` / `NULLS LAST` | `e IS NULL DESC/ASC, e ...` |
| `offset` without `limit` | `OFFSET m` | `LIMIT -1 OFFSET m` | `LIMIT 18446744073709551615 OFFSET m` |
| `for_update` / `for_share` | Emitted | Omitted | Emitted |
| `update_from_returning` / `delete_using_returning` | Alias-qualified `RETURNING "t0"."c"` | Bare `RETURNING "c"` | Not supported by MySQL |
| `delete_using` with `USING` | Supported | Use `exists` without `from` | Not supported; use `exists` |

---

## Troubleshooting

- **`Type Comments.t is not compatible with type Users.t` on `.@()`:**
  You indexed handle `u : Users.t ref` with a column from `Comments`. Use the
  handle bound for `Comments` instead.
- **Getting `'a option option` from a `left_join` column:**
  You used `c.@?(col)` on a column that is already `'a option`. Use
  `c.@??(col)` instead (or `Comments.opt_cols c` on generated schemas).
- **`Invalid_argument "Qb.Query.in_query: subquery must select a single column"`:**
  Use `select_one e` instead of `select (expr e)` when building a query for
  `in_query`, `not_in_query`, `scalar`, or `scalar_opt`.
- **`Invalid_argument "Qb.Query.update_from: missing where"` (or `delete_using`):**
  Every `update_from` and `delete_using` requires at least one `where` clause
  by default. Pass `~all_rows:true` if you really intend to affect every row.
- **`Invalid_argument "Qb.Query: table handle used outside the query that introduced it"`:**
  A `ref` captured from one top-level query was referenced inside an unrelated
  query that doesn't nest inside the first. Handles may only be reused inside
  subqueries of the query that created them.
- **`Invalid_argument "Qb.Query.whole: only base tables can be selected whole"`:**
  `whole` / `whole_opt` decode a base `'r table`. For a CTE or derived table,
  select the exported `field` columns via `expr s.@(col)` / `expr s.@?(col)`.
