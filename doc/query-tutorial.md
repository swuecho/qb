# Tutorial: multi-table queries with `Qb.Query` and `Qb.Rel`

A short, hands-on walkthrough of `qb`'s multi-table builder—from two tables to
self-joins, `GROUP BY`, dynamic filters, recursive CTEs, and N+1-free eager
loading—running on an in-memory SQLite database (with zero setup).

You can run the full example right away:

```sh
dune exec ./example/multi.exe
```

For the complete API reference, see [the multi-table reference](query.md).

---

## 1. Define your tables and relations

Start with two tables: `Users` and `Comments` (where a comment can optionally
reply to another comment via `reply_to`).

```ocaml
open Qb
open Qb.Blocking

module Users = struct
  type t = { id : int; name : string; age : int }

  let id = column "id" int (fun u -> u.id) ~primary_key:true
  let name = column "name" text (fun u -> u.name)
  let age = column "age" int (fun u -> u.age)

  let table =
    table "users"
      (let+ id = col id and+ name = col name and+ age = col age in
       { id; name; age })
end

module Comments = struct
  type t = { id : int; author_id : int; body : string; reply_to : int option }

  let id = column "id" int (fun c -> c.id) ~primary_key:true
  let author_id = column "author_id" int (fun c -> c.author_id)
  let body = column "body" text (fun c -> c.body)
  let reply_to = column "reply_to" (nullable int) (fun c -> c.reply_to)

  let table =
    table "comments"
      (let+ id = col id
       and+ author_id = col author_id
       and+ body = col body
       and+ reply_to = col reply_to in
       { id; author_id; body; reply_to })
end

(* Describe the foreign-key relationships once (or let `qb-pg-parse gen`
   generate them automatically from `REFERENCES` in your migrations). *)
let user_comments =
  Rel.has_many ~parent_key:Users.id ~child:Comments.table
    ~child_key:Comments.author_id

let comment_author =
  Rel.belongs_to ~child_key:Comments.author_id ~parent:Users.table
    ~parent_key:Users.id
```

Seed a few rows on SQLite:

```ocaml
let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let* db = Caqti_blocking.connect (Uri.of_string "sqlite3::memory:") in
let* () = create_table db Users.table in
let* () = create_table db Comments.table in
let* () =
  insert_many db Users.table
    [
      { id = 1; name = "Ann"; age = 34 };
      { id = 2; name = "Bob"; age = 51 };
      { id = 3; name = "Cy"; age = 17 };
    ]
in
let* () =
  insert_many db Comments.table
    [
      { id = 10; author_id = 1; body = "RFC: new query builder"; reply_to = None };
      { id = 11; author_id = 2; body = "Looks great!"; reply_to = Some 10 };
      { id = 12; author_id = 1; body = "Thanks, shipping it."; reply_to = Some 11 };
    ]
in
```

---

## 2. Your first multi-table query

Inside `Qb.Query.(build (...))`, each `from` or `join` binds a **table handle**
using `let*`. Index a handle with `.@()` to turn a column into a SQL
expression:

```ocaml
let comments_with_authors db =
  Query.(
    build
      (let* c = from Comments.table in
       let* u =
         join Users.table ~on:(fun u ->
             Expr.Ops.(u.@(Users.id) = c.@(Comments.author_id)))
       in
       let* () = where Expr.Ops.(u.@(Users.age) >= Expr.int 18) in
       let* () = order_by [ asc c.@(Comments.id) ] in
       select
         (let+ author = expr u.@(Users.name)
          and+ body = expr c.@(Comments.body) in
          (author, body))))
  |> query_all db
(* : ((string * string) list, ...) result *)
```

- Mixing up handles (`u.@(Comments.body)`) is a **compile error**.
- Using `comment_author` from step 1, you can shorten the join to
  `let* u = join_one c comment_author in`.

---

## 3. Left joins, self-joins, and nullability

Now pair every comment with its author **and** (optionally) the parent comment
it replied to. Notice the two operators for outer-joined handles:

- `p.@?(Comments.body)` turns a non-null `string` column into `string option`.
- `p.@??(Comments.reply_to)` keeps an already-nullable `int option` column as
  `int option` (never `int option option`).

```ocaml
let with_parent db =
  Query.(
    build
      (let* c = from Comments.table in
       let* u = join_one c comment_author in
       let* parent =
         left_join Comments.table ~on:(fun p ->
             Expr.Ops.(c.@(Comments.reply_to) =? p.@(Comments.id)))
       in
       let* () = order_by [ asc c.@(Comments.id) ] in
       select
         (let+ author = expr u.@(Users.name)
          and+ body = expr c.@(Comments.body)
          and+ parent_body = expr parent.@?(Comments.body) in
          (author, body, parent_body))))
  |> query_all db
```

Because `Comments.table` is joined to itself, `qb` automatically assigns fresh
aliases (`"t0"` and `"t2"`)—no manual aliasing required.

---

## 4. Aggregates and `GROUP BY`

Count comments per user, including users with zero comments, using
`left_join_many`:

```ocaml
let comment_counts db =
  Query.(
    build
      (let* u = from Users.table in
       let* c = left_join_many u user_comments in
       let* () = group_by [ Expr.any u.@(Users.id); Expr.any u.@(Users.name) ] in
       let* () =
         order_by [ desc (Expr.count c.@?(Comments.id)); asc u.@(Users.name) ]
       in
       select
         (let+ name = expr u.@(Users.name)
          and+ total = expr (Expr.count c.@?(Comments.id)) in
          (name, total))))
  |> query_all db
(* [("Ann", 2); ("Bob", 1); ("Cy", 0)] *)
```

Inspect the SQL with `Query.to_sql` (it renders dialect-specific `CAST`s for
`sum_int` / `avg` on Postgres automatically):

```sql
SELECT "t0"."name", COUNT("t1"."id")
FROM "users" AS "t0"
LEFT JOIN "comments" AS "t1" ON "t1"."author_id" = "t0"."id"
GROUP BY "t0"."id", "t0"."name"
ORDER BY COUNT("t1"."id") DESC, "t0"."name" ASC
```

---

## 5. Dynamic filters

When filters are decided at run time (like a search endpoint), use `opt` and
`when_`:

```ocaml
let search_users db ?min_age ?(only_commenters = false) () =
  Query.(
    build
      (let* u = from Users.table in
       let* () =
         opt min_age (fun a -> where Expr.Ops.(u.@(Users.age) >= Expr.int a))
       in
       let* () =
         when_ only_commenters (fun () ->
             where
               (exists
                  (sub
                     (let* c = from Comments.table in
                      where Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id))))))
       in
       let* () = order_by [ asc u.@(Users.name) ] in
       select (whole u)))
  |> query_all db
```

---

## 6. Walking a tree with a recursive CTE

To print a reply thread indented by depth, use `with_recursive_cte`:

```ocaml
let thread_tree db =
  Query.(
    build
      (let* tree, (cid, depth) =
         with_recursive_cte "thread"
           ~base:
             (let* c = from Comments.table in
              let* () = where Expr.Ops.(is_null c.@(Comments.reply_to)) in
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
       let* u = join_one c comment_author in
       let* () = order_by [ asc t.@(depth); asc c.@(Comments.id) ] in
       select
         (let+ depth = expr t.@(depth)
          and+ author = expr u.@(Users.name)
          and+ body = expr c.@(Comments.body) in
          (depth, author, body))))
  |> query_all db
```

Output:

```text
[Ann] RFC: new query builder
  [Bob] Looks great!
    [Ann] Thanks, shipping it.
```

---

## 7. Nested data without N+1 (`load_many`)

Instead of flattening parents and children through a SQL join (which repeats
parent columns), load parents and then batch-load their children in one `IN`
query:

```ocaml
let* users = from Users.table |> order_by [ asc Users.id ] |> all db in
let* users_with_comments =
  load_many db user_comments users ~order_by:[ asc Comments.id ]
(* : (Users.t * Comments.t list) list — 2 queries total *)
```

Output:

```text
Ann  -> [RFC: new query builder; Thanks, shipping it.]
Bob  -> [Looks great!]
Cy   -> []
```

---

## 8. Expression updates

Use `set_expr` for atomic SQL updates without reading the row first:

```ocaml
let* () =
  from Users.table
  |> where Ops.(Users.id = 1)
  |> update db [ set_expr Users.age Expr.Ops.(Expr.col Users.age + Expr.int 1) ]
```

Or update rows filtered by another table with `Query.update_from`:

```ocaml
let* updated =
  Query.(
    update_from Users.table (fun u ->
        let* () =
          where
            (exists
               (sub
                  (let* c = from Comments.table in
                   where Expr.Ops.(c.@(Comments.author_id) = u.@(Users.id)))))
        in
        return [ set Users.age Expr.Ops.(u.@(Users.age) + Expr.int 1) ]))
  |> update_from_count db
```

---

## 9. Even less boilerplate with `qb-pg-parse gen`

If you generate your table modules from SQL migrations (`qb-pg-parse gen migrations -o schema.ml`),
`qb` generates both **`Relations`** (from `REFERENCES` constraints) and scoped
**`cols` / `opt_cols`** accessors automatically:

```ocaml
open Schema

let account_articles db =
  Qb.Query.(
    build
      (let* a = from Accounts.table in
       let* art = left_join_many a Relations.accounts_articles in
       let a' = Accounts.cols a and art' = Articles.opt_cols art in
       let* () = order_by [ asc a'#id; nulls_last (asc art'#title) ] in
       select
         (let+ email = expr a'#email
          and+ title = expr art'#title
          and+ rating = expr art'#rating in
          (email, title, rating))))
  |> Qb.Blocking.query_all db
```

With `opt_cols`, nullable columns on outer joins stay `'a option` automatically
— you never have to remember `.@?()` vs. `.@??()`.

---

## Next steps

- [Multi-table query builder reference](query.md): every function, `Qb.Raw`
  fragments, CTEs, set operations, row locking, dialect differences, and
  troubleshooting.
- [`[%sql]` tutorial](tutorial.md): writing inline Postgres SQL checked against
  your migrations at compile time.
- [`example/multi.ml`](../example/multi.ml): the complete runnable program for
  this tutorial.
