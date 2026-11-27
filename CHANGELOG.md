# Changelog

All notable changes to qb are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the packages
follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **`qb`**: a typed, Drizzle-style query builder for single-table CRUD on
  Caqti. Select, insert, update, delete and `CREATE TABLE`, with compile-time
  checks on columns, row types and conditions. Includes changesets (with
  `update_change` / `update_change_opt`), thread- and domain-safe statement
  caching, logging (including named statements), composable nested savepoint
  transactions, prepared statements, `RETURNING`, `distinct`, `where_opt` /
  `Ops.all_of` / `Ops.any_of`, `nulls_first` / `nulls_last` ordering,
  `insert_many`, `insert_many_values`, single-table upserts (`On_conflict`,
  `upsert`, `upsert_many`), deduplicated column assignments, `Stmt` placeholder
  validation, and a two-table join.
- **`qb` multi-table builder**: `Qb.Expr` (typed SQL expressions, scalar
  functions, aggregates with `FILTER (WHERE ...)`, window functions
  `row_number`/`rank`/`dense_rank`/`lag`/`lead`/`first_value`/`last_value`/`over`,
  `Expr.Keyset` tuple comparisons for cursor pagination, `Expr.Json` /
  `Expr.Pg` helpers, `CASE`, `COALESCE`, `EXCLUDED`), `Qb.Raw` (typed raw SQL
  fragments with bound parameters), `set_expr` (expression assignments in
  updates and upserts), `Qb.Query` (scoped `let*` multi-table joins across
  arbitrary tables, `DISTINCT ON`, `GROUP BY` / `HAVING`, correlated `EXISTS` /
  `IN` / scalar subqueries, CTEs via `with_cte` and `with_recursive_cte`,
  derived tables, `when_` / `opt` dynamic clauses, `UNION` / `INTERSECT` /
  `EXCEPT`, `FOR UPDATE` / `FOR SHARE` row locking, and multi-table
  `insert_select`, `update_from` / `update_from_returning`, `delete_using` /
  `delete_using_returning`), and `Qb.Rel` (typed `has_many`, `has_one`,
  `belongs_to`, and `many_to_many` relations with relation-driven joins,
  `Rel.flatten` / `Rel.flatten_opt` for multi-level nesting, and batched
  `load_many` / `load_one` / `load_has_one` / `load_many_to_many` eager
  loading).
- **`qb-lwt`**: the same queries, changesets and transactions on
  `Caqti_lwt` connections.
- **`qb-pg-parse`**: bindings to libpg_query (releasing the OCaml runtime lock
  while parsing) to check SQL, replay migrations into a schema (including
  `FOREIGN KEY` constraints across `CREATE`, `ALTER`, `RENAME`, and `DROP`),
  generate qb table modules (including `by_pk` for both single and composite
  primary keys, scoped `cols` / `opt_cols` accessors for `Qb.Query`, and a
  `Relations` module of typed `Qb.Rel` descriptors including auto-detected
  `many_to_many` join tables),
  and type SQL queries (including simple/searched `CASE`, `NULLIF`,
  `GREATEST`/`LEAST`, and boolean parameter inference). `gen`, `queries`, and
  `check` accept directories as well as individual `.sql` files, and `init`
  supports `--bigint int64`.
- **`qb-ppx`**: `[%sql "..."]` and `[%sql.obj "..."]` queries checked and typed
  against your migrations at compile time, including `-qb-bigint int|int64`,
  full-token error spans (with exact single-line and delimited-string offsets),
  and automatic `file.ml:line` statement names in `Qb.src` debug logs.
- **`qb-migrate`**: apply and verify `migrations/*.sql` against PostgreSQL
  (with SHA-256 checksums, `pg_dump` `search_path` and tuple-returning
  statement support, atomic `baseline`, read-only `plan` / `up --dry-run`, and
  `status --check` for CI/deploy gates), as a library or with the `qb-migrate`
  command.
- A tutorial, an SQL reference and a migrations reference under `doc/`, plus
  runnable examples under `example/`.
- CI on OCaml 4.14 and 5.4 (with cached `libpg_query` builds), formatting
  checks, CLI cram tests, and odoc builds.

### Notes

- Supports Caqti 2.x. Caqti 3 moved the Caqti 2 API behind `caqti.classic`;
  qb has not been ported yet, so the opam packages require `caqti < 3.0`.
- PostgreSQL tests are optional and run when `QB_TEST_PG_URL` is set.
