# Changelog

All notable changes to qb are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the packages
follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **`qb`**: a typed, Drizzle-style query builder for single-table CRUD on
  Caqti. Select, insert, update, delete and `CREATE TABLE`, with compile-time
  checks on columns, row types and conditions. Includes changesets, thread- and
  domain-safe statement caching, logging (including named statements),
  transactions, prepared statements, `RETURNING`, `distinct`,
  `nulls_first` / `nulls_last` ordering, `insert_many`, `insert_many_values`,
  single-table upserts (`On_conflict`, `upsert`, `upsert_many`), and a
  two-table join.
- **`qb-lwt`**: the same queries, changesets and transactions on
  `Caqti_lwt` connections.
- **`qb-pg-parse`**: bindings to libpg_query (releasing the OCaml runtime lock
  while parsing) to check SQL, replay migrations into a schema, generate qb
  table modules (including `by_pk` for both single and composite primary keys),
  and type SQL queries. `gen`, `queries`, and `check` accept directories as
  well as individual `.sql` files, and `init` supports `--bigint int64`.
- **`qb-ppx`**: `[%sql "..."]` and `[%sql.obj "..."]` queries checked and typed
  against your migrations at compile time, including `-qb-bigint int|int64` and
  automatic `file.ml:line` statement names in `Qb.src` debug logs.
- **`qb-migrate`**: apply and verify `migrations/*.sql` against PostgreSQL
  (with SHA-256 checksums, `pg_dump` `search_path` and tuple-returning
  statement support, and atomic `baseline`), as a library or with the
  `qb-migrate` command.
- A tutorial, an SQL reference and a migrations reference under `doc/`, plus
  runnable examples under `example/`.
- CI on OCaml 4.14 and 5.4 (with cached `libpg_query` builds), formatting
  checks, CLI cram tests, and odoc builds.

### Notes

- Supports Caqti 2.x. Caqti 3 moved the Caqti 2 API behind `caqti.classic`;
  qb has not been ported yet, so the opam packages require `caqti < 3.0`.
- PostgreSQL tests are optional and run when `QB_TEST_PG_URL` is set.
