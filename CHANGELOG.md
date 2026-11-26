# Changelog

All notable changes to qb are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and the packages
follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- **`qb`**: a typed, Drizzle-style query builder for single-table CRUD on
  Caqti. Select, insert, update, delete and `CREATE TABLE`, with compile-time
  checks on columns, row types and conditions. Includes changesets, statement
  caching, logging, transactions, prepared statements, `RETURNING`, batch
  inserts and a two-table join.
- **`qb-lwt`**: the same queries, changesets and transactions on
  `Caqti_lwt` connections.
- **`qb-pg-parse`**: bindings to libpg_query to check SQL, replay migrations
  into a schema, generate qb table modules, and type SQL queries.
- **`qb-ppx`**: `[%sql "..."]` queries checked and typed against your
  migrations at compile time.
- **`qb-migrate`**: apply and verify `migrations/*.sql` against PostgreSQL,
  as a library or with the `qb-migrate` command.
- A tutorial, an SQL reference and a migrations reference under `doc/`, plus
  runnable examples under `example/`.
- CI on OCaml 4.14 and 5.4, formatting checks, and odoc builds.

### Notes

- Supports Caqti 2.x. Caqti 3 moved the Caqti 2 API behind `caqti.classic`;
  qb has not been ported yet, so the opam packages require `caqti < 3.0`.
- PostgreSQL tests are optional and run when `QB_TEST_PG_URL` is set.
