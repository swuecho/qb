(lang dune 3.0)

(name qb)

(generate_opam_files true)

(authors "Hao Wu")

(maintainers "Hao Wu")

(package
 (name qb)
 (synopsis "A typed, Drizzle-style query builder for single-table CRUD on Caqti")
 (description
  "Define a table once - a record type, one column per field - and get typed select, insert, update, delete and CREATE TABLE. Every column and condition carries its row type, so a column of one table cannot be used in a query on another.")
 (depends
  (ocaml (>= 4.14))
  dune
  (angstrom (>= 0.14))
  ; Caqti_type.unify, used by the statement cache
  (caqti (>= 2.3))
  logs
  (mtime (>= 2.0))
  ptime
  uri
  (caqti-driver-sqlite3 :with-test)
  (caqti-driver-postgresql :with-test)
  (alcotest :with-test)))

(package
 (name qb-pg-parse)
 (synopsis "Postgres's own parser for qb: check SQL, import schemas, generate tables and typed queries")
 (description
  "Binds libpg_query, the parser extracted from Postgres, to check SQL, read CREATE TABLE and ALTER TABLE migrations into a schema, and generate qb table modules from it. Needs libpg_query installed (e.g. brew install libpg_query); set LIBPG_QUERY_PREFIX if it is somewhere unusual.")
 (depends
  (ocaml (>= 4.14))
  dune
  dune-configurator
  (angstrom (>= 0.14))
  (yojson (>= 2.0))
  (cmdliner (>= 1.1))
  (qb (and :with-test (= :version)))
  (caqti-driver-sqlite3 :with-test)
  (caqti-driver-postgresql :with-test)
  (alcotest :with-test)))

(package
 (name qb-lwt)
 (synopsis "Run qb queries on Caqti_lwt connections")
 (depends
  (ocaml (>= 4.14))
  dune
  (qb (= :version))
  (caqti (>= 2.3))
  caqti-lwt
  lwt
  (caqti-driver-sqlite3 :with-test)
  (caqti-driver-postgresql :with-test)
  (uri :with-test)
  (alcotest :with-test)))

(package
 (name qb-ppx)
 (synopsis "SQL written inline in OCaml, typed from your Postgres schema")
 (description
  "[%sql \"SELECT ...\"] is checked against your migrations when the file is compiled, and becomes a qb statement whose parameter and row types are worked out from the schema.")
 (depends
  (ocaml (>= 4.14))
  dune
  (ppxlib (>= 0.36))
  (qb-pg-parse (= :version))
  (qb (and :with-test (= :version)))
  (caqti-driver-sqlite3 :with-test)
  (caqti-driver-postgresql :with-test)
  (alcotest :with-test)))

(package
 (name qb-migrate)
 (synopsis "Apply the migrations qb is typed from to a Postgres database")
 (description
  "Runs migrations/*.sql in order, each once and in a transaction, records them in a qb_migrations table, and checks that the database matches the schema [%sql] was compiled against. A library to call when a program starts, and the qb-migrate command.")
 (depends
  (ocaml (>= 4.14))
  dune
  (qb-pg-parse (= :version))
  (caqti (>= 2.3))
  caqti-driver-postgresql
  ptime
  uri
  (cmdliner (>= 1.1))
  (alcotest :with-test)))
