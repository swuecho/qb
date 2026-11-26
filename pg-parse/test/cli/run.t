CLI behaviour for qb-pg-parse: init, check, gen, and queries.

`init` prints the matching dune stanzas for custom folder and module names.

  $ qb-pg-parse init -m db/migrations --module Db_schema
  ; For the dune file of the folder holding db/migrations/.
  
  ; Table modules from the migrations: db_schema.ml, the module Db_schema.
  (rule
   (targets db_schema.ml)
   (deps
    (glob_files db/migrations/*.sql))
   (action
    (run qb-pg-parse gen %{deps} -o %{targets})))
  
  ; [%sql "..."] checked against the same migrations: add the preprocess and
  ; preprocessor_deps fields to your own executable or library.
  (executable
   (name main)
   (libraries qb caqti-driver-postgresql)
   (preprocess
    (pps qb-ppx -- -qb-schema db/migrations -qb-tables Db_schema))
   (preprocessor_deps
    (source_tree db/migrations)))

`check` validates Postgres SQL and (with --caqti) Caqti ? placeholders.

  $ cat > good.sql <<EOF
  > CREATE TABLE users (id BIGSERIAL PRIMARY KEY, email TEXT NOT NULL);
  > EOF
  $ qb-pg-parse check good.sql

  $ printf 'SELECT id FROM users WHERE email = ?' > caqti.sql
  $ qb-pg-parse check --caqti caqti.sql

  $ printf 'SELECT FROM WHERE' > bad.sql
  $ qb-pg-parse check bad.sql
  bad.sql: line 1: syntax error at or near "FROM"
    SELECT FROM WHERE
           ^
  [1]

`gen` and `queries` respect --bigint int64 and --tables.

  $ qb-pg-parse gen good.sql --bigint int64 -o schema.ml
  $ grep -c 'id : int64;' schema.ml
  1
  $ grep -c 'Qb\.int64' schema.ml
  1

  $ cat > queries.sql <<EOF
  > -- name: by_id
  > SELECT * FROM users WHERE id = :id;
  > EOF
  $ qb-pg-parse queries good.sql -q queries.sql --tables Schema --bigint int64 -o queries.ml
  $ grep -c 'Qb\.Stmt\.make ~name:"by_id"' queries.ml
  1
  $ grep -c 'Qb\.Stmt\.arg Qb\.int64 id' queries.ml
  1
  $ grep -c 'Schema\.Users\.table' queries.ml
  1
