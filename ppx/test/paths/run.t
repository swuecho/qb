A project with its migrations at the top and a query two folders down:

  $ mkdir -p proj/schema proj/src/area
  $ touch proj/dune-project proj/src/dune
  $ cat > proj/schema/001_users.sql <<EOF
  > CREATE TABLE users (id bigint PRIMARY KEY, name text NOT NULL);
  > EOF
  $ cat > proj/src/area/q.ml <<EOF
  > let by_id id = [%sql "SELECT name FROM users WHERE id = :id"]
  > EOF
  $ expands () { grep -c 'Qb.Stmt.make' "$1"; grep -o 'qb: [^"]*' "$1"; true; }

As dune runs it, from the project root with the file's path from there:

  $ (cd proj && ../driver.exe -qb-schema schema/ -impl src/area/q.ml -o ../out.ml)
  $ expands out.ml
  1

As merlin runs it, from the file's folder with its name alone:

  $ (cd proj/src/area && ../../../driver.exe -qb-schema schema -impl q.ml -o ../../../out.ml)
  $ expands out.ml
  1

The search stops at the project root: a schema above it isn't found.

  $ mkdir -p proj/inner/src && touch proj/inner/dune-project
  $ cp proj/src/area/q.ml proj/inner/src/q.ml
  $ (cd proj/inner/src && ../../../driver.exe -qb-schema schema -impl q.ml -o ../../../out.ml)
  $ expands out.ml
  0
  qb: schema: schema: No such file or directory (looked in src, .; is it in preprocessor_deps?)

An empty migrations folder says so directly, rather than claiming -qb-schema
was omitted.

  $ mkdir -p proj/empty_schema
  $ (cd proj && ../driver.exe -qb-schema empty_schema -impl src/area/q.ml -o ../out.ml)
  $ expands out.ml
  0
  qb: schema: no *.sql files found in empty_schema

-qb-bigint int64 reads BIGINT parameters and columns as int64.

  $ (cd proj && ../driver.exe -qb-schema schema -qb-bigint int64 -impl src/area/q.ml -o ../out.ml)
  $ grep -c 'Qb\.int64' out.ml
  1
