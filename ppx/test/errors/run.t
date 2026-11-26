A project with one table, and a file per mistake.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ mkdir schema
  $ cat > schema/001_users.sql <<EOF
  > CREATE TABLE users (
  >   id BIGSERIAL PRIMARY KEY,
  >   name TEXT NOT NULL,
  >   email TEXT NOT NULL,
  >   age INTEGER
  > );
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (name mistakes)
  >  (libraries qb)
  >  (preprocess (pps qb-ppx -- -qb-schema schema))
  >  (preprocessor_deps (source_tree schema)))
  > EOF
  $ build () { dune build ./.mistakes.objs/byte/mistakes__$1.cmo 2>&1 | grep -v '^ *$'; }

A :name with no variable of that name is reported at the :name, here on
the second line of the SQL.

  $ cat > unbound.ml <<EOF
  > let q =
  >   [%sql
  >     {|SELECT id FROM users
  >       WHERE age > 18 AND name = :name|}]
  > EOF
  $ build Unbound
  File "unbound.ml", line 4, characters 32-37:
  4 |       WHERE age > 18 AND name = :name|}]
                                      ^^^^^
  Error: Unbound value name

So is a variable of the wrong type.

  $ cat > wrong_type.ml <<EOF
  > let q = let name = 3 in [%sql "SELECT id FROM users WHERE name = :name"]
  > EOF
  $ build Wrong_type
  File "wrong_type.ml", line 1, characters 65-70:
  1 | let q = let name = 3 in [%sql "SELECT id FROM users WHERE name = :name"]
                                                                       ^^^^^
  Error: The value name has type int but an expression was expected of type
           string

A misspelt column, with what was probably meant.

  $ cat > misspelt.ml <<EOF
  > let q = [%sql "SELECT id FROM users WHERE emial = 'a'"]
  > EOF
  $ build Misspelt
  File "misspelt.ml", line 1, characters 42-47:
  1 | let q = [%sql "SELECT id FROM users WHERE emial = 'a'"]
                                                ^^^^^
  Error: unknown column emial (did you mean email?)

A qualified misspelt column inside a delimited string highlights the full
qualified reference at the exact column.

  $ cat > qualified.ml <<EOF
  > let q = [%sql {sql|SELECT u.emial FROM users u|sql}]
  > EOF
  $ build Qualified
  File "qualified.ml", line 1, characters 26-33:
  1 | let q = [%sql {sql|SELECT u.emial FROM users u|sql}]
                                ^^^^^^^
  Error: unknown column u.emial (did you mean email?)

An unbound nullable :name? highlights the full parameter including ?.

  $ cat > nullable_param.ml <<EOF
  > let q = [%sql "SELECT id FROM users WHERE age = :age?"]
  > EOF
  $ build Nullable_param
  File "nullable_param.ml", line 1, characters 48-53:
  1 | let q = [%sql "SELECT id FROM users WHERE age = :age?"]
                                                      ^^^^^
  Error: Unbound value age

An INSERT that leaves out a column the database can't fill in.

  $ cat > left_out.ml <<EOF
  > let q name = [%sql "INSERT INTO users (name) VALUES (:name)"]
  > EOF
  $ build Left_out
  File "left_out.ml", line 1, characters 32-37:
  1 | let q name = [%sql "INSERT INTO users (name) VALUES (:name)"]
                                      ^^^^^
  Error: INSERT leaves out email: it is NOT NULL and has no default

A function qb has no type for, and the cast that gives it one.

  $ cat > no_type.ml <<EOF
  > let q = [%sql "SELECT soundex(name) FROM users"]
  > EOF
  $ build No_type
  File "no_type.ml", line 1, characters 22-29:
  1 | let q = [%sql "SELECT soundex(name) FROM users"]
                            ^^^^^^^
  Error: qb doesn't know what soundex(...) returns: say so with a cast, e.g.
         soundex(...)::text

Two methods of an object with one name.

  $ cat > same_name.ml <<EOF
  > let q = [%sql.obj "SELECT a.id, b.id FROM users a, users b"]
  > EOF
  $ build Same_name
  File "same_name.ml", line 1, characters 8-60:
  1 | let q = [%sql.obj "SELECT a.id, b.id FROM users a, users b"]
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: qb: two columns are named id: rename one with AS
