(* Postgres's parser against qb: checking SQL, replaying migrations into a
   schema, and the modules generated from it (Schema_gen, built from
   schema/*.sql by the dune rule). *)

open Qb_pg_parse

let ok_or_fail sql = function
  | Ok x -> x
  | Error e -> Alcotest.fail (Pg.error_to_string ~sql e)

(* ---------- Pg ---------- *)

let parses () =
  Alcotest.(check bool) "select" true (Result.is_ok (Pg.check "SELECT 1"));
  Alcotest.(check bool)
    "params" true
    (Result.is_ok (Pg.check "SELECT * FROM t WHERE a = $1 AND b = $2"));
  match Pg.statements "SELECT 1; DELETE FROM t" with
  | Ok [ `Assoc [ ("SelectStmt", _) ]; `Assoc [ ("DeleteStmt", _) ] ] -> ()
  | _ -> Alcotest.fail "expected a SelectStmt and a DeleteStmt"

let syntax_error () =
  let sql = "SELECT a\nFROM t WHERE" in
  match Pg.check sql with
  | Ok () -> Alcotest.fail "expected an error"
  | Error e ->
      Alcotest.(check (option int))
        "position"
        (Some (String.length sql))
        e.position;
      Alcotest.(check string)
        "message with caret"
        "line 2: syntax error at end of input\n  FROM t WHERE\n              ^"
        (Pg.error_to_string ~sql e)

let caqti_placeholders () =
  let pg, n, _ =
    Pg.of_caqti {|SELECT '?', "a?" FROM t WHERE x = ? -- y = ?
AND z IN (?, ?)|}
  in
  Alcotest.(check int) "count" 3 n;
  Alcotest.(check string)
    "numbered"
    {|SELECT '?', "a?" FROM t WHERE x = $1 -- y = ?
AND z IN ($2, $3)|} pg;
  Alcotest.(check bool)
    "? is not Postgres" true
    (Result.is_error (Pg.check "SELECT ?"));
  Alcotest.(check bool)
    "unless read as caqti" true
    (Result.is_ok (Pg.check ~caqti:true "SELECT ?"))

let caqti_error_position () =
  (* Each ? grows to $n, but the position still points into the original. *)
  let sql = "SELECT ? , ? , ? FROM FROM" in
  match Pg.check ~caqti:true sql with
  | Error { position = Some p; _ } -> Alcotest.(check int) "position" 22 p
  | _ -> Alcotest.fail "expected an error with a position"

(* ---------- qb's SQL ---------- *)

module Users = struct
  type t = { id : int; name : string; age : int; email : string option }

  let id =
    Qb.column "id" Qb.int (fun u -> u.id) ~primary_key:true ~auto_increment:true

  let name = Qb.column "name" Qb.text (fun u -> u.name)
  let age = Qb.column "age" Qb.int (fun u -> u.age)
  let email = Qb.column "email" (Qb.nullable Qb.text) (fun u -> u.email)

  let table =
    Qb.(
      table "users"
        (let+ id = col id
         and+ name = col name
         and+ age = col age
         and+ email = col email in
         { id; name; age; email }))
end

(* Keywords as names, which only work quoted. *)
module Order = struct
  type t = { user : int; group : string option }

  let user = Qb.column "user" Qb.int (fun o -> o.user) ~primary_key:true
  let group = Qb.column "group" (Qb.nullable Qb.text) (fun o -> o.group)

  let table =
    Qb.(
      table "order"
        (let+ user = col user and+ group = col group in
         { user; group }))
end

module Tickets = struct
  let id = Qb.column "id" Qb.int Fun.id ~primary_key:true ~auto_increment:true
  let table = Qb.table "tickets" (Qb.col id)
end

let ann = { Users.id = 1; name = "Ann"; age = 30; email = None }

(* Every statement shape qb renders. *)
let qb_statements dialect =
  let open Qb in
  let users = from Users.table in
  let filtered =
    users
    |> where
         Ops.(
           (Users.age > 18 && Users.email <> None)
           || not (Users.name = "Bob" || is_in Users.id [ 1; 2; 3 ]))
    |> where
         Ops.(like Users.name "A%" && is_in Users.id [] && Users.email = None)
  in
  [
    ("select", to_sql ~dialect users);
    ("select where", to_sql ~dialect filtered);
    ( "select columns, order, limit",
      to_sql ~dialect
        (filtered
        |> select [ Users.id; Users.name ]
        |> order_by [ asc Users.name; desc Users.age ]
        |> limit 10 |> offset 20) );
    ("select all_rows", to_sql ~dialect (users |> where all_rows));
    ("count", count_sql ~dialect filtered);
    ("insert", insert_sql ~dialect Users.table ann);
    ( "insert returning",
      insert_returning_sql ~dialect Users.table
        Ops.[ Users.name := "Ann"; Users.age := 30 ]
        [ Users.id ] );
    ( "insert defaults",
      insert_returning_sql ~dialect Tickets.table [] (col Tickets.id) );
    ( "update",
      update_sql ~dialect
        Ops.[ Users.name := "Bo"; Users.email := Some "b@x" ]
        filtered );
    ("delete", delete_sql ~dialect filtered);
    ("delete all", delete_sql ~dialect (users |> where all_rows));
    ( "insert values",
      insert_values_sql ~dialect
        (values Users.table Ops.[ Users.name := "Ann"; Users.age := 30 ]) );
    ("create users", create_table_sql ~dialect Users.table);
    ("create order", create_table_sql ~dialect Order.table);
    ( "order table",
      to_sql ~dialect (from Order.table |> where Ops.(Order.group = Some "g"))
    );
  ]

let qb_sql_parses () =
  List.iter
    (fun (what, sql) ->
      match Pg.check ~caqti:true sql with
      | Ok () -> ()
      | Error e -> Alcotest.failf "%s: %s" what (Pg.error_to_string ~sql e))
    (qb_statements Qb.Postgres)

let mysql_sql_does_not () =
  (* A sanity check that the parser is really Postgres's: MySQL's backtick
     quoting isn't valid there. *)
  let sql = Qb.to_sql ~dialect:Qb.Mysql (Qb.from Users.table) in
  Alcotest.(check bool)
    "backticks rejected" true
    (Result.is_error (Pg.check ~caqti:true sql))

(* ---------- Schema ---------- *)

let read file =
  Pg.strip_psql_meta (In_channel.with_open_bin file In_channel.input_all)

let migrations =
  List.map read [ "schema/001_init.sql"; "schema/002_changes.sql" ]

let replay sqls =
  List.fold_left
    (fun st sql -> ok_or_fail sql (Schema.apply_sql ~init:st sql))
    Schema.empty sqls

let table ?schema st name =
  match Schema.find ?schema st name with
  | Some t -> t
  | None -> Alcotest.failf "no table %s" name

let col (t : Schema.table) name =
  match List.find_opt (fun (c : Schema.column) -> c.name = name) t.columns with
  | Some c -> c
  | None -> Alcotest.failf "no column %s.%s" t.name name

let col_names (t : Schema.table) =
  List.map (fun (c : Schema.column) -> c.name) t.columns

let replays_migrations () =
  let st = replay migrations in
  Alcotest.(check (list string))
    "tables"
    [ "accounts"; "articles"; "audit.accounts" ]
    (List.map
       (fun (t : Schema.table) ->
         Option.fold t.schema ~none:t.name ~some:(fun s -> s ^ "." ^ t.name))
       st.tables);
  let events = table ~schema:"audit" st "accounts" in
  Alcotest.(check bool)
    "nextval default is generated" true (col events "event_id").generated;
  Alcotest.(check (list string))
    "pk added by ALTER" [ "event_id" ] events.primary_key;
  let accounts = table st "accounts" in
  Alcotest.(check (list string))
    "accounts columns"
    [ "id"; "email"; "nickname"; "active"; "created_at"; "login_count" ]
    (col_names accounts);
  Alcotest.(check (list string)) "pk" [ "id" ] accounts.primary_key;
  let id = col accounts "id" in
  Alcotest.(check string) "bigserial is int8" "int8" id.ty.name;
  Alcotest.(check bool) "bigserial is generated" true id.generated;
  let nick = col accounts "nickname" in
  Alcotest.(check string) "varchar(80)" "VARCHAR(80)" (Schema.type_sql nick.ty);
  Alcotest.(check bool) "nullable" false nick.not_null;
  Alcotest.(check bool) "default" true (col accounts "login_count").has_default;
  let articles = table st "articles" in
  Alcotest.(check (list string))
    "articles columns"
    [
      "id";
      "account_id";
      "title";
      "body";
      "meta";
      "published_on";
      "type";
      "rating";
      "values";
    ]
    (col_names articles);
  Alcotest.(check bool) "SET NOT NULL" true (col articles "body").not_null;
  Alcotest.(check (list string)) "no warnings" [] st.warnings

let composite_and_warnings () =
  let sql =
    {|CREATE TABLE m (a INT, b INT, c TEXT[], PRIMARY KEY (a, b));
      ALTER TABLE m DROP CONSTRAINT m_pkey;
      ALTER TABLE nope ADD COLUMN x INT;
      CREATE TABLE IF NOT EXISTS m (z INT);|}
  in
  let st = ok_or_fail sql (Schema.of_sql sql) in
  let m = table st "m" in
  Alcotest.(check (list string)) "composite pk" [ "a"; "b" ] m.primary_key;
  Alcotest.(check bool) "pk columns are not null" true (col m "a").not_null;
  Alcotest.(check int) "array" 1 (col m "c").ty.array_dims;
  Alcotest.(check (list string))
    "IF NOT EXISTS kept the first" [ "a"; "b"; "c" ] (col_names m);
  Alcotest.(check (list string))
    "warnings"
    [ "m: DROP CONSTRAINT m_pkey not tracked"; "unknown table nope" ]
    st.warnings

(* ---------- Codegen ---------- *)

(* What qb can express: defaults aren't part of a qb column. *)
let shape (t : Schema.table) =
  ( t.name,
    t.primary_key,
    List.map
      (fun (c : Schema.column) ->
        (c.name, Schema.type_sql c.ty, c.not_null, c.generated))
      t.columns )

let shape_t =
  Alcotest.(
    triple string (list string)
      (list (pair string (pair string (pair bool bool)))))

let flat (n, pk, cs) =
  (n, pk, List.map (fun (a, b, c, d) -> (a, (b, (c, d)))) cs)

(* The generated module renders a CREATE TABLE that reads back as the table
   it was generated from. *)
let round_trip () =
  let st = replay migrations in
  List.iter
    (fun (schema, name, create) ->
      let back = ok_or_fail create (Schema.of_sql create) in
      Alcotest.check shape_t name
        (flat (shape (table ?schema st name)))
        (flat (shape (table ?schema back name))))
    [
      (None, "accounts", Qb.create_table_sql Schema_gen.Accounts.table);
      (None, "articles", Qb.create_table_sql Schema_gen.Articles.table);
      (* Generated inside module Audit, with Qb.table ~schema:"audit". *)
      ( Some "audit",
        "accounts",
        Qb.create_table_sql Schema_gen.Audit.Accounts.table );
    ]

let strips_psql_meta () =
  Alcotest.(check string)
    "same length" "SELECT 1;\n           \nSELECT 2;"
    (Pg.strip_psql_meta "SELECT 1;\n\\restrict x\nSELECT 2;")

let qb_table_round_trip () =
  (* And the other way: a hand-written qb table, through its DDL, generates
     the same column definitions. *)
  let ddl = Qb.create_table_sql Users.table in
  let src = Codegen.ocaml (ok_or_fail ddl (Schema.of_sql ddl)) in
  List.iter
    (fun line ->
      let contains s sub =
        let n = String.length sub in
        let rec go i =
          i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
        in
        go 0
      in
      if not (contains src line) then
        Alcotest.failf "missing %S in:\n%s" line src)
    [
      {|Qb.column "id" Qb.int (fun (r : t) -> r.id) ~primary_key:true ~auto_increment:true|};
      {|Qb.column "name" Qb.text (fun (r : t) -> r.name)|};
      {|Qb.column "email" (Qb.nullable Qb.text) (fun (r : t) -> r.email)|};
      "email : string option;";
    ]

let idents () =
  Alcotest.(check (list string))
    "idents"
    [ "type_"; "user_id"; "_2fa"; "camelCase"; "a_b" ]
    (List.map Codegen.ident [ "type"; "user_id"; "2fa"; "CamelCase"; "a-b" ]);
  Alcotest.(check (list string))
    "module names"
    [ "Drizzle_migrations"; "T_2fa"; "Users"; "Type_"; "T" ]
    (List.map Codegen.module_name
       [ "__drizzle_migrations"; "2fa"; "users"; "type"; "_" ])

(* The generated modules work against a real database. *)
(* The generated modules work against a real database: [values] leaves out
   what the database fills in, [by_pk] finds, updates and deletes a row. *)
let generated_on_sqlite () =
  let module Db =
    (val Caqti_blocking.connect (Uri.of_string "sqlite3::memory:")
         |> Result.fold ~ok:Fun.id ~error:(fun e ->
             Alcotest.fail (Caqti_error.show e)))
  in
  let db = (module Db : Caqti_blocking.CONNECTION) in
  let get = function
    | Ok x -> x
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  let open Schema_gen in
  get (Qb.Blocking.create_table db Accounts.table);
  let now = Ptime.of_float_s 1_700_000_000. |> Option.get in
  (* SQLite gets no DEFAULTs from create_table, so login_count and active are
     given here; id is still left to the database. *)
  let ann =
    get
      (Qb.Blocking.insert_values db
         (Accounts.values ~email:"ann@example.com" ~active:true ~created_at:now
            ~login_count:3 ()))
  in
  let bo =
    get
      (Qb.Blocking.insert_values db
         (Accounts.values ~email:"bo@example.com" ~nickname:"bo" ~active:false
            ~created_at:now ~login_count:0 ()))
  in
  Alcotest.(check (list int)) "ids filled in" [ 1; 2 ] [ ann.id; bo.id ];
  Alcotest.(check (option string)) "omitted nullable is NULL" None ann.nickname;
  Alcotest.(check (option string)) "given nullable" (Some "bo") bo.nickname;
  Alcotest.(check bool) "created_at" true (Ptime.equal now ann.created_at);
  let find id = get (Qb.Blocking.first db (Accounts.by_pk id)) in
  Alcotest.(check (option string))
    "by_pk finds" (Some "bo@example.com")
    (Option.map (fun (a : Accounts.t) -> a.email) (find bo.id));
  get
    (Qb.Blocking.update db
       Qb.Ops.[ Accounts.login_count := 4 ]
       (Accounts.by_pk ann.id));
  Alcotest.(check (option int))
    "by_pk updates" (Some 4)
    (Option.map (fun (a : Accounts.t) -> a.login_count) (find ann.id));
  get (Qb.Blocking.delete db (Accounts.by_pk ann.id));
  Alcotest.(check bool) "by_pk deletes" true (find ann.id = None);
  Alcotest.(check int)
    "the other row is untouched" 1
    (get (Qb.Blocking.count db (Qb.from Accounts.table)))

(* ---------- queries ---------- *)

let query_file = In_channel.with_open_bin "queries.sql" In_channel.input_all

(* Parameters take their types from the columns they are compared with;
   result columns are nullable when the column is, or its table is on the
   outer side of a join. *)
let query_types () =
  let st = replay migrations in
  let qs = ok_or_fail query_file (Query.of_sql st query_file) in
  let q name = List.find (fun (q : Query.t) -> q.name = name) qs in
  let params (q : Query.t) =
    List.map
      (fun (p : Query.param) ->
        Printf.sprintf "%s %s%s" p.name (Schema.type_sql p.ty)
          (if p.nullable then "?" else ""))
      q.params
  in
  let row (q : Query.t) =
    List.map
      (function
        | Query.Column c ->
            Printf.sprintf "%s %s%s" c.name (Schema.type_sql c.ty)
              (if c.nullable then "?" else "")
        | Whole { table; nullable } ->
            Printf.sprintf "%s.*%s" table.name (if nullable then "?" else ""))
      q.row
  in
  let strings = Alcotest.(check (list string)) in
  strings "names"
    [
      "account_by_email";
      "articles_with_authors";
      "accounts_and_articles";
      "titles";
      "article_count";
      "counts";
      "by_nickname";
    ]
    (List.map (fun (q : Query.t) -> q.name) qs);
  let titles = q "titles" in
  strings "params, numbered as they first appear"
    [
      "pattern TEXT";
      "active BOOLEAN";
      "low INTEGER";
      "high INTEGER";
      "limit BIGINT";
    ]
    (params titles);
  strings "columns: the left join makes title nullable"
    [
      "email TEXT";
      "nickname VARCHAR(80)?";
      "title TEXT?";
      "stars DOUBLE PRECISION?";
    ]
    (row titles);
  Alcotest.(check bool)
    "numbered, comments and the semicolon gone" true
    (String.ends_with ~suffix:"LIMIT $5" titles.sql
    && String.starts_with ~prefix:"SELECT ac.email" titles.sql);
  strings "whole tables"
    [ "accounts.*"; "articles.*?" ]
    (row (q "accounts_and_articles"));
  strings "count, coalesce"
    [ "email TEXT"; "n BIGINT"; "best DOUBLE PRECISION" ]
    (row (q "counts"));
  strings ":name? may be NULL; used twice, one parameter" [ "nickname TEXT?" ]
    (params (q "by_nickname"));
  (* Expressions typed without a cast. *)
  let one sql = ok_or_fail sql (Query.one st sql) in
  let cols sql = row (one ("SELECT " ^ sql ^ " FROM accounts")) in
  strings "text functions: NULL when an argument can be"
    [ "lower TEXT"; "upper TEXT?"; "n INTEGER"; "concat TEXT" ]
    (cols
       "lower(email), upper(nickname), length(email) AS n, concat(nickname, \
        'x')");
  strings "the clock"
    [ "now TIMESTAMP WITH TIME ZONE"; "column2 DATE" ]
    (cols "now(), CURRENT_DATE");
  strings "aggregates"
    [ "sum BIGINT?"; "bool_or BOOLEAN?"; "string_agg TEXT?" ]
    (cols "sum(login_count), bool_or(active), string_agg(email, ',')");
  strings "conditions"
    [
      "a BOOLEAN";
      "b BOOLEAN?";
      "c BOOLEAN";
      "d BOOLEAN";
      "e BOOLEAN?";
      "f BOOLEAN";
    ]
    (cols
       "login_count > 3 AS a, nickname = 'x' AS b, nickname IS NULL AS c, \
        nickname IS DISTINCT FROM 'x' AS d, active AND nickname LIKE 'a%' AS \
        e, id IN (1, 2) AS f");
  strings "CASE, NULLIF, GREATEST"
    [ "a TEXT"; "b TEXT?"; "c BIGINT"; "d TEXT?"; "e BIGINT" ]
    (cols
       "CASE WHEN active THEN 'on' ELSE 'off' END AS a, CASE WHEN active THEN \
        email END AS b, CASE WHEN active THEN login_count ELSE id END AS c, \
        NULLIF(email, '') AS d, GREATEST(id, login_count) AS e");
  strings "a parameter in a CASE branch takes the others' type"
    [ "fallback TEXT" ]
    (params
       (one
          "SELECT CASE WHEN active THEN email ELSE :fallback END FROM accounts"));
  let typed sql = params (one sql) in
  strings "a parameter inside a text function is text" [ "email TEXT" ]
    (typed "SELECT id FROM accounts WHERE lower(email) = lower(:email)");
  strings "and one put through a function into a column"
    [ "nick TEXT"; "id BIGINT" ]
    (typed "UPDATE accounts SET nickname = upper(:nick) WHERE id = :id");
  strings "a parameter concatenated with a constant is text" [ "q TEXT" ]
    (typed "SELECT id FROM accounts WHERE email LIKE '%' || :q || '%'");
  strings "a parameter in arithmetic takes the type of what it is compared to"
    [ "n INTEGER" ]
    (typed "SELECT id FROM accounts WHERE login_count > :n + 1");
  strings "through COALESCE too" [ "nick VARCHAR(80)?" ]
    (typed "SELECT id FROM accounts WHERE nickname = COALESCE(:nick?, nickname)");
  strings "contains: both sides have one type" [ "filter JSONB" ]
    (typed "SELECT id FROM articles WHERE meta @> :filter");
  strings "numbers: a sign, a whole number with a float, date_part"
    [ "a INTEGER"; "b DOUBLE PRECISION?"; "c DOUBLE PRECISION" ]
    (row
       (one
          "SELECT -login_count AS a, rating / 2 AS b, date_part('year', \
           created_at) AS c FROM accounts, articles"));
  strings "JSON: ->> is text, -> is JSON, and either may be NULL"
    [ "a TEXT?"; "b JSONB?" ]
    (row (one "SELECT meta->>'k' AS a, meta->'k' AS b FROM articles"));
  strings "a $$-quoted body and an E'' string hold no parameters" [ "x TEXT" ]
    (typed
       "SELECT id FROM accounts WHERE email = $$:a$$ OR email = E'it\\'s :b' \
        OR email = :x");
  Alcotest.(check (list (pair string int)))
    "where each parameter is first written"
    [ ("b", 37); ("a", 49) ]
    (List.map
       (fun (p : Query.param) -> (p.name, p.at))
       (one
          "SELECT 1 FROM accounts WHERE email = :b AND id = :a AND email <> :b")
         .params)

(* Postgres rejects a bare [$1 IS NULL]; the type worked out is written in. *)
let null_test_cast () =
  let st = replay migrations in
  let sql q = (ok_or_fail q (Query.one st q)).sql in
  Alcotest.(check string)
    "cast added"
    "SELECT email FROM accounts WHERE CAST($1 AS VARCHAR(80)) IS NULL OR \
     nickname = $1 OR CAST($1 AS VARCHAR(80)) IS NOT NULL"
    (sql
       "SELECT email FROM accounts WHERE :n? IS NULL OR nickname = :n OR :n IS \
        NOT NULL");
  Alcotest.(check string)
    "a cast already there is kept"
    "SELECT email FROM accounts WHERE $1::text IS NULL OR nickname = $1"
    (sql "SELECT email FROM accounts WHERE :n?::text IS NULL OR nickname = :n")

let query_errors () =
  let st = replay migrations in
  let fails name sql expected =
    let sql = "-- name: q\n" ^ sql in
    match Query.of_sql st sql with
    | Ok _ -> Alcotest.failf "%s: expected an error" name
    | Error e ->
        Alcotest.(check string) name expected (Pg.error_to_string ~sql e)
  in
  fails "unknown column" "SELECT email\nFROM accounts WHERE nope = :x"
    "line 3: unknown column nope\n\
    \  FROM accounts WHERE nope = :x\n\
    \                      ^";
  fails "position after a parameter"
    "SELECT email FROM accounts WHERE email = :email AND nope = 1"
    "line 2: unknown column nope\n\
    \  SELECT email FROM accounts WHERE email = :email AND nope = 1\n\
    \                                                      ^";
  let message name sql expected =
    match Query.of_sql st ("-- name: q\n" ^ sql) with
    | Ok _ -> Alcotest.failf "%s: expected an error" name
    | Error e -> Alcotest.(check string) name expected e.message
  in
  message "unknown table" "SELECT 1 FROM missing" "unknown table missing";
  message "ambiguous"
    "SELECT id FROM accounts a JOIN articles ar ON ar.account_id = a.id"
    "column id is in several tables: write table.id";
  message "parameter with no type"
    "SELECT email FROM accounts WHERE my_fn(email) = :e"
    "can't work out the type of :e: write a cast, e.g. :e::text";
  message "function qb doesn't know" "SELECT my_fn(email) FROM accounts"
    "qb doesn't know what my_fn(...) returns: say so with a cast, e.g. \
     my_fn(...)::text";
  message "expression with no type" "SELECT email::int + 1.5 FROM accounts"
    "can't work out the type of this expression: write a cast, e.g. (...)::text";
  message "named column with no type"
    "SELECT email::int + 1.5 AS n FROM accounts"
    "can't work out the type of column n: write a cast, e.g. (...)::text";
  message "avg of an integer" "SELECT avg(login_count) FROM accounts"
    "avg of a whole number is NUMERIC, which qb reads as text: for a float, \
     write avg(...)::float8";
  message "sum of a bigint" "SELECT sum(id) FROM accounts"
    "sum of a BIGINT or NUMERIC is NUMERIC, which qb reads as text: write \
     sum(...)::bigint, or ::numeric to keep it";
  message "ANY of a parameter" "SELECT id FROM accounts WHERE id = ANY(:ids)"
    "ANY(:ids) takes an array, which a parameter can't be: write IN :ids for a \
     list of values";
  message "extract" "SELECT extract(year FROM created_at) FROM accounts"
    "extract is NUMERIC, which qb reads as text: write extract(...)::int, or \
     ::float8 for a fraction";
  message "did you mean a column" "SELECT emial FROM accounts"
    "unknown column emial (did you mean email?)";
  message "did you mean a qualified column" "SELECT a.nikname FROM accounts a"
    "unknown column a.nikname (did you mean nickname?)";
  message "did you mean a table" "SELECT 1 FROM acounts"
    "unknown table acounts (did you mean accounts?)";
  message "did you mean an alias" "SELECT ac.id FROM accounts acc"
    "unknown table ac (did you mean acc?)";
  message "insert leaves out a required column"
    "INSERT INTO articles (id, title) VALUES (:id, :title)"
    "INSERT leaves out account_id, body: they are NOT NULL and have no default";
  message "insert default values" "INSERT INTO accounts DEFAULT VALUES"
    "INSERT leaves out email: it is NOT NULL and has no default";
  message "insert with too few values" "INSERT INTO audit.accounts VALUES (1)"
    "INSERT leaves out account_id, at: they are NOT NULL and have no default";
  message "one parameter, two types"
    "SELECT email FROM accounts WHERE id = :x AND email = :x"
    "parameter :x is used as both BIGINT and TEXT";
  message "not a query" "CREATE TABLE x (a int)"
    "only SELECT, INSERT, UPDATE and DELETE are read";
  message "list and single value"
    "SELECT email FROM accounts WHERE id IN :x OR id = :x"
    ":x is a list in one place (IN :x) and a single value in another";
  message "a list can't be NULL" "SELECT email FROM accounts WHERE id IN :x?"
    "a list can't be NULL: write IN :x";
  message "insert into an unknown column"
    "INSERT INTO accounts (emial) VALUES (:e)"
    "unknown column accounts.emial (did you mean email?)";
  message "set an unknown column" "UPDATE accounts SET emial = :e"
    "unknown column accounts.emial (did you mean email?)";
  message "insert ... select"
    "INSERT INTO accounts (email) SELECT email FROM accounts"
    "INSERT ... SELECT isn't supported: write VALUES";
  message "too many values" "INSERT INTO accounts (email) VALUES (:a, :b)"
    "INSERT has more values than columns";
  message "a constant doesn't type a parameter in arithmetic"
    "SELECT email FROM accounts WHERE id = 1 AND :n + 1 > 2"
    "can't work out the type of :n: write a cast, e.g. :n::text";
  message "subquery"
    "SELECT email FROM accounts WHERE id IN (SELECT account_id FROM articles)"
    "subqueries aren't supported";
  message "self-join without aliases"
    "SELECT email FROM accounts JOIN accounts ON true"
    "table accounts is named twice: give one an alias";
  message "function in FROM" "SELECT x FROM generate_series(1, 3) x"
    "RangeFunction in FROM isn't supported";
  message "IS NULL alone types nothing"
    "SELECT email FROM accounts WHERE :p IS NULL"
    "can't work out the type of :p: write a cast, e.g. :p::text";
  message "two statements" "SELECT 1; SELECT 2"
    "several statements: a query is one statement";
  message "none" "-- nothing" "there is no statement here";
  message "syntax" "SELECT FROM WHERE" "syntax error at or near \"WHERE\"";
  message "$1" "SELECT email FROM accounts WHERE id = $1"
    "parameters are written :name here, not $1";
  (match Query.of_sql st "SELECT 1" with
  | Error e ->
      Alcotest.(check string)
        "no name" "expected -- name: query_name before this statement" e.message
  | Ok _ -> Alcotest.fail "expected an error");
  (* Without ~tables, t.* is spelled out, and two ids can't share a record. *)
  let sql =
    "-- name: q\n\
     SELECT a.*, ar.* FROM accounts a JOIN articles ar ON ar.account_id = a.id"
  in
  match Codegen.queries st (ok_or_fail sql (Query.of_sql st sql)) with
  | Error m ->
      Alcotest.(check string)
        "duplicate field"
        "query q: two columns are named id: rename one with AS" m
  | Ok _ -> Alcotest.fail "expected an error"

(* What [Codegen.queries] writes for its options: each statement named after its
   query, [BIGINT] as [int64] with [~bigint:`Int64], and one record for queries
   whose columns are the same. *)
let codegen_options () =
  let st = replay migrations in
  let sql =
    "-- name: by_email\n\
     SELECT id, email FROM accounts WHERE email = :email;\n\
     -- name: by_id\n\
     SELECT id, email FROM accounts WHERE id = :id;\n\
     -- name: emails\n\
     SELECT email, id FROM accounts;"
  in
  let qs = ok_or_fail sql (Query.of_sql st sql) in
  let src ?bigint () =
    match Codegen.queries ?bigint st qs with
    | Ok src -> src
    | Error m -> Alcotest.fail m
  in
  let has name sub text =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length text && (String.sub text i n = sub || go (i + 1))
    in
    Alcotest.(check bool) name true (go 0)
  in
  let lacks name sub text =
    let n = String.length sub in
    let rec go i =
      i + n <= String.length text && (String.sub text i n = sub || go (i + 1))
    in
    Alcotest.(check bool) name false (go 0)
  in
  let int64 = src ~bigint:`Int64 () in
  has "named" {|Qb.Stmt.make ~name:"by_email"|} int64;
  has "int64 column" "id : int64;" int64;
  has "int64 reader" "Qb.Stmt.ret Qb.int64" int64;
  has "shared record" "let by_id ~id : by_email_row Qb.stmt" int64;
  has "columns in another order: a record of its own" "type emails_row = {" int64;
  lacks "no second record for the same columns" "type by_id_row" int64;
  let int = src () in
  has "int by default" "id : int;" int;
  lacks "no int64 by default" "int64" int

(* Two accounts and three articles, for the generated queries. *)
let seed_queries ~insert_account ~insert_article =
  let open Schema_gen in
  let ann : Accounts.t = insert_account "ann@example.com" None true 3 in
  let bo : Accounts.t = insert_account "bo@example.com" (Some "bo") false 0 in
  List.iter
    (fun (id, title, rating) ->
      insert_article
        (Articles.values
           ~id:("00000000-0000-0000-0000-00000000000" ^ id)
           ~account_id:ann.id ~title ~body:"..." ?rating ()))
    [ ("1", "Apples", Some 4.5); ("2", "Bees", Some 2.0); ("3", "Cats", None) ];
  (ann, bo)

(* The queries that are also SQLite's SQL (no casts). *)
let get = function Ok x -> x | Error e -> Alcotest.fail (Caqti_error.show e)

let run_queries db ((ann : Schema_gen.Accounts.t), (bo : Schema_gen.Accounts.t))
    =
  let open Schema_gen in
  let all s = get (Qb.Blocking.fetch_all db s) in
  Alcotest.(check (option int))
    "a whole row" (Some ann.id)
    (get
       (Qb.Blocking.fetch_opt db
          (Queries_gen.account_by_email ~email:"ann@example.com"))
    |> Option.map (fun (a : Accounts.t) -> a.id));
  Alcotest.(check (list (pair string string)))
    "join: pairs of whole rows"
    [ ("Apples", "ann@example.com"); ("Bees", "ann@example.com") ]
    (all (Queries_gen.articles_with_authors ~min_rating:1.0)
    |> List.map (fun ((ar : Articles.t), (ac : Accounts.t)) ->
        (ar.title, ac.email)));
  Alcotest.(check (list (pair string (option string))))
    "left join: None where nothing matched"
    [
      ("ann@example.com", Some "Apples");
      ("ann@example.com", Some "Bees");
      ("ann@example.com", Some "Cats");
      ("bo@example.com", None);
    ]
    (all Queries_gen.accounts_and_articles
    |> List.map (fun ((ac : Accounts.t), (ar : Articles.t option)) ->
        (ac.email, Option.map (fun (a : Articles.t) -> a.title) ar)));
  let titles ~active ~pattern =
    all (Queries_gen.titles ~pattern ~active ~low:0 ~high:10 ~limit:2)
    |> List.map (fun (r : Queries_gen.titles_row) ->
        (r.email, r.title, r.stars))
  in
  let rows =
    Alcotest.(list (triple string (option string) (option (float 0.))))
  in
  Alcotest.check rows "a record per row, limited"
    [
      ("ann@example.com", Some "Apples", Some 4.5);
      ("ann@example.com", Some "Bees", Some 2.0);
    ]
    (titles ~active:true ~pattern:"%");
  Alcotest.check rows "nullable columns from the outer side"
    [ ("bo@example.com", None, None) ]
    (titles ~active:false ~pattern:"%");
  Alcotest.(check int)
    "one column is the bare value" 3
    (get
       (Qb.Blocking.fetch_one db (Queries_gen.article_count ~account_id:ann.id)));
  ignore bo

let queries_on_sqlite () =
  let db =
    match Caqti_blocking.connect (Uri.of_string "sqlite3::memory:") with
    | Ok db -> db
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  let get = function
    | Ok x -> x
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  let open Schema_gen in
  get (Qb.Blocking.create_table db Accounts.table);
  get (Qb.Blocking.create_table db Articles.table);
  let now = Ptime.of_float_s 1_700_000_000. |> Option.get in
  let seeded =
    seed_queries
      ~insert_account:(fun email nickname active login_count ->
        get
          (Qb.Blocking.insert_values db
             (Accounts.values ~email ?nickname ~active ~created_at:now
                ~login_count ())))
      ~insert_article:(fun v -> ignore (get (Qb.Blocking.insert_values db v)))
  in
  run_queries db seeded

(* On Postgres the migrations run for real, so [values] can leave out every
   column with a DEFAULT and the database fills them in. Statements are split
   with the parser's own locations. *)
let split sql =
  match Pg.parse sql with
  | Error e -> Alcotest.fail (Pg.error_to_string ~sql e)
  | Ok j ->
      let int k s =
        match Yojson.Safe.Util.member k s with `Int i -> i | _ -> 0
      in
      Yojson.Safe.Util.(member "stmts" j |> to_list)
      |> List.map (fun s ->
          let at = int "stmt_location" s in
          let len = int "stmt_len" s in
          let len = if len = 0 then String.length sql - at else len in
          String.trim (String.sub sql at len))

let generated_on_postgres url () =
  let db =
    match Caqti_blocking.connect (Uri.of_string url) with
    | Ok db -> db
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  let (module Db : Caqti_blocking.CONNECTION) = db in
  Fun.protect ~finally:Db.disconnect @@ fun () ->
  let get = function
    | Ok x -> x
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  let exec sql =
    get
      (Db.exec
         (Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit
            Caqti_mult.zero (fun _ -> Caqti_query.L sql))
         ())
  in
  List.iter exec
    [
      "DROP TABLE IF EXISTS accounts, posts, articles, scratch CASCADE";
      "DROP SCHEMA IF EXISTS audit CASCADE";
    ];
  List.iter (fun sql -> List.iter exec (split sql)) migrations;
  let open Schema_gen in
  (* Only the required column: id, active, created_at and login_count all
     come from the database. *)
  let a =
    get
      (Qb.Blocking.insert_values db
         (Accounts.values ~email:"cy@example.com" ()))
  in
  Alcotest.(check int) "bigserial id after the seed row" 2 a.id;
  Alcotest.(check bool) "DEFAULT true" true a.active;
  Alcotest.(check int) "DEFAULT 0" 0 a.login_count;
  Alcotest.(check (option string)) "NULL" None a.nickname;
  let e =
    get
      (Qb.Blocking.insert_values db
         (Audit.Accounts.values ~account_id:a.id ~at:a.created_at ()))
  in
  Alcotest.(check int) "nextval default, in schema audit" 1 e.event_id;
  get
    (Qb.Blocking.update db
       Qb.Ops.[ Accounts.nickname := Some "cy" ]
       (Accounts.by_pk a.id));
  Alcotest.(check (option (option string)))
    "by_pk" (Some (Some "cy"))
    (get (Qb.Blocking.first db (Accounts.by_pk a.id))
    |> Option.map (fun (r : Accounts.t) -> r.nickname));
  (* The generated queries, including the ones with casts. *)
  get (Qb.Blocking.delete db Qb.(from Accounts.table |> where all_rows));
  let seeded =
    seed_queries
      ~insert_account:(fun email nickname active login_count ->
        get
          (Qb.Blocking.insert_values db
             (Accounts.values ~email ?nickname ~active ~login_count ())))
      ~insert_article:(fun v -> ignore (get (Qb.Blocking.insert_values db v)))
  in
  run_queries db seeded;
  Alcotest.(check (list (triple string int (float 0.))))
    "count, coalesce(max)"
    [ ("ann@example.com", 3, 4.5); ("bo@example.com", 0, 0.) ]
    (get (Qb.Blocking.fetch_all db Queries_gen.counts)
    |> List.map (fun (r : Queries_gen.counts_row) -> (r.email, r.n, r.best)));
  let by_nickname nickname =
    get (Qb.Blocking.fetch_all db (Queries_gen.by_nickname ~nickname))
  in
  Alcotest.(check (list string))
    "a parameter that may be NULL: Some" [ "bo@example.com" ]
    (by_nickname (Some "bo"));
  Alcotest.(check (list string))
    "and None" [ "ann@example.com" ] (by_nickname None)

(* ---------- the oracle: Postgres's own answer ---------- *)

(* Every type worked out here is one Postgres would also work out: each query
   is PREPAREd on a real server, which then says what it takes and returns
   (pg_prepared_statements). A type that differs would be a row that fails to
   decode, or a value sent as the wrong thing, when the query runs.

   What Postgres doesn't say is whether a column can be NULL, so that part
   of the inference is not checked here. *)

let oracle_schema =
  {|CREATE TYPE mood AS ENUM ('ok', 'bad');
CREATE TABLE users (
  id BIGSERIAL PRIMARY KEY,
  email TEXT NOT NULL,
  name TEXT,
  nick VARCHAR(80),
  age INT NOT NULL,
  small SMALLINT,
  r REAL,
  d DOUBLE PRECISION,
  score NUMERIC(10, 2),
  active BOOLEAN NOT NULL DEFAULT true,
  tags TEXT[],
  meta JSONB,
  m mood,
  "type" TEXT,
  born DATE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE posts (
  id BIGSERIAL PRIMARY KEY,
  user_id BIGINT NOT NULL REFERENCES users (id),
  title TEXT NOT NULL,
  body TEXT,
  views INT NOT NULL DEFAULT 0,
  published_at TIMESTAMPTZ
);|}

let oracle_queries =
  [
    (* columns of every type, whole rows, joins *)
    "SELECT * FROM users";
    "SELECT u.*, p.* FROM users u LEFT JOIN posts p ON p.user_id = u.id";
    "SELECT p.title, u.email FROM posts p JOIN users u ON u.id = p.user_id";
    {|SELECT "type", m, tags, meta, score, born FROM users|};
    (* parameters against columns of each type *)
    "SELECT id FROM users WHERE id = :id";
    "SELECT id FROM users WHERE email = :email AND nick = :nick";
    "SELECT id FROM users WHERE age > :age AND small < :small";
    "SELECT id FROM users WHERE r > :r AND d < :d";
    "SELECT id FROM users WHERE active = :active";
    "SELECT id FROM users WHERE created_at > :since AND born = :born";
    "SELECT id FROM users WHERE m = :mood";
    "SELECT id FROM users WHERE meta @> :filter";
    "SELECT id FROM users WHERE :tag = ANY(tags)";
    "SELECT id FROM users WHERE id IN (:a, :b)";
    "SELECT id FROM users WHERE id IN :ids";
    "SELECT id FROM users WHERE age BETWEEN :lo AND :hi";
    "SELECT id FROM users WHERE email LIKE :p OR name ILIKE :q";
    "SELECT id FROM users WHERE name IS NOT DISTINCT FROM :name?";
    "SELECT id FROM users WHERE :name? IS NULL OR name = :name";
    "SELECT title FROM posts ORDER BY views DESC LIMIT :limit OFFSET :offset";
    (* parameters reached through an expression *)
    "SELECT id FROM users WHERE lower(email) = lower(:email)";
    "SELECT id FROM users WHERE email LIKE '%' || :q || '%'";
    "SELECT id FROM users WHERE age > :n + 1";
    "SELECT id FROM users WHERE age + :n > 10";
    "SELECT id FROM users WHERE name = COALESCE(:name?, name)";
    "SELECT id FROM users WHERE length(email) > :n";
    "SELECT id FROM users WHERE created_at::date = :day";
    "SELECT user_id FROM posts GROUP BY user_id HAVING count(*) > :min";
    "SELECT COALESCE(name, :fallback) FROM users";
    "SELECT CASE WHEN active THEN email ELSE :fallback END FROM users";
    "SELECT id FROM posts WHERE published_at < now() - :age::interval";
    (* aggregates *)
    "SELECT count(*), count(name), count(DISTINCT age) FROM users";
    "SELECT min(age), max(email), min(created_at), max(d) FROM users";
    "SELECT sum(age), sum(small), sum(r), sum(d) FROM users";
    "SELECT avg(r), avg(d) FROM users";
    "SELECT bool_and(active), bool_or(active), string_agg(email, ',') FROM \
     users";
    "SELECT COALESCE(sum(views), 0) FROM posts";
    "SELECT count(*) FILTER (WHERE views > 10) FROM posts";
    "SELECT max(views) - min(views) FROM posts";
    (* text *)
    "SELECT lower(email), upper(nick), initcap(name) FROM users";
    "SELECT trim(name), ltrim(name), rtrim(name), btrim(name, 'x') FROM users";
    "SELECT replace(email, 'a', 'b'), substr(email, 1, 3), left(email, 2), \
     right(email, 2) FROM users";
    "SELECT substring(email from 1 for 3), lpad(email, 9, ' '), rpad(email, \
     9), repeat(email, 2), reverse(email), md5(email) FROM users";
    "SELECT split_part(email, '@', 1), translate(email, 'a', 'b'), \
     to_char(created_at, 'YYYY') FROM users";
    "SELECT length(email), char_length(nick), octet_length(email), \
     bit_length(email), position('@' in email), strpos(email, '@'), \
     ascii(email) FROM users";
    "SELECT concat(email, name), email || name, nick || email, email || 'x' \
     FROM users";
    (* numbers *)
    "SELECT age + 1, age - small, age * id, id / 2, age % 2, -age, +age FROM \
     users";
    "SELECT r + d, d * 2, r / 2, age + d, age * r, -d FROM users";
    "SELECT abs(age), abs(d), abs(id) FROM users";
    "SELECT GREATEST(id, age), LEAST(age, small), GREATEST(r, d) FROM users";
    "SELECT NULLIF(email, ''), NULLIF(age, 0) FROM users";
    "SELECT 1, 'a', true, 1.5 FROM users";
    "SELECT random()";
    (* time *)
    "SELECT now(), CURRENT_TIMESTAMP, CURRENT_DATE, LOCALTIMESTAMP";
    "SELECT date_trunc('day', created_at), date_part('year', created_at), \
     created_at::date FROM users";
    "SELECT transaction_timestamp(), statement_timestamp(), clock_timestamp()";
    (* conditions, CASE *)
    "SELECT age > 18, name IS NULL, active AND age > 1, NOT active, email LIKE \
     'a%', id IN (1, 2), age BETWEEN 1 AND 2, name IS DISTINCT FROM 'x', \
     active IS TRUE FROM users";
    "SELECT CASE WHEN active THEN 'on' ELSE 'off' END, CASE WHEN active THEN \
     age ELSE id END, CASE WHEN active THEN email END, CASE WHEN active THEN r \
     ELSE d END FROM users";
    "SELECT COALESCE(name, email), COALESCE(nick, ''), COALESCE(small, age), \
     COALESCE(age, 0) FROM users";
    (* JSON, casts *)
    "SELECT meta->>'k', meta->'k', meta#>>'{a,b}', meta#>'{a,b}' FROM users";
    "SELECT age::bigint, age::text, email::varchar(10), d::real, CAST(score AS \
     float8), id::int FROM users";
    "SELECT gen_random_uuid()";
    (* writes *)
    "INSERT INTO users (email, age) VALUES (:email, :age) RETURNING *";
    "INSERT INTO users (email, age, name, nick, small, r, d, active, m, born, \
     created_at) VALUES (:email, :age, :name?, :nick?, :small?, :r?, :d?, \
     :active, :m?, :born?, :at) RETURNING id";
    "INSERT INTO users (email, age, name) VALUES (lower(:email), :age + 1, \
     upper(:name))";
    "INSERT INTO users (email, age) VALUES (:email, :age) ON CONFLICT (id) DO \
     UPDATE SET email = excluded.email, age = users.age + :bump RETURNING id, \
     age";
    "UPDATE users SET name = :name?, age = age + :n WHERE id = :id RETURNING \
     age, name";
    "UPDATE posts p SET views = p.views + :n FROM users u WHERE u.id = \
     p.user_id AND u.email = :email";
    "DELETE FROM posts WHERE published_at < :before RETURNING id, title";
    "DELETE FROM users u USING posts p WHERE p.user_id = u.id AND p.title = :t";
  ]

let oracle url () =
  let db =
    match Caqti_blocking.connect (Uri.of_string url) with
    | Ok db -> db
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  let (module Db : Caqti_blocking.CONNECTION) = db in
  Fun.protect ~finally:Db.disconnect @@ fun () ->
  let literal sql =
    Db.exec
      (Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit
         Caqti_mult.zero (fun _ -> Caqti_query.L sql))
      ()
  in
  let exec sql =
    match literal sql with
    | Ok () -> ()
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  (* A schema of its own: other suites use the same database. *)
  List.iter exec
    ([
       "DROP SCHEMA IF EXISTS qb_oracle_test CASCADE";
       "CREATE SCHEMA qb_oracle_test";
       "SET search_path TO qb_oracle_test";
     ]
    @ split oracle_schema);
  let st = ok_or_fail oracle_schema (Schema.of_sql oracle_schema) in
  (* Postgres's names for a prepared statement's parameter and result types,
     e.g. "int8,text,_text". *)
  let described =
    let names col =
      Printf.sprintf
        "array_to_string(array(SELECT t.typname::text FROM unnest(p.%s) WITH \
         ORDINALITY u(o, i) JOIN pg_type t ON t.oid = u.o::oid ORDER BY u.i), \
         ',')"
        col
    in
    Caqti_request.Infix.(Caqti_type.unit ->! Caqti_type.(t2 string string))
      (Printf.sprintf
         "SELECT %s, %s FROM pg_prepared_statements p WHERE p.name = \
          'qb_oracle'"
         (names "parameter_types") (names "result_types"))
  in
  let pg_name (ty : Schema.pg_type) =
    let base =
      match String.rindex_opt ty.name '.' with
      | Some i -> String.sub ty.name (i + 1) (String.length ty.name - i - 1)
      | None -> ty.name
    in
    if ty.array_dims > 0 then "_" ^ base else base
  in
  (* A parameter is sent as an OCaml value, and Postgres takes any type of
     the same kind for it: an int for INTEGER or BIGINT, a string for TEXT or
     VARCHAR. Results are compared exactly. *)
  let kind = function
    | "int2" | "int4" | "int8" -> "integer"
    | "float4" | "float8" -> "float"
    | "text" | "varchar" | "bpchar" | "name" -> "text"
    | "timestamp" | "timestamptz" -> "timestamp"
    | n -> n
  in
  let wrong = ref [] in
  let report fmt = Printf.ksprintf (fun m -> wrong := m :: !wrong) fmt in
  List.iter
    (fun sql ->
      match Query.one st sql with
      | Error e -> report "%s\n    qb: %s" sql e.message
      | Ok q -> (
          match literal ("PREPARE qb_oracle AS " ^ q.sql) with
          | Error e -> report "%s\n    Postgres: %s" sql (Caqti_error.show e)
          | Ok () ->
              let params, results =
                match Db.find described () with
                | Ok x -> x
                | Error e -> Alcotest.fail (Caqti_error.show e)
              in
              exec "DEALLOCATE qb_oracle";
              let ours_params =
                List.map (fun (p : Query.param) -> pg_name p.ty) q.params
              in
              let ours_results =
                List.concat_map
                  (function
                    | Query.Column c -> [ pg_name c.ty ]
                    | Whole { table; _ } ->
                        List.map
                          (fun (c : Schema.column) -> pg_name c.ty)
                          table.columns)
                  q.row
              in
              let list s = if s = "" then [] else String.split_on_char ',' s in
              if List.map kind ours_params <> List.map kind (list params) then
                report "%s\n    parameters: qb %s, Postgres %s" sql
                  (String.concat "," ours_params)
                  params;
              if ours_results <> list results then
                report "%s\n    columns: qb %s, Postgres %s" sql
                  (String.concat "," ours_results)
                  results))
    oracle_queries;
  match List.rev !wrong with
  | [] -> ()
  | wrong ->
      Alcotest.failf "%d of %d queries differ from Postgres:\n\n%s"
        (List.length wrong)
        (List.length oracle_queries)
        (String.concat "\n\n" wrong)

(* ---------- can it be NULL: real rows ---------- *)

(* What Postgres won't say for a prepared statement, it shows when the
   statement runs: each query is run over rows that hold a NULL wherever one
   can be, over users with no posts, and over empty tables, and no column
   worked out here as never NULL may come back NULL. (The other way round, a
   column called nullable that never is, costs an [option] and breaks
   nothing.) *)

let null_queries =
  [
    (* outer joins, each side *)
    "SELECT u.email, p.title FROM users u LEFT JOIN posts p ON p.user_id = u.id";
    "SELECT u.email, p.title FROM posts p RIGHT JOIN users u ON u.id = \
     p.user_id";
    "SELECT u.email, p.title FROM users u FULL JOIN posts p ON p.user_id = \
     u.id AND p.body IS NOT NULL";
    "SELECT u.*, p.* FROM users u FULL JOIN posts p ON p.user_id = u.id AND \
     p.body IS NOT NULL";
    "SELECT a.email, b.email, p.title FROM users a LEFT JOIN (posts p JOIN \
     users b ON b.id = p.user_id) ON b.id = a.id";
    "SELECT u.id, count(p.id), max(p.title), min(p.views) FROM users u LEFT \
     JOIN posts p ON p.user_id = u.id GROUP BY u.id";
    "SELECT user_id, max(body), count(body), count(*) FROM posts GROUP BY \
     user_id";
    (* expressions over columns that can be NULL *)
    "SELECT lower(name), upper(nick), length(name), nick || email, \
     concat(name, nick), substr(name, 1, 2), position('a' in name) FROM users";
    "SELECT age + small, -small, abs(small), small * 2, r + d, age + d FROM \
     users";
    "SELECT name = 'x', name LIKE 'a%', NOT (name = 'x'), small > 1 AND \
     active, small IN (1, 2), small BETWEEN 1 AND 2, name IS NULL, name IS \
     DISTINCT FROM 'x', (name = 'x') IS TRUE FROM users";
    "SELECT CASE WHEN active THEN name ELSE email END, CASE WHEN name IS NULL \
     THEN 1 END, CASE WHEN active THEN 1 ELSE small END FROM users";
    "SELECT COALESCE(name, nick), COALESCE(name, nick, 'x'), COALESCE(small, \
     age), NULLIF(email, 'nulls@x'), GREATEST(small, age), LEAST(small, small) \
     FROM users";
    "SELECT name::varchar(3), small::bigint, born::text, score::float8, \
     email::varchar(3) FROM users";
    "SELECT date_part('year', born), date_trunc('day', born), \
     date_part('year', created_at), to_char(born, 'YYYY') FROM users";
    "SELECT meta->>'k', meta->'k', meta->>'missing' FROM users";
    "SELECT sum(small), sum(age), min(name), max(born), bool_or(active), \
     string_agg(name, ','), count(name) FROM users";
    "SELECT sum(views), max(views) - min(views), COALESCE(sum(views), 0) FROM \
     posts WHERE false";
  ]

let nulls url () =
  let db =
    match Caqti_blocking.connect (Uri.of_string url) with
    | Ok db -> db
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  let (module Db : Caqti_blocking.CONNECTION) = db in
  Fun.protect ~finally:Db.disconnect @@ fun () ->
  let literal sql =
    Db.exec
      (Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit
         Caqti_mult.zero (fun _ -> Caqti_query.L sql))
      ()
  in
  let exec sql =
    match literal sql with
    | Ok () -> ()
    | Error e -> Alcotest.fail (Caqti_error.show e)
  in
  List.iter exec
    ([
       "DROP SCHEMA IF EXISTS qb_nulls_test CASCADE";
       "CREATE SCHEMA qb_nulls_test";
       "SET search_path TO qb_nulls_test";
     ]
    @ split oracle_schema
    @ [
        (* One user with a NULL wherever there can be one, and no posts; one
           with everything, a bare post and a full one. *)
        "INSERT INTO users (email, age) VALUES ('nulls@x', 1)";
        {|INSERT INTO users (email, name, nick, age, small, r, d, score, tags,
            meta, m, "type", born)
          VALUES ('full@x', 'Full', 'f', 30, 2, 1.5, 2.5, 9.5, '{a}',
            '{"k": 1}', 'ok', 't', CURRENT_DATE)|};
        "INSERT INTO posts (user_id, title) VALUES (2, 'bare')";
        "INSERT INTO posts (user_id, title, body, published_at) VALUES (2, \
         'full', 'b', now())";
      ]);
  let st = ok_or_fail oracle_schema (Schema.of_sql oracle_schema) in
  (* A value of each type to stand for a parameter, or a NULL of it. *)
  let sample ~null (ty : Schema.pg_type) =
    let value =
      if null then "NULL"
      else if ty.array_dims > 0 then "'{}'"
      else
        match ty.name with
        | "int2" | "int4" | "int8" -> "1"
        | "float4" | "float8" | "numeric" -> "1.5"
        | "bool" -> "true"
        | "timestamp" | "timestamptz" -> "now()"
        | "date" -> "CURRENT_DATE"
        | "interval" -> "'1 day'"
        | "json" | "jsonb" -> "'{}'"
        | "mood" -> "'ok'"
        | _ -> "'a'"
    in
    Printf.sprintf "CAST(%s AS %s)" value (Schema.type_sql ty)
  in
  (* The statement with each $n written out. *)
  let filled ~null (q : Query.t) =
    let n = String.length q.sql in
    let b = Buffer.create (n + 64) in
    let rec go i =
      if i < n then
        if
          q.sql.[i] = '$'
          && i + 1 < n
          && q.sql.[i + 1] >= '0'
          && q.sql.[i + 1] <= '9'
        then (
          let rec stop j =
            if j < n && q.sql.[j] >= '0' && q.sql.[j] <= '9' then stop (j + 1)
            else j
          in
          let j = stop (i + 1) in
          let p =
            List.nth q.params
              (int_of_string (String.sub q.sql (i + 1) (j - i - 1)) - 1)
          in
          Buffer.add_string b (sample ~null:(null && p.nullable) p.ty);
          go j)
        else (
          Buffer.add_char b q.sql.[i];
          go (i + 1))
    in
    go 0;
    Buffer.contents b
  in
  let wrong = ref [] in
  let report fmt = Printf.ksprintf (fun m -> wrong := m :: !wrong) fmt in
  let checked = ref 0 in
  List.iter
    (fun sql ->
      match Query.one st sql with
      | Error e -> report "%s\n    qb: %s" sql e.message
      | Ok q ->
          (* Each column's name and whether it was worked out as nullable. *)
          let cols =
            List.concat_map
              (function
                | Query.Column c -> [ (c.name, c.nullable) ]
                | Whole { table; nullable } ->
                    List.map
                      (fun (c : Schema.column) ->
                        (table.name ^ "." ^ c.name, nullable || not c.not_null))
                      table.columns)
              q.row
          in
          if cols <> [] then (
            let names =
              List.mapi (fun i _ -> Printf.sprintf "c%d" (i + 1)) cols
            in
            let run ~what ~null ~empty =
              let text =
                Printf.sprintf "WITH t(%s) AS (%s) SELECT concat(%s) FROM t"
                  (String.concat ", " names) (filled ~null q)
                  (String.concat ", "
                     (List.map
                        (fun c -> Printf.sprintf "(%s IS NULL)::int" c)
                        names))
              in
              exec "BEGIN";
              if empty then (
                exec "DELETE FROM posts";
                exec "DELETE FROM users");
              (match
                 Db.collect_list
                   (Caqti_request.create ~oneshot:true Caqti_type.unit
                      Caqti_type.string Caqti_mult.zero_or_more (fun _ ->
                        Caqti_query.L text))
                   ()
               with
              | Error e ->
                  report "%s\n    (%s) Postgres: %s" sql what
                    (Caqti_error.show e)
              | Ok rows ->
                  incr checked;
                  List.iteri
                    (fun i (name, nullable) ->
                      if
                        (not nullable)
                        && List.exists (fun row -> row.[i] = '1') rows
                      then
                        report
                          "%s\n\
                          \    (%s) column %d, %s, is NULL, and was worked out \
                           as never NULL"
                          sql what (i + 1) name)
                    cols);
              exec "ROLLBACK"
            in
            run ~what:"rows" ~null:false ~empty:false;
            run ~what:"no rows" ~null:false ~empty:true;
            if List.exists (fun (p : Query.param) -> p.nullable) q.params then
              run ~what:"NULL parameters" ~null:true ~empty:false))
    (oracle_queries @ null_queries);
  Alcotest.(check bool) "queries were run" true (!checked > 150);
  match List.sort_uniq compare !wrong with
  | [] -> ()
  | wrong ->
      Alcotest.failf "%d findings:\n\n%s" (List.length wrong)
        (String.concat "\n\n" wrong)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "qb-pg-parse"
    [
      ( "pg",
        [
          t "parses" parses;
          t "syntax error" syntax_error;
          t "caqti placeholders" caqti_placeholders;
          t "caqti error position" caqti_error_position;
        ] );
      ( "qb sql",
        [ t "postgres parses it" qb_sql_parses; t "mysql" mysql_sql_does_not ]
      );
      ( "schema",
        [
          t "migrations" replays_migrations;
          t "composite, warnings" composite_and_warnings;
        ] );
      ( "queries",
        [
          t "types" query_types;
          t "IS NULL cast" null_test_cast;
          t "errors" query_errors;
          (match Sys.getenv_opt "QB_TEST_PG_URL" with
          | Some url -> t "types agree with postgres" (oracle url)
          | None ->
              t "types agree with postgres: set QB_TEST_PG_URL to run"
                (fun () -> Alcotest.skip ()));
          (match Sys.getenv_opt "QB_TEST_PG_URL" with
          | Some url -> t "never NULL holds on postgres" (nulls url)
          | None ->
              t "never NULL holds on postgres: set QB_TEST_PG_URL to run"
                (fun () -> Alcotest.skip ()));
        ] );
      ( "codegen",
        [
          t "round trip" round_trip;
          t "qb table round trip" qb_table_round_trip;
          t "idents" idents;
          t "options" codegen_options;
          t "psql meta-commands" strips_psql_meta;
          t "generated on sqlite" generated_on_sqlite;
          t "queries on sqlite" queries_on_sqlite;
          (match Sys.getenv_opt "QB_TEST_PG_URL" with
          | Some url -> t "generated on postgres" (generated_on_postgres url)
          | None ->
              t "generated on postgres: set QB_TEST_PG_URL to run" (fun () ->
                  Alcotest.skip ()));
        ] );
    ]
