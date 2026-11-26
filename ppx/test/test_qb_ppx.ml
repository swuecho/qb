(* [%sql "..."]: SQL written where it is used, typed from the migrations in
   pg-parse/test/schema (see dune). Nothing here says a type: parameters
   and rows are worked out from the schema. *)

open Schema_gen

(* :email is the argument [email]; the columns make a tuple. *)
let id_and_nickname email =
  [%sql "SELECT id, nickname FROM accounts WHERE email = :email"]

(* One column is the bare value. Written in the quoted form. *)
let article_count account_id =
  {%sql|SELECT count(*) FROM articles WHERE account_id = :account_id|}

(* t.* is the table's own row, [None] on the outer side of a join. *)
let accounts_and_articles =
  [%sql
    {|SELECT ac.*, ar.*
      FROM accounts ac
      LEFT JOIN articles ar ON ar.account_id = ac.id
      ORDER BY ac.id, ar.title|}]

(* Whole rows and columns together; a parameter used twice. *)
let rated ~low ~limit =
  [%sql
    {|SELECT ac.*, ar.title, ar.rating
      FROM articles ar
      JOIN accounts ac ON ac.id = ar.account_id
      WHERE ar.rating >= :low OR ar.rating < :low - 100
      ORDER BY ar.title
      LIMIT :limit|}]

(* Postgres only (a cast): :nickname? may be NULL, so it is an option. *)
let by_nickname nickname =
  [%sql
    {|SELECT email FROM accounts
      WHERE (:nickname?::text IS NULL AND nickname IS NULL)
         OR nickname = :nickname
      ORDER BY email|}]

(* An optional filter with no cast: the other use types the parameter. *)
let maybe_nickname nick =
  [%sql
    {|SELECT email FROM accounts
      WHERE :nick? IS NULL OR nickname = :nick
      ORDER BY email|}]

(* IN :emails is a list of any length. *)
let with_emails emails =
  [%sql "SELECT email FROM accounts WHERE email IN :emails ORDER BY email"]

let without_emails emails =
  [%sql "SELECT email FROM accounts WHERE email NOT IN :emails ORDER BY email"]

(* Writes. A parameter takes the type of the column it goes into;
   RETURNING is typed like a select list, and without it there are no rows. *)
let add_account ~email ~nickname ~now =
  [%sql
    {|INSERT INTO accounts (email, nickname, active, created_at, login_count)
      VALUES (:email, :nickname?, true, :now, 0)
      RETURNING *|}]

let bump id n =
  [%sql
    {|UPDATE accounts SET login_count = login_count + :n
      WHERE id = :id
      RETURNING login_count|}]

let rename id nickname =
  [%sql "UPDATE accounts SET nickname = :nickname? WHERE id = :id"]

(* The full name, for when another ppx also answers to [%sql]. *)
let remove ids = [%qb.sql "DELETE FROM accounts WHERE id IN :ids"]
let _ = (with_emails : string list -> string Qb.stmt)

let _ =
  (add_account
    : email:string ->
      nickname:string option ->
      now:Ptime.t ->
      Accounts.t Qb.stmt)

let _ = (bump : int -> int -> int Qb.stmt)
let _ = (rename : int -> string option -> unit Qb.stmt)
let _ = (remove : int list -> unit Qb.stmt)

(* Checked by the type checker only: each shape the reference describes. *)
module Shapes = struct
  (* HAVING count > :n, COALESCE(col, :p), IS NOT DISTINCT FROM :p?. *)
  let a n fallback nick =
    [%sql
      {|SELECT email, COALESCE(nickname, :fallback)
        FROM accounts
        WHERE nickname IS NOT DISTINCT FROM :nick?
        GROUP BY email, nickname
        HAVING count(*) > :n|}]

  let _ = (a : int -> string -> string option -> (string * string) Qb.stmt)

  (* Everything inside the outer side of a join is nullable. *)
  let b =
    [%sql
      {|SELECT e.account_id, ar.title, ac.email
        FROM audit.accounts e
        LEFT JOIN (articles ar JOIN accounts ac ON ac.id = ar.account_id)
          ON ac.id = e.account_id|}]

  let _ = (b : (int * string option * string option) Qb.stmt)

  (* A keyword as a parameter name; arithmetic with a cast. *)
  let c type_ n =
    [%sql
      {|SELECT title FROM articles
        WHERE "type" = :type AND account_id > 1 + :n::int|}]

  let _ = (c : string -> int -> string Qb.stmt)

  (* * over two tables (a comma join, a self join with aliases). *)
  let d = [%sql "SELECT * FROM accounts a, accounts b WHERE a.id < b.id"]
  let _ = (d : (Accounts.t * Accounts.t) Qb.stmt)

  (* Arithmetic on integers is an integer; other casts say the type and
     may be NULL; constants have a type; a decimal is NUMERIC. *)
  let e =
    [%sql
      {|SELECT (login_count / 100)::int AS hundreds, lower(email)::text,
               COALESCE(lower(nickname)::text, ''), 42, true, 1.5
        FROM accounts|}]

  let _ = (e : (int * string * string * int * bool * string) Qb.stmt)

  (* A table in another schema is its own module. *)
  let f id = [%sql "SELECT e.* FROM audit.accounts e WHERE e.event_id = :id"]
  let _ = (f : int -> Audit.Accounts.t Qb.stmt)

  (* Arithmetic types a parameter from a column, and || is text. *)
  let g n suffix =
    [%sql
      {|SELECT login_count * :n, email || :suffix, login_count + id
        FROM accounts|}]

  let _ = (g : int -> string -> (int * string * int) Qb.stmt)

  (* An upsert: EXCLUDED is the row that was to be inserted. *)
  let h email now =
    [%sql
      {|INSERT INTO accounts (email, active, created_at, login_count)
        VALUES (:email, true, :now, 1)
        ON CONFLICT (email) DO UPDATE
          SET login_count = accounts.login_count + excluded.login_count
        RETURNING id, login_count|}]

  let _ = (h : string -> Ptime.t -> (int * int) Qb.stmt)

  (* UPDATE ... FROM and DELETE ... USING bring another table into scope. *)
  let i title =
    [%sql
      {|UPDATE accounts SET login_count = 0
        FROM articles ar
        WHERE ar.account_id = accounts.id AND ar.title = :title
        RETURNING accounts.email, ar.rating|}]

  let _ = (i : string -> (string * float option) Qb.stmt)

  let j email =
    [%sql
      {|DELETE FROM articles USING accounts ac
        WHERE ac.id = articles.account_id AND ac.email = :email|}]

  let _ = (j : string -> unit Qb.stmt)

  (* Several rows, DO NOTHING; no column list; DEFAULT VALUES. *)
  let k a b now =
    [%sql
      {|INSERT INTO accounts (email, active, created_at, login_count)
        VALUES (:a, true, :now, 0), (:b, false, :now, 1)
        ON CONFLICT DO NOTHING|}]

  let _ = (k : string -> string -> Ptime.t -> unit Qb.stmt)

  let l event account at =
    [%sql
      {|INSERT INTO audit.accounts VALUES (:event, :account, :at)
        RETURNING event_id|}]

  let _ = (l : int -> int -> Ptime.t -> int Qb.stmt)
  let m = [%sql "INSERT INTO accounts (email) VALUES ('m@x.org') RETURNING id"]
  let _ = (m : int Qb.stmt)
end

(* The types the ppx worked out, written down so a change shows up here. *)
let _ = (id_and_nickname : string -> (int * string option) Qb.stmt)
let _ = (article_count : int -> int Qb.stmt)
let _ = (accounts_and_articles : (Accounts.t * Articles.t option) Qb.stmt)

let _ =
  (rated
    : low:float -> limit:int -> (Accounts.t * string * float option) Qb.stmt)

let _ = (by_nickname : string option -> string Qb.stmt)

(* Functions, conditions and CASE have types without a cast. A function of
   a nullable column, or a sum over no rows, may be NULL. *)
let facts =
  [%sql
    {|SELECT lower(email), length(nickname), login_count > 3,
             nickname IS NULL, CASE WHEN active THEN 'on' ELSE 'off' END
      FROM accounts
      ORDER BY email|}]

let _ = (facts : (string * int option * bool * bool * string) Qb.stmt)

let totals =
  [%sql "SELECT sum(login_count), max(length(email)), count(*) FROM accounts"]

let _ = (totals : (int option * int option * int) Qb.stmt)
let clock = [%sql "SELECT now(), CURRENT_DATE"]
let _ = (clock : (Ptime.t * Ptime.t) Qb.stmt)

(* [%sql.obj]: a row is an object with a method per column, named after the
   column or its AS; a t.* is named after its table. *)
let account_obj id =
  [%sql.obj "SELECT id, email, nickname AS nick FROM accounts WHERE id = :id"]

let _ =
  (account_obj
    : int -> < id : int ; email : string ; nick : string option > Qb.stmt)

let titled =
  [%sql.obj
    {|SELECT ac.*, ar.title
      FROM accounts ac JOIN articles ar ON ar.account_id = ac.id
      ORDER BY ar.title|}]

let _ = (titled : < accounts : Accounts.t ; title : string > Qb.stmt)
let get = function Ok x -> x | Error e -> Alcotest.fail (Caqti_error.show e)

let run ?created_at db =
  let account email nickname active =
    get
      (Qb.Blocking.insert_values db
         (Accounts.values ~email ?nickname ~active ?created_at ~login_count:0 ()))
  in
  let ann = account "ann@example.com" None true in
  let bo = account "bo@example.com" (Some "bo") false in
  List.iter
    (fun (id, title, rating) ->
      ignore
        (get
           (Qb.Blocking.insert_values db
              (Articles.values
                 ~id:("00000000-0000-0000-0000-00000000000" ^ id)
                 ~account_id:ann.id ~title ~body:"..." ?rating ()))))
    [ ("1", "Apples", Some 4.5); ("2", "Bees", Some 2.0); ("3", "Cats", None) ];
  Alcotest.(check (option (pair int (option string))))
    "a tuple"
    (Some (bo.id, Some "bo"))
    (get (Qb.Blocking.fetch_opt db (id_and_nickname "bo@example.com")));
  Alcotest.(check int)
    "one column" 3
    (get (Qb.Blocking.fetch_one db (article_count ann.id)));
  Alcotest.(check (list (pair string (option int))))
    "functions"
    [ ("ann@example.com", None); ("bo@example.com", Some 2) ]
    (get (Qb.Blocking.fetch_all db facts)
    |> List.map (fun (email, len, _, _, _) -> (email, len)));
  Alcotest.(check (list (triple bool bool string)))
    "conditions and CASE"
    [ (false, true, "on"); (false, false, "off") ]
    (get (Qb.Blocking.fetch_all db facts)
    |> List.map (fun (_, _, many, anon, state) -> (many, anon, state)));
  Alcotest.(check (triple (option int) (option int) int))
    "aggregates" (Some 0, Some 15, 2)
    (get (Qb.Blocking.fetch_one db totals));
  let o = get (Qb.Blocking.fetch_one db (account_obj bo.id)) in
  Alcotest.(check (triple int string (option string)))
    "a row as an object"
    (bo.id, "bo@example.com", Some "bo")
    (o#id, o#email, o#nick);
  Alcotest.(check (list (pair string string)))
    "an object with a whole table"
    [
      ("ann@example.com", "Apples");
      ("ann@example.com", "Bees");
      ("ann@example.com", "Cats");
    ]
    (get (Qb.Blocking.fetch_all db titled)
    |> List.map (fun o -> (o#accounts.Accounts.email, o#title)));
  Alcotest.(check (list (pair string (option string))))
    "whole rows, None where nothing matched"
    [
      ("ann@example.com", Some "Apples");
      ("ann@example.com", Some "Bees");
      ("ann@example.com", Some "Cats");
      ("bo@example.com", None);
    ]
    (get (Qb.Blocking.fetch_all db accounts_and_articles)
    |> List.map (fun ((ac : Accounts.t), (ar : Articles.t option)) ->
        (ac.email, Option.map (fun (a : Articles.t) -> a.title) ar)));
  Alcotest.(check (list (triple string string (option (float 0.)))))
    "a row and columns"
    [ ("ann@example.com", "Apples", Some 4.5) ]
    (get (Qb.Blocking.fetch_all db (rated ~low:3.0 ~limit:5))
    |> List.map (fun ((ac : Accounts.t), title, rating) ->
        (ac.email, title, rating)));
  let strings = Alcotest.(check (list string)) in
  let all s = get (Qb.Blocking.fetch_all db s) in
  strings "IN a list" [ "bo@example.com" ]
    (all (with_emails [ "bo@example.com"; "nobody@example.com" ]));
  strings "IN an empty list" [] (all (with_emails []));
  strings "NOT IN a list" [ "ann@example.com" ]
    (all (without_emails [ "bo@example.com" ]));
  strings "NOT IN an empty list"
    [ "ann@example.com"; "bo@example.com" ]
    (all (without_emails []));
  let now = Option.get (Ptime.of_float_s 1_700_000_000.) in
  let cy =
    get
      (Qb.Blocking.fetch_one db
         (add_account ~email:"cy@example.com" ~nickname:None ~now))
  in
  Alcotest.(check (pair string (option string)))
    "INSERT ... RETURNING *" ("cy@example.com", None) (cy.email, cy.nickname);
  Alcotest.(check int)
    "UPDATE ... RETURNING" 2
    (get (Qb.Blocking.fetch_one db (bump cy.id 2)));
  get (Qb.Blocking.execute db (rename cy.id (Some "cy")));
  Alcotest.(check (option (pair int (option string))))
    "UPDATE with no rows back"
    (Some (cy.id, Some "cy"))
    (get (Qb.Blocking.fetch_opt db (id_and_nickname "cy@example.com")));
  get (Qb.Blocking.execute db (remove [ cy.id; -1 ]));
  strings "DELETE ... IN a list" [] (all (with_emails [ "cy@example.com" ]))

let on_sqlite () =
  let db = get (Caqti_blocking.connect (Uri.of_string "sqlite3::memory:")) in
  get (Qb.Blocking.create_table db Accounts.table);
  get (Qb.Blocking.create_table db Articles.table);
  run ~created_at:(Option.get (Ptime.of_float_s 1_700_000_000.)) db

(* On Postgres the tables come from qb's own DDL, so the defaults the
   migrations declare are given by hand. *)
let on_postgres url () =
  let db = get (Caqti_blocking.connect (Uri.of_string url)) in
  let (module Db : Caqti_blocking.CONNECTION) = db in
  Fun.protect ~finally:Db.disconnect @@ fun () ->
  (* A schema of its own: other test suites use the same database, and
     their own accounts table, at the same time. *)
  List.iter
    (fun sql -> get (Qb.Blocking.execute db (Qb.Stmt.make sql [] Qb.Stmt.unit)))
    [
      "DROP SCHEMA IF EXISTS qb_ppx_test CASCADE";
      "CREATE SCHEMA qb_ppx_test";
      "SET search_path TO qb_ppx_test";
    ];
  get (Qb.Blocking.create_table db Accounts.table);
  get (Qb.Blocking.create_table db Articles.table);
  run ~created_at:(Option.get (Ptime.of_float_s 1_700_000_000.)) db;
  let emails nickname = get (Qb.Blocking.fetch_all db (by_nickname nickname)) in
  Alcotest.(check (list string))
    "Some" [ "bo@example.com" ] (emails (Some "bo"));
  Alcotest.(check (list string)) "None" [ "ann@example.com" ] (emails None);
  let emails nick = get (Qb.Blocking.fetch_all db (maybe_nickname nick)) in
  Alcotest.(check (list string))
    "optional filter, given" [ "bo@example.com" ] (emails (Some "bo"));
  Alcotest.(check (list string))
    "optional filter, left out"
    [ "ann@example.com"; "bo@example.com" ]
    (emails None)

let () =
  let t name f = Alcotest.test_case name `Quick f in
  Alcotest.run "qb-ppx"
    [
      ( "queries",
        [
          t "on sqlite" on_sqlite;
          (match Sys.getenv_opt "QB_TEST_PG_URL" with
          | Some url -> t "on postgres" (on_postgres url)
          | None ->
              t "on postgres: set QB_TEST_PG_URL to run" (fun () ->
                  Alcotest.skip ()));
        ] );
    ]
