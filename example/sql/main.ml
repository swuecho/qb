(* SQL written inline with [%sql], typed from migrations/*.sql.
   The walkthrough is doc/tutorial.md.
   Run with: dune exec ./example/sql/main.exe

   It runs on an in-memory SQLite database so there is nothing to set up;
   the SQL here is the subset SQLite and Postgres share. *)

open Schema
open Qb.Blocking

(* ---------- the queries: no type is written anywhere ---------- *)

(* Two tables, some columns: a tuple.
   string -> (string * int) Qb.stmt *)
let titles_by author =
  [%sql
    {|SELECT b.title, b.year
      FROM books b JOIN authors a ON a.id = b.author_id
      WHERE a.name = :author
      ORDER BY b.year|}]

(* Three tables, whole rows.
   int -> (Reviews.t * Books.t * Authors.t) Qb.stmt *)
let reviews_with_at_least stars =
  [%sql
    {|SELECT r.*, b.*, a.*
      FROM reviews r
      JOIN books b ON b.id = r.book_id
      JOIN authors a ON a.id = b.author_id
      WHERE r.stars >= :stars
      ORDER BY r.id|}]

(* A left join: what comes from reviews may be missing, so it is an option,
   even r.stars, which is NOT NULL in the table.
   (string * int option * string option) Qb.stmt *)
let books_and_reviews =
  [%sql
    {|SELECT b.title, r.stars, r.note
      FROM books b LEFT JOIN reviews r ON r.book_id = b.id
      ORDER BY b.title, r.id|}]

(* Aggregates. count is never NULL; max is, over no rows; for avg, which qb
   doesn't know, a cast says the type.
   (string * int * int option * float option) Qb.stmt *)
let author_stats =
  [%sql
    {|SELECT a.name,
             count(DISTINCT b.id) AS books,
             max(b.year) AS latest,
             CAST(avg(r.stars) AS REAL) AS stars
      FROM authors a
      LEFT JOIN books b ON b.author_id = a.id
      LEFT JOIN reviews r ON r.book_id = b.id
      GROUP BY a.name
      ORDER BY books DESC, a.name|}]

(* An optional filter: :country? may be NULL, so it is an option.
   country:string option -> limit:int -> Authors.t Qb.stmt *)
let authors ~country ~limit =
  [%sql
    {|SELECT * FROM authors
      WHERE :country? IS NULL OR country = :country
      ORDER BY name
      LIMIT :limit|}]

(* IN, BETWEEN and LIKE type their parameters like = does.
   string -> int -> int -> string Qb.stmt *)
let search pattern from until =
  [%sql
    {|SELECT title FROM books
      WHERE title LIKE :pattern AND year BETWEEN :from AND :until
      ORDER BY title|}]

(* A list of any length: IN :years.
   int list -> string Qb.stmt *)
let titles_in years =
  [%sql "SELECT title FROM books WHERE year IN :years ORDER BY title"]

(* Rows with no match: something the builder's join can't say.
   string Qb.stmt *)
let unreviewed =
  [%sql
    {|SELECT b.title
      FROM books b LEFT JOIN reviews r ON r.book_id = b.id
      WHERE r.id IS NULL
      ORDER BY b.title|}]

(* Writes. Each parameter takes the type of the column it goes into, and
   RETURNING is typed like a select list.
   book:int -> stars:int -> note:string option -> Reviews.t Qb.stmt *)
let add_review ~book ~stars ~note =
  [%sql
    {|INSERT INTO reviews (book_id, stars, note)
      VALUES (:book, :stars, :note?)
      RETURNING *|}]

(* int -> int -> int option Qb.stmt: pages can be NULL, so pages + :n can. *)
let add_pages book n =
  [%sql "UPDATE books SET pages = pages + :n WHERE id = :book RETURNING pages"]

(* No RETURNING, no rows: int -> unit Qb.stmt *)
let drop_reviews_below stars = [%sql "DELETE FROM reviews WHERE stars < :stars"]

(* What [%sql] doesn't read (here, a subquery) is written with its types
   given. *)
let authors_without_books : string Qb.stmt =
  Qb.Stmt.(
    make
      "SELECT name FROM authors a WHERE NOT EXISTS (SELECT 1 FROM books b \
       WHERE b.author_id = a.id) ORDER BY name"
      [] (ret Qb.text))

(* ---------- running them ---------- *)

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e
let opt f = function Some x -> f x | None -> "-"

let demo () =
  let* db = Caqti_blocking.connect (Uri.of_string "sqlite3::memory:") in
  let* () = create_table db Authors.table in
  let* () = create_table db Books.table in
  let* () = create_table db Reviews.table in

  (* Rows go in with the builder: ids come back from the database. *)
  let* ursula =
    insert_values db (Authors.values ~name:"Ursula" ~country:"US" ())
  in
  let* italo =
    insert_values db (Authors.values ~name:"Italo" ~country:"IT" ())
  in
  let* _anon = insert_values db (Authors.values ~name:"Anon" ()) in
  let book (a : Authors.t) title year pages =
    insert_values db (Books.values ~author_id:a.id ~title ~year ?pages ())
  in
  let* left_hand = book ursula "The Left Hand" 1969 (Some 286) in
  let* dispossessed = book ursula "The Dispossessed" 1974 None in
  let* cities = book italo "Invisible Cities" 1972 (Some 165) in
  let review (b : Books.t) stars note =
    insert_values db (Reviews.values ~book_id:b.id ~stars ?note ())
  in
  let* _ = review left_hand 5 (Some "cold") in
  let* _ = review left_hand 4 None in
  let* _ = review cities 5 (Some "short") in

  (* ---------- the builder: one table, or two with a join ---------- *)
  print_endline "-- builder: books after 1970 with a page count, newest first";
  let* rows =
    Qb.(
      from Books.table
      |> where Ops.(Books.year > 1970 && Books.pages <> None)
      |> order_by [ desc Books.year ]
      |> limit 10 |> all db)
  in
  List.iter (fun (b : Books.t) -> Printf.printf "  %d %s\n" b.year b.title) rows;

  print_endline "\n-- builder: some columns, a list of values, a count";
  let* rows =
    Qb.(
      from Books.table
      |> select [ Books.title; Books.year ]
      |> where Ops.(is_in Books.year [ 1969; 1972 ])
      |> order_by [ asc Books.title ]
      |> all db)
  in
  List.iter
    (fun Qb.Row.[ title; year ] -> Printf.printf "  %d %s\n" year title)
    rows;
  let* n =
    Qb.(from Reviews.table |> where Ops.(Reviews.stars >= 5) |> count db)
  in
  Printf.printf "  %d reviews of 5 stars\n" n;

  (* A query is a value: a filter can be added, or not, at run time. *)
  let books_of_year year =
    Qb.(
      from Books.table
      |> where_opt (Option.map (fun y -> Ops.(Books.year = y)) year)
      |> count db)
  in
  let* all_books = books_of_year None in
  let* in_1972 = books_of_year (Some 1972) in
  Printf.printf "  %d books, %d from 1972\n" all_books in_1972;

  print_endline "\n-- builder: update and delete";
  let* () =
    update db Qb.Ops.[ Books.pages := Some 341 ] (Books.by_pk dispossessed.id)
  in
  let* b = first db (Books.by_pk dispossessed.id) in
  Printf.printf "  %s\n"
    (opt (fun (b : Books.t) -> opt string_of_int b.pages) b);
  let* extra = review cities 1 None in
  let* () =
    delete db Qb.(from Reviews.table |> where Ops.(Reviews.stars < 2))
  in
  let* gone = first db (Reviews.by_pk extra.id) in
  Printf.printf "  the 1-star review is %s\n"
    (if gone = None then "gone" else "there");

  print_endline "\n-- builder: books with their author (US authors only)";
  let* rows =
    Qb.(
      from Books.table
      |> join Authors.table
           ~on:Ops.(Books.author_id == Authors.id)
           ~where:Ops.(Authors.country = Some "US")
      |> where Ops.(Books.year > 1960)
      |> order_by [ asc Books.year ]
      |> all db)
  in
  List.iter
    (fun ((b : Books.t), (a : Authors.t)) ->
      Printf.printf "  %d %s, by %s\n" b.year b.title a.name)
    rows;

  print_endline "\n-- builder: every book, with a review if it has one";
  let* rows =
    Qb.(
      from Books.table |> select [ Books.title ]
      |> left_join Reviews.table ~on:Ops.(Books.id == Reviews.book_id)
      |> order_by [ asc Books.title ]
      |> all db)
  in
  List.iter
    (fun (Qb.Row.[ title ], (r : Reviews.t option)) ->
      Printf.printf "  %-18s %s\n" title
        (opt (fun (r : Reviews.t) -> string_of_int r.stars) r))
    rows;

  (* ---------- [%sql]: everything else ---------- *)
  print_endline "\n-- titles by Ursula";
  let* rows = fetch_all db (titles_by "Ursula") in
  List.iter (fun (title, year) -> Printf.printf "  %d %s\n" year title) rows;

  print_endline "\n-- reviews of 5 stars, with book and author";
  let* rows = fetch_all db (reviews_with_at_least 5) in
  List.iter
    (fun ((r : Reviews.t), (b : Books.t), (a : Authors.t)) ->
      Printf.printf "  %d* %s, by %s\n" r.stars b.title a.name)
    rows;

  print_endline "\n-- every book, with its reviews if any";
  let* rows = fetch_all db books_and_reviews in
  List.iter
    (fun (title, stars, note) ->
      Printf.printf "  %-18s %s %s\n" title (opt string_of_int stars)
        (opt Fun.id note))
    rows;

  print_endline "\n-- per author";
  let* rows = fetch_all db author_stats in
  List.iter
    (fun (name, books, latest, stars) ->
      Printf.printf "  %-7s %d books, latest %s, %s stars\n" name books
        (opt string_of_int latest)
        (opt (Printf.sprintf "%.1f") stars))
    rows;

  print_endline "\n-- authors: all, then only IT";
  let names country =
    let* rows = fetch_all db (authors ~country ~limit:10) in
    Ok (String.concat ", " (List.map (fun (a : Authors.t) -> a.name) rows))
  in
  let* all = names None in
  let* it = names (Some "IT") in
  Printf.printf "  %s\n  %s\n" all it;

  print_endline "\n-- titles with 'The', 1970 to 1980";
  let* rows = fetch_all db (search "The%" 1970 1980) in
  List.iter (Printf.printf "  %s\n") rows;

  print_endline "\n-- titles from 1969 or 1972, then from no year at all";
  let* rows = fetch_all db (titles_in [ 1969; 1972 ]) in
  let* none = fetch_all db (titles_in []) in
  Printf.printf "  %s\n  (%d rows)\n" (String.concat ", " rows)
    (List.length none);

  print_endline "\n-- books with no review";
  let* rows = fetch_all db unreviewed in
  List.iter (Printf.printf "  %s\n") rows;

  print_endline "\n-- writes";
  let* r =
    fetch_one db (add_review ~book:dispossessed.id ~stars:3 ~note:None)
  in
  Printf.printf "  review %d: %d stars\n" r.id r.stars;
  let* pages = fetch_one db (add_pages left_hand.id 14) in
  Printf.printf "  The Left Hand now has %s pages\n" (opt string_of_int pages);
  let* () = execute db (drop_reviews_below 4) in
  let* n = Qb.(from Reviews.table |> count db) in
  Printf.printf "  %d reviews left\n" n;

  print_endline "\n-- a statement written by hand";
  let* rows = fetch_all db authors_without_books in
  List.iter (Printf.printf "  %s\n") rows;
  Ok ()

let () =
  match demo () with
  | Ok () -> ()
  | Error e ->
      prerr_endline (Caqti_error.show e);
      exit 1
