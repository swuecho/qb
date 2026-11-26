(* Joins in qb, on an in-memory SQLite database.
   Run with: dune exec ./example/join.exe *)

open Qb
open Qb.Blocking

module Users = struct
  type t = { id : int; name : string; manager_id : int option }

  let id = column "id" int (fun u -> u.id) ~primary_key:true
  let name = column "name" text (fun u -> u.name)
  let manager_id = column "manager_id" (nullable int) (fun u -> u.manager_id)

  let table =
    table "users"
      (let+ id = col id and+ name = col name and+ manager_id = col manager_id in
       { id; name; manager_id })
end

module Comments = struct
  type t = { id : int; author_id : int; body : string }

  let id = column "id" int (fun c -> c.id) ~primary_key:true
  let author_id = column "author_id" int (fun c -> c.author_id)
  let body = column "body" text (fun c -> c.body)

  let table =
    table "comments"
      (let+ id = col id and+ author_id = col author_id and+ body = col body in
       { id; author_id; body })
end

(* These do not type-check (the error says "Type joined is not compatible
   with type plain"): a joined query can be filtered, ordered and read, not
   reshaped or written through.
     joined |> select (col Comments.body)            select after the join
     joined |> join Users.table ~on                  a second join
     update db [ Comments.body := "x" ] joined       update through a join
     delete db joined                                delete through a join *)

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let demo () =
  let* db = Caqti_blocking.connect (Uri.of_string "sqlite3::memory:") in
  let* () = create_table db Users.table in
  let* () = create_table db Comments.table in
  let* () = insert db Users.table { id = 1; name = "Ann"; manager_id = None } in
  let* () =
    insert db Users.table { id = 2; name = "Bob"; manager_id = Some 1 }
  in
  let* () =
    insert db Users.table { id = 3; name = "Cy"; manager_id = Some 1 }
  in
  let* () =
    insert db Comments.table { id = 10; author_id = 2; body = "looks good" }
  in
  let* () =
    insert db Comments.table { id = 11; author_id = 1; body = "ship it" }
  in
  let* () =
    insert db Comments.table { id = 12; author_id = 2; body = "one more thing" }
  in

  (* 1. Inner join: each comment with its author, as pairs. The query is
     still about comments: where and order_by take Comments' own columns,
     before or after the join. The users' own condition is the join's
     ~where. *)
  let with_authors =
    from Comments.table
    |> join Users.table
         ~on:Ops.(Comments.author_id == Users.id)
         ~where:Ops.(Users.name <> "Cy")
    |> order_by [ asc Comments.id ]
  in
  print_endline "-- comments with their authors";
  print_endline ("  " ^ to_sql with_authors);
  let* rows = all db with_authors in
  List.iter
    (fun ((c : Comments.t), (u : Users.t)) ->
      Printf.printf "  %d %-6s %s\n" c.id u.name c.body)
    rows;

  (* 2. Left join: every user, with a comment of theirs if any. *)
  let users_comments =
    from Users.table
    |> order_by [ asc Users.id ]
    |> left_join Comments.table ~on:Ops.(Users.id == Comments.author_id)
  in
  print_endline "\n-- every user, and their comments (None: no comments)";
  print_endline ("  " ^ to_sql users_comments);
  let* rows = all db users_comments in
  List.iter
    (fun ((u : Users.t), (c : Comments.t option)) ->
      Printf.printf "  %-4s %s\n" u.name
        (match c with Some c -> c.body | None -> "None"))
    rows;

  (* 3. A self join: users and their managers. Each side gets its own alias
     in the SQL, so the two [users] don't clash. Only the names are read on
     the left: its select carries over. *)
  let managers =
    from Users.table
    |> select (col Users.name)
    |> join Users.table ~on:Ops.(Users.manager_id == some Users.id)
  in
  print_endline "\n-- users and their managers";
  print_endline ("  " ^ to_sql managers);
  let* rows = all db managers in
  List.iter
    (fun (user, (m : Users.t)) ->
      Printf.printf "  %-4s reports to %s\n" user m.name)
    rows;
  Ok ()

let () =
  match demo () with
  | Ok () -> ()
  | Error e ->
      prerr_endline (Caqti_error.show e);
      exit 1
