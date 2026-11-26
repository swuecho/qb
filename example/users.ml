(* Qb walkthrough on an in-memory SQLite database.
   Run with: dune exec ./example/users.exe *)

open Qb
open Qb.Blocking

module Users = struct
  type t = { id : int; name : string; age : int; email : string option }

  let id =
    column "id" int (fun u -> u.id) ~primary_key:true ~auto_increment:true

  let name = column "name" text (fun u -> u.name)
  let age = column "age" int (fun u -> u.age)
  let email = column "email" (nullable text) (fun u -> u.email)

  let table =
    table "users"
      (let+ id = col id
       and+ name = col name
       and+ age = col age
       and+ email = col email in
       { id; name; age; email })
end

(* These do not type-check:
     where Ops.(Users.age > "18")              string where int expected
     Ops.[ Users.age := "old" ]                string where int expected
     from Users.table |> where Ops.(Posts.id = 1)   column of another table
     from Users.table |> delete db             no where: use [where all_rows] *)

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let show (u : Users.t) =
  Printf.printf "  %d %-4s %d %s\n" u.id u.name u.age
    (Option.value u.email ~default:"-")

(* Print each statement qb runs, indented under the step that runs it. *)
let show_sql () =
  let report _src _level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Format.kfprintf
          (fun _ ->
            over ();
            k ())
          Format.std_formatter
          ("  sql> " ^^ fmt ^^ "@."))
  in
  Logs.set_reporter { Logs.report };
  Logs.Src.set_level Qb.src (Some Logs.Debug)

let demo () =
  let* db = Caqti_blocking.connect (Uri.of_string "sqlite3::memory:") in
  show_sql ();
  let* () = create_table db Users.table in

  print_endline "\n-- create";
  let* () =
    insert db Users.table
      { id = 1; name = "Ann"; age = 34; email = Some "ann@example.com" }
  in
  let* () =
    insert db Users.table { id = 2; name = "Bob"; age = 51; email = None }
  in
  let* () =
    insert db Users.table { id = 3; name = "Cy"; age = 17; email = None }
  in
  (* No id given: the database assigns it and we read it back. *)
  let* new_id =
    insert_returning db Users.table
      Ops.[ Users.name := "Dee"; Users.age := 29; Users.email := None ]
      (col Users.id)
  in
  Printf.printf "  new id = %d\n" new_id;

  print_endline "\n-- read: whole rows";
  let* adults =
    from Users.table
    |> where Ops.(Users.age > 18 && Users.name <> "Bob")
    |> order_by [ asc Users.name ]
    |> all db
  in
  List.iter show adults;

  print_endline "\n-- read: one row by id";
  let* ann = from Users.table |> where Ops.(Users.id = 1) |> first db in
  Option.iter show ann;

  print_endline "\n-- read: some columns, as a labeled tuple";
  let* names =
    from Users.table
    |> where Ops.(Users.email = None)
    |> select
         (let+ id = col Users.id and+ name = col Users.name in
          (~id, ~name))
    |> all db
  in
  List.iter (fun (~id, ~name) -> Printf.printf "  %d %s\n" id name) names;

  print_endline "\n-- read: some columns, as a list";
  let* rows =
    from Users.table
    |> select [ Users.id; Users.name; Users.age ]
    |> order_by [ desc Users.age ]
    |> all db
  in
  List.iter
    (fun Row.[ id; name; age ] -> Printf.printf "  %d %s %d\n" id name age)
    rows;

  print_endline "\n-- upsert (ON CONFLICT)";
  let* skipped =
    upsert db
      (On_conflict.do_nothing ())
      (values Users.table
         Ops.[ Users.id := 1; Users.name := "Ann (duplicate)"; Users.age := 99 ])
  in
  Printf.printf "  do_nothing on existing id=1 -> %s\n"
    (match skipped with None -> "skipped" | Some _ -> "inserted");
  let* updated =
    upsert db
      On_conflict.(
        do_update
          ~target:[ col Users.id ]
          ~excluded:[ col Users.name ]
          ~set:Ops.[ Users.age := 35 ]
          ())
      (values Users.table
         Ops.[ Users.id := 1; Users.name := "Ann Smith"; Users.age := 35 ])
  in
  Option.iter show updated;

  print_endline "\n-- distinct + nulls_last";
  let* emails =
    from Users.table |> select [ Users.email ] |> distinct
    |> order_by [ nulls_last (asc Users.email) ]
    |> all db
  in
  List.iter
    (fun Row.[ email ] ->
      Printf.printf "  email: %s\n" (Option.value email ~default:"<none>"))
    emails;

  print_endline "\n-- changeset validation";
  let cs =
    Changeset.(
      for_insert Users.table
      |> cast
           [ ("name", "A"); ("age", "200") ]
           [ field Users.name; field Users.age ]
      |> validate_required [ field Users.name; field Users.age ]
      |> validate Users.name [ length ~min:2 () ]
      |> validate Users.age [ range ~ge:0 ~lt:150 () ])
  in
  List.iter
    (fun (e : Changeset.error) -> Printf.printf "  %s: %s\n" e.field e.message)
    (Changeset.errors cs);

  print_endline "\n-- update";
  let* () =
    from Users.table
    |> where Ops.(Users.id = 2)
    |> update db Ops.[ Users.age := 52; Users.email := Some "bob@example.com" ]
  in

  print_endline "\n-- delete";
  let* () = from Users.table |> where Ops.(Users.age < 18) |> delete db in

  print_endline "\n-- count, then everything";
  let* n = from Users.table |> count db in
  Printf.printf "  %d users\n" n;
  let* everyone = from Users.table |> order_by [ asc Users.id ] |> all db in
  List.iter show everyone;
  Ok ()

let () =
  match demo () with
  | Ok () -> ()
  | Error e ->
      prerr_endline (Caqti_error.show e);
      exit 1
