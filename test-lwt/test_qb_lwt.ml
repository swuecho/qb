(* The same queries through Qb_lwt, on SQLite and, when QB_TEST_PG_URL is
   set, on Postgres. *)

open Qb

module Notes = struct
  type t = { id : int; body : string }

  let id =
    column "id" int (fun n -> n.id) ~primary_key:true ~auto_increment:true

  let body = column "body" text (fun n -> n.body)

  let table =
    table "notes"
      (let+ id = col id and+ body = col body in
       { id; body })
end

let ( let* ) = Lwt_result.bind

let run url =
  let* db = Caqti_lwt_unix.connect (Uri.of_string url) in
  let (module Db : Caqti_lwt.CONNECTION) = db in
  let* () =
    Db.exec
      (Caqti_request.Infix.(Caqti_type.unit ->. Caqti_type.unit)
         {|DROP TABLE IF EXISTS "notes"|})
      ()
  in
  let* () = Qb_lwt.create_table db Notes.table in
  let* id =
    Qb_lwt.insert_returning db Notes.table
      Ops.[ Notes.body := "second" ]
      (col Notes.id)
  in
  let* () = Qb_lwt.insert db Notes.table { id = 100; body = "first" } in
  let* () =
    from Notes.table
    |> where Ops.(Notes.id = 100)
    |> Qb_lwt.update db Ops.[ Notes.body := "edited" ]
  in
  let* bodies =
    from Notes.table
    |> select (col Notes.body)
    |> order_by [ asc Notes.id ]
    |> Qb_lwt.all db
  in
  let* () = from Notes.table |> where all_rows |> Qb_lwt.delete db in
  let* left = from Notes.table |> Qb_lwt.count db in
  (* Nested transactions use SAVEPOINT: an inner Error rolls back only the
     inner savepoint while the outer transaction commits. *)
  let* () =
    Qb_lwt.transaction db (fun () ->
        let* () = Qb_lwt.insert db Notes.table { id = 7; body = "kept" } in
        let* inner =
          Qb_lwt.transaction db (fun () ->
              let* () =
                Qb_lwt.insert db Notes.table { id = 8; body = "rolled_back" }
              in
              Lwt.return_error `Abort)
          |> Lwt.map (function
            | Error `Abort -> Ok ()
            | Ok () -> Ok ()
            | Error (#Caqti_error.t as e) -> Error e)
        in
        let () = inner in
        Lwt.return_ok ())
  in
  let* kept = from Notes.table |> Qb_lwt.count db in
  (* Multi-table Qb.Query works through Qb_lwt too. *)
  let* queried =
    Qb.Query.(
      build
        (let* n = from Notes.table in
         let* () = where Expr.Ops.(n.@(Notes.id) = Expr.int 7) in
         select (expr n.@(Notes.body))))
    |> Qb_lwt.query_all db
  in
  let* () = Db.disconnect () |> Lwt_result.ok in
  Lwt.return_ok (id, bodies, left, kept, queried)

let test url () =
  match Lwt_main.run (run url) with
  | Error e -> Alcotest.fail (Caqti_error.show e)
  | Ok (id, bodies, left, kept, queried) ->
      Alcotest.(check int)
        "nested savepoint rolled back inner, kept outer" 1 kept;
      Alcotest.(check (list string)) "Qb_lwt.query_all" [ "kept" ] queried;
      Alcotest.(check int) "returned id" 1 id;
      Alcotest.(check (list string)) "bodies" [ "second"; "edited" ] bodies;
      Alcotest.(check int) "deleted all" 0 left

let () =
  Alcotest.run "qb-lwt"
    [
      ("sqlite", [ Alcotest.test_case "crud" `Quick (test "sqlite3::memory:") ]);
      ( "postgres",
        [
          (match Sys.getenv_opt "QB_TEST_PG_URL" with
          | Some url -> Alcotest.test_case "crud" `Quick (test url)
          | None ->
              Alcotest.test_case "set QB_TEST_PG_URL to run" `Quick (fun () ->
                  Alcotest.skip ()));
        ] );
    ]
