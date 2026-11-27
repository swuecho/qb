(* Multi-table Query builder, aggregates, recursive CTEs, Raw fragments, and
   typed Rel eager loading on an in-memory SQLite database.
   Run with: dune exec ./example/multi.exe *)

open Qb
open Qb.Blocking

module Users = struct
  type t = { id : int; name : string; age : int }

  let id = column "id" int (fun u -> u.id) ~primary_key:true
  let name = column "name" text (fun u -> u.name)
  let age = column "age" int (fun u -> u.age)

  let table =
    table "users"
      (let+ id = col id and+ name = col name and+ age = col age in
       { id; name; age })
end

module Comments = struct
  type t = { id : int; author_id : int; body : string; reply_to : int option }

  let id = column "id" int (fun c -> c.id) ~primary_key:true
  let author_id = column "author_id" int (fun c -> c.author_id)
  let body = column "body" text (fun c -> c.body)
  let reply_to = column "reply_to" (nullable int) (fun c -> c.reply_to)

  let table =
    table "comments"
      (let+ id = col id
       and+ author_id = col author_id
       and+ body = col body
       and+ reply_to = col reply_to in
       { id; author_id; body; reply_to })
end

let user_comments =
  Rel.has_many ~parent_key:Users.id ~child:Comments.table
    ~child_key:Comments.author_id

let ( let* ) r f = match r with Ok x -> f x | Error _ as e -> e

let demo () =
  let* db = Caqti_blocking.connect (Uri.of_string "sqlite3::memory:") in
  let* () = create_table db Users.table in
  let* () = create_table db Comments.table in
  let* () =
    insert_many db Users.table
      [
        { id = 1; name = "Ann"; age = 34 };
        { id = 2; name = "Bob"; age = 51 };
        { id = 3; name = "Cy"; age = 17 };
      ]
  in
  let* () =
    insert_many db Comments.table
      [
        {
          id = 10;
          author_id = 1;
          body = "RFC: new query builder";
          reply_to = None;
        };
        { id = 11; author_id = 2; body = "Looks great!"; reply_to = Some 10 };
        {
          id = 12;
          author_id = 1;
          body = "Thanks, shipping it.";
          reply_to = Some 11;
        };
      ]
  in

  (* 1. Multi-table join + GROUP BY + HAVING *)
  let stats =
    Query.(
      build
        (let* u = from Users.table in
         let* c = left_join_many u user_comments in
         let* () =
           group_by [ Expr.any u.@(Users.id); Expr.any u.@(Users.name) ]
         in
         let* () =
           order_by [ desc (Expr.count c.@?(Comments.id)); asc u.@(Users.name) ]
         in
         select
           (let+ name = expr u.@(Users.name)
            and+ total = expr (Expr.count c.@?(Comments.id)) in
            (name, total))))
  in
  print_endline "-- comment counts per user";
  print_endline ("  " ^ Query.to_sql stats);
  let* rows = query_all db stats in
  List.iter (fun (name, n) -> Printf.printf "  %-4s %d comments\n" name n) rows;

  (* 2. Recursive CTE walking the reply tree *)
  let thread =
    Query.(
      build
        (let* tree, (cid, depth) =
           with_recursive_cte "thread"
             ~base:
               (let* c = from Comments.table in
                let* () = where Expr.Ops.(is_null c.@(Comments.reply_to)) in
                select_named
                  Named.(
                    let+ cid = field "id" c.@(Comments.id)
                    and+ depth = field "depth" (Expr.int 0) in
                    (cid, depth)))
             ~step:(fun self (cid, depth) ->
               let* p = from_cte self in
               let* c =
                 join Comments.table ~on:(fun c ->
                     Expr.Ops.(c.@(Comments.reply_to) =? p.@(cid)))
               in
               select_named
                 Named.(
                   let+ cid = field "id" c.@(Comments.id)
                   and+ depth =
                     field "depth" Expr.Ops.(p.@(depth) + Expr.int 1)
                   in
                   (cid, depth)))
         in
         let* t = from_cte tree in
         let* c =
           join Comments.table ~on:(fun c ->
               Expr.Ops.(c.@(Comments.id) = t.@(cid)))
         in
         let* u =
           join Users.table ~on:(fun u ->
               Expr.Ops.(u.@(Users.id) = c.@(Comments.author_id)))
         in
         let* () = order_by [ asc t.@(depth); asc c.@(Comments.id) ] in
         select
           (let+ depth = expr t.@(depth)
            and+ author = expr u.@(Users.name)
            and+ body = expr c.@(Comments.body) in
            (depth, author, body))))
  in
  print_endline "\n-- recursive CTE comment thread";
  let* rows = query_all db thread in
  List.iter
    (fun (depth, author, body) ->
      Printf.printf "  %s[%s] %s\n" (String.make (depth * 2) ' ') author body)
    rows;

  (* 3. Batched eager loading (2 queries total, zero N+1) *)
  print_endline "\n-- batched load_many";
  let* users = from Users.table |> order_by [ asc Users.id ] |> all db in
  let* users_with_comments =
    load_many db user_comments users ~order_by:[ asc Comments.id ]
  in
  List.iter
    (fun ((u : Users.t), cs) ->
      Printf.printf "  %-4s -> [%s]\n" u.name
        (String.concat "; " (List.map (fun (c : Comments.t) -> c.body) cs)))
    users_with_comments;
  Ok ()

let () =
  match demo () with
  | Ok () -> ()
  | Error e ->
      prerr_endline (Caqti_error.show e);
      exit 1
