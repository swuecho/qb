(* qb-migrate new NAME      The next numbered, empty migration
   qb-migrate up            Applies what is pending, then verifies
   qb-migrate status        Each migration: applied, pending, changed, missing
   qb-migrate verify        Compares the database with the migrations
   qb-migrate baseline      Records every migration as applied, running none
   qb-migrate embed -o F    The migrations as an OCaml list *)

open Cmdliner

let ( let* ) = Result.bind

let connect url =
  Result.map_error
    (fun e -> "can't connect to the database: " ^ Caqti_error.show e)
    (Caqti_blocking.connect (Uri.of_string url))

let migrations dir =
  Result.map_error
    (fun m -> "can't read the migrations: " ^ m)
    (Qb_migrate.of_dir dir)

let with_db url dir f =
  let* url =
    Option.to_result url ~none:"no database: give --db URL, or set DATABASE_URL"
  in
  let* ms = migrations dir in
  let* db = connect url in
  let (module Db : Caqti_blocking.CONNECTION) = db in
  Fun.protect ~finally:Db.disconnect (fun () ->
      Result.map_error Qb_migrate.error_to_string (f db ms))

let finish = function
  | Ok code -> code
  | Error m ->
      prerr_endline ("qb-migrate: " ^ m);
      1

let report_differences = function
  | [] ->
      print_endline "the database matches the migrations";
      0
  | diffs ->
      prerr_endline "the database doesn't match the migrations:";
      List.iter (fun d -> prerr_endline ("  " ^ d)) diffs;
      1

let up url dir out_of_order no_verify =
  finish
    (with_db url dir (fun db ms ->
         let* applied = Qb_migrate.run ~out_of_order db ms in
         (match applied with
         | [] -> print_endline "nothing to apply"
         | names -> List.iter (Printf.printf "applied %s\n") names);
         if no_verify then Ok 0
         else
           let* diffs = Qb_migrate.verify db ms in
           Ok (report_differences diffs)))

let status url dir =
  finish
    (with_db url dir (fun db ms ->
         let* st = Qb_migrate.status db ms in
         let time t = Ptime.to_rfc3339 ~tz_offset_s:0 t in
         List.iter
           (fun (name, (s : Qb_migrate.status)) ->
             Printf.printf "%-9s %s%s\n"
               (match s with
               | Applied _ -> "applied"
               | Pending -> "pending"
               | Changed _ -> "CHANGED"
               | Missing _ -> "MISSING")
               name
               (match s with
               | Applied t | Changed t | Missing t -> "  " ^ time t
               | Pending -> ""))
           st;
         (* Changed and missing migrations stop [up]: say so in the code. *)
         Ok
           (if
              List.exists
                (fun (_, (s : Qb_migrate.status)) ->
                  match s with Changed _ | Missing _ -> true | _ -> false)
                st
            then 1
            else 0)))

let verify url dir =
  finish
    (with_db url dir (fun db ms ->
         let* diffs = Qb_migrate.verify db ms in
         Ok (report_differences diffs)))

let baseline url dir =
  finish
    (with_db url dir (fun db ms ->
         let* names = Qb_migrate.baseline db ms in
         (match names with
         | [] -> print_endline "nothing to record"
         | names -> List.iter (Printf.printf "recorded %s\n") names);
         Ok 0))

(* 001_init.sql, 002_reviews.sql, ...: one more than the highest number, as
   wide as the numbers already there. *)
let new_ dir name =
  finish
    (let* () =
       if
         name <> ""
         && String.for_all
              (function
                | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
                | _ -> false)
              name
       then Ok ()
       else Error "a migration's name is letters, digits, _ and -"
     in
     let* () =
       if Sys.file_exists dir then Ok ()
       else
         match Sys.mkdir dir 0o755 with
         | () -> Ok ()
         | exception Sys_error m -> Error ("can't create " ^ dir ^ ": " ^ m)
     in
     let* ms = migrations dir in
     let prefix (m : Qb_migrate.migration) =
       let n = String.length m.name in
       let rec stop i =
         if i < n && m.name.[i] >= '0' && m.name.[i] <= '9' then stop (i + 1)
         else i
       in
       String.sub m.name 0 (stop 0)
     in
     let prefixes = List.filter (( <> ) "") (List.map prefix ms) in
     let next =
       1
       + List.fold_left
           (fun a p ->
             match int_of_string_opt p with Some n -> max a n | None -> a)
           0 prefixes
     in
     let width =
       List.fold_left (fun a p -> max a (String.length p)) 3 prefixes
     in
     let file =
       Filename.concat dir (Printf.sprintf "%0*d_%s.sql" width next name)
     in
     match
       Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_excl ]
         0o644 file (fun oc -> Printf.fprintf oc "-- %s\n" name)
     with
     | () ->
         print_endline file;
         Ok 0
     | exception Sys_error m -> Error m)

let embed dir output =
  finish
    (let* ms = migrations dir in
     let src = Qb_migrate.embed ms in
     (match output with
     | None -> print_string src
     | Some f -> Out_channel.with_open_bin f (fun oc -> output_string oc src));
     Ok 0)

let db =
  Arg.(
    value
    & opt (some string) None
    & info [ "db" ] ~docv:"URL"
        ~env:(Cmd.Env.info "DATABASE_URL")
        ~doc:"The database, e.g. postgresql://user:pass@host/name.")

let dir =
  Arg.(
    value & opt string "migrations"
    & info [ "m"; "migrations" ] ~docv:"DIR"
        ~doc:"The folder of *.sql migrations, applied in name order.")

let up_cmd =
  let out_of_order =
    Arg.(
      value & flag
      & info [ "out-of-order" ]
          ~doc:
            "Apply a pending migration even when one after it is already \
             applied.")
  in
  let no_verify =
    Arg.(
      value & flag
      & info [ "no-verify" ]
          ~doc:"Don't compare the database with the migrations afterwards.")
  in
  Cmd.v
    (Cmd.info "up"
       ~doc:
         "Apply the pending migrations, each in a transaction, then check that \
          the database matches.")
    Term.(const up $ db $ dir $ out_of_order $ no_verify)

let status_cmd =
  Cmd.v
    (Cmd.info "status"
       ~doc:"List each migration: applied, pending, changed or missing.")
    Term.(const status $ db $ dir)

let verify_cmd =
  Cmd.v
    (Cmd.info "verify"
       ~doc:
         "Compare the database's tables and columns with the schema the \
          migrations describe, the one [%sql] is typed from.")
    Term.(const verify $ db $ dir)

let baseline_cmd =
  Cmd.v
    (Cmd.info "baseline"
       ~doc:
         "Record every migration as applied without running it, for a database \
          that already has the schema. Refuses unless verify passes.")
    Term.(const baseline $ db $ dir)

let new_cmd =
  let name =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"NAME" ~doc:"What the migration does, e.g. add_reviews.")
  in
  Cmd.v
    (Cmd.info "new" ~doc:"Create the next numbered, empty migration.")
    Term.(const new_ $ dir $ name)

let embed_cmd =
  let output =
    Arg.(
      value
      & opt (some string) None
      & info [ "o"; "output" ] ~docv:"FILE" ~doc:"Write to FILE, not stdout.")
  in
  Cmd.v
    (Cmd.info "embed"
       ~doc:
         "Write the migrations as an OCaml list, to build them into a program.")
    Term.(const embed $ dir $ output)

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "qb-migrate"
             ~doc:"Apply the migrations qb is typed from to Postgres.")
          [ new_cmd; up_cmd; status_cmd; verify_cmd; baseline_cmd; embed_cmd ]))
