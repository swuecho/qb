(* Qb_migrate: applies migrations to Postgres. See qb_migrate.mli. *)

open Qb_pg_parse

type migration = { name : string; sql : string }
type db = Caqti_blocking.connection

type error =
  [ `Changed of string
  | `Missing of string
  | `Out_of_order of string * string
  | `Invalid of string * string
  | `Failed of string * string
  | `Not_verified of string list
  | `Db of Caqti_error.t ]

let error_to_string : error -> string = function
  | `Changed n ->
      Printf.sprintf
        "%s was changed after it was applied: put it back, and make the change \
         in a new migration"
        n
  | `Missing n ->
      Printf.sprintf "%s was applied, and is no longer among the migrations" n
  | `Out_of_order (pending, applied) ->
      Printf.sprintf
        "%s is not applied, and comes before %s, which is: rename it to come \
         last, or allow migrations out of order"
        pending applied
  | `Invalid (n, m) -> Printf.sprintf "%s: %s" n m
  | `Failed (n, m) -> Printf.sprintf "%s failed: %s" n m
  | `Not_verified diffs ->
      "the database doesn't match the migrations:\n  "
      ^ String.concat "\n  " diffs
  | `Db e -> Caqti_error.show e

type status =
  | Applied of Ptime.t
  | Pending
  | Changed of Ptime.t
  | Missing of Ptime.t

let ( let* ) = Result.bind

let of_dir dir =
  match Sys.readdir dir with
  | exception Sys_error m -> Error m
  | files ->
      Array.to_list files
      |> List.filter (fun f -> Filename.check_suffix f ".sql")
      |> List.sort compare
      |> List.fold_left
           (fun acc name ->
             let* acc = acc in
             match
               In_channel.with_open_bin (Filename.concat dir name)
                 In_channel.input_all
             with
             | sql -> Ok ({ name; sql } :: acc)
             | exception Sys_error m -> Error m)
           (Ok [])
      |> Result.map List.rev

let checksum m = Sha256.hex m.sql

(* ---------- talking to the database ---------- *)

module type DB = Caqti_blocking.CONNECTION

let db_error r = Result.map_error (fun e -> `Db (e :> Caqti_error.t)) r

(* A migration's statement is sent as it is: Caqti reads nothing into its
   [?] or [$]. Statements that return rows (e.g. pg_dump's
   [SELECT pg_catalog.set_config(...)]) use [literal_rows] so Caqti doesn't
   reject the returned tuples. *)
let literal sql =
  Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit
    Caqti_mult.zero (fun _ -> Caqti_query.L sql)

let literal_rows sql =
  Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit
    Caqti_mult.zero_or_more (fun _ -> Caqti_query.L sql)

module Q = struct
  open Caqti_request.Infix
  open Caqti_type

  let create_table =
    literal
      "CREATE TABLE IF NOT EXISTS qb_migrations (name TEXT PRIMARY KEY, \
       checksum TEXT NOT NULL, applied_at TIMESTAMPTZ NOT NULL DEFAULT now())"

  let has_table =
    (unit ->! bool) "SELECT to_regclass('qb_migrations') IS NOT NULL"

  let applied =
    (unit ->* t3 string string ptime)
      "SELECT name, checksum, applied_at FROM qb_migrations ORDER BY name"

  let quote_ident name =
    "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

  let record =
    (t2 string string ->. unit)
      "INSERT INTO qb_migrations (name, checksum) VALUES (?, ?)"

  (* Qualified with the schema captured before the migration ran, so a
     migration such as a pg_dump that clears search_path can still be
     recorded. *)
  let record_in schema =
    let table = quote_ident schema ^ ".qb_migrations" in
    Caqti_request.create ~oneshot:true (t2 string string) unit Caqti_mult.zero
      (fun _ ->
        Caqti_query.S
          [
            Caqti_query.L ("INSERT INTO " ^ table ^ " (name, checksum) VALUES (");
            Caqti_query.P 0;
            Caqti_query.L ", ";
            Caqti_query.P 1;
            Caqti_query.L ")";
          ])

  (* One key for every qb-migrate run on the database. *)
  let lock = (unit ->! bool) "SELECT true FROM pg_advisory_lock(7126173401)"
  let unlock = (unit ->! bool) "SELECT pg_advisory_unlock(7126173401)"
  let current_schema = (unit ->! string) "SELECT current_schema()::text"
  let search_path = (unit ->! string) "SHOW search_path"

  let set_search_path =
    (string ->! string) "SELECT pg_catalog.set_config('search_path', ?, false)"

  (* Each column of each table: its type's name, whether it can be NULL,
     the type's length or precision where it has one, and whether the
     database fills it in (a default, an identity, a generated column). *)
  let columns =
    (unit
    ->* t2 (t2 string string)
          (t2 (t3 string string bool)
             (t2
                (t3 (option int) (option int) (option int))
                (t2 (option int) bool))))
      "SELECT c.table_schema::text, c.table_name::text, c.column_name::text, \
       c.udt_name::text, c.is_nullable = 'YES', \
       c.character_maximum_length::int, c.numeric_precision::int, \
       c.numeric_scale::int, c.datetime_precision::int, (c.column_default IS \
       NOT NULL OR c.is_identity = 'YES' OR c.is_generated = 'ALWAYS') FROM \
       information_schema.columns c JOIN information_schema.tables t ON \
       t.table_schema = c.table_schema AND t.table_name = c.table_name WHERE \
       t.table_type = 'BASE TABLE' AND c.table_schema NOT IN ('pg_catalog', \
       'information_schema') ORDER BY 1, 2, c.ordinal_position"

  (* Each primary key's columns, in the key's order. *)
  let primary_keys =
    (unit ->* t2 (t2 string string) string)
      "SELECT n.nspname::text, c.relname::text, a.attname::text FROM pg_index \
       i JOIN pg_class c ON c.oid = i.indrelid JOIN pg_namespace n ON n.oid = \
       c.relnamespace JOIN unnest(i.indkey::int2[]) WITH ORDINALITY k(attnum, \
       ord) ON true JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = \
       k.attnum WHERE i.indisprimary ORDER BY 1, 2, k.ord"
end

let applied (module Db : DB) =
  let* exists = db_error (Db.find Q.has_table ()) in
  if exists then db_error (Db.collect_list Q.applied ()) else Ok []

let with_lock (module Db : DB) f =
  let* _ = db_error (Db.find Q.lock ()) in
  let unlock () = ignore (Db.find Q.unlock ()) in
  match f () with
  | r ->
      unlock ();
      r
  | exception e ->
      unlock ();
      raise e

(* ---------- status ---------- *)

let statuses ms applied =
  List.map
    (fun m ->
      ( m.name,
        match List.find_opt (fun (n, _, _) -> n = m.name) applied with
        | None -> Pending
        | Some (_, sum, at) when sum = checksum m -> Applied at
        | Some (_, _, at) -> Changed at ))
    ms
  @ List.filter_map
      (fun (n, _, at) ->
        if List.exists (fun m -> m.name = n) ms then None
        else Some (n, Missing at))
      applied

let status db ms =
  let* applied = applied db in
  Ok (statuses ms applied)

(* ---------- run ---------- *)

let no_transaction m =
  let first =
    match String.index_opt m.sql '\n' with
    | Some i -> String.sub m.sql 0 i
    | None -> m.sql
  in
  String.trim first = "-- qb:no-transaction"

(* Statements for which Postgres replies with Tuples_ok rather than
   Command_ok. *)
let returns_rows = function
  | `Assoc [ (("SelectStmt" | "VariableShowStmt" | "ExplainStmt"), _) ] -> true
  | `Assoc [ (("InsertStmt" | "UpdateStmt" | "DeleteStmt"), `Assoc kv) ] ->
      List.mem_assoc "returningClause" kv || List.mem_assoc "returningList" kv
  | _ -> false

(* A migration's statements, paired with whether each returns rows. One that
   runs in a transaction can't have its own: its COMMIT would end the runner's,
   and what follows, with the record that it was applied, would no longer be
   undone together. *)
let split m =
  let sql = Pg.strip_psql_meta m.sql in
  let parsed r =
    Result.map_error (fun e -> `Invalid (m.name, Pg.error_to_string ~sql e)) r
  in
  let field k = function `Assoc kv -> List.assoc_opt k kv | _ -> None in
  let* tree = parsed (Pg.parse sql) in
  let raw_stmts =
    match field "stmts" tree with Some (`List stmts) -> stmts | _ -> []
  in
  let own_transaction =
    List.exists
      (fun s ->
        match field "stmt" s with
        | Some (`Assoc [ ("TransactionStmt", _) ]) -> true
        | _ -> false)
      raw_stmts
  in
  if own_transaction && not (no_transaction m) then
    Error
      (`Invalid
         ( m.name,
           "it has its own BEGIN or COMMIT, and a migration already runs in a \
            transaction: take them out, or make -- qb:no-transaction its first \
            line" ))
  else
    let stmts =
      List.filter_map
        (fun s ->
          let int k = match field k s with Some (`Int i) -> i | _ -> 0 in
          let at = int "stmt_location" in
          let len =
            match int "stmt_len" with 0 -> String.length sql - at | n -> n
          in
          match String.trim (String.sub sql at len) with
          | "" -> None
          | text ->
              let has_rows =
                match field "stmt" s with
                | Some node -> returns_rows node
                | None -> false
              in
              Some (text, has_rows))
        raw_stmts
    in
    Ok stmts

(* What stops a run before anything is applied. *)
let check ~out_of_order ms applied =
  let st = statuses ms applied in
  (* [pending]: the first migration not yet applied, once one is seen. *)
  let rec go pending = function
    | [] -> Ok ()
    | (n, Changed _) :: _ -> Error (`Changed n)
    | (n, Missing _) :: _ -> Error (`Missing n)
    | (n, Pending) :: rest ->
        go (match pending with None -> Some n | p -> p) rest
    | (n, Applied _) :: rest -> (
        match pending with
        | Some p when not out_of_order -> Error (`Out_of_order (p, n))
        | _ -> go pending rest)
  in
  let* () = go None st in
  Ok (List.filter_map (fun (n, s) -> if s = Pending then Some n else None) st)

let first_line s =
  match String.index_opt s '\n' with
  | Some i -> String.sub s 0 i ^ " ..."
  | None -> s

let apply ~schema ~saved_path (module Db : DB) m stmts =
  let failed stmt e ~kept =
    `Failed
      ( m.name,
        Printf.sprintf "%s\n  in: %s%s" (Caqti_error.show e) (first_line stmt)
          (if kept then
             "\n\
             \  it runs without a transaction: what came before this statement \
              stays"
           else "") )
  in
  let rec exec ~kept = function
    | [] -> Ok ()
    | (stmt, has_rows) :: rest -> (
        let r =
          if has_rows then Db.call ~f:(fun _ -> Ok ()) (literal_rows stmt) ()
          else Db.exec (literal stmt) ()
        in
        match r with
        | Ok () -> exec ~kept rest
        | Error e -> Error (failed stmt e ~kept))
  in
  let record () =
    db_error (Db.exec (Q.record_in schema) (m.name, checksum m))
  in
  let restore_path () = ignore (Db.find Q.set_search_path saved_path) in
  if no_transaction m then
    Fun.protect ~finally:restore_path (fun () ->
        let* () = exec ~kept:true stmts in
        record ())
  else
    let* () = db_error (Db.start ()) in
    match
      let* () = exec ~kept:false stmts in
      record ()
    with
    | Ok () ->
        let r = db_error (Db.commit ()) in
        restore_path ();
        r
    | Error _ as e ->
        ignore (Db.rollback ());
        restore_path ();
        e
    | exception e ->
        ignore (Db.rollback ());
        restore_path ();
        raise e

let parse_pending ~out_of_order ms applied =
  let* pending = check ~out_of_order ms applied in
  let pending = List.filter (fun m -> List.mem m.name pending) ms in
  (* Every pending migration must parse before the first is applied. *)
  let* parsed =
    List.fold_left
      (fun acc m ->
        let* acc = acc in
        let* stmts = split m in
        Ok ((m, stmts) :: acc))
      (Ok []) pending
  in
  Ok (List.rev parsed)

let plan ?(out_of_order = false) db ms =
  let* applied = applied db in
  let* parsed = parse_pending ~out_of_order ms applied in
  Ok (List.map (fun (m, _) -> m.name) parsed)

let run ?(out_of_order = false) ((module Db : DB) as db) ms =
  with_lock db @@ fun () ->
  let* schema = db_error (Db.find Q.current_schema ()) in
  let* saved_path = db_error (Db.find Q.search_path ()) in
  let* () = db_error (Db.exec Q.create_table ()) in
  let* applied = db_error (Db.collect_list Q.applied ()) in
  let* parsed = parse_pending ~out_of_order ms applied in
  List.fold_left
    (fun acc (m, stmts) ->
      let* acc = acc in
      let* () = apply ~schema ~saved_path db m stmts in
      Ok (m.name :: acc))
    (Ok []) parsed
  |> Result.map List.rev

(* ---------- verify ---------- *)

(* The schema the migrations describe, as [%sql] reads it. *)
let schema ~default_schema ms =
  List.fold_left
    (fun st m ->
      let* st = st in
      let sql = Pg.strip_psql_meta m.sql in
      Result.map_error
        (fun e -> `Invalid (m.name, Pg.error_to_string ~sql e))
        (Schema.apply_sql ~default_schema ~init:st sql))
    (Ok Schema.empty) ms

(* information_schema's name for a column type: the internal name, with a
   leading underscore for an array. A user type is compared without its
   schema. *)
let udt_name (ty : Schema.pg_type) =
  let base =
    match String.rindex_opt ty.name '.' with
    | Some i -> String.sub ty.name (i + 1) (String.length ty.name - i - 1)
    | None -> ty.name
  in
  if ty.array_dims > 0 then "_" ^ base else base

(* A column as the database has it. *)
type live = {
  column : string;
  udt : string;
  nullable : bool;
  length : int option;
  precision : int option;
  scale : int option;
  time_precision : int option;
  filled : bool; (* a default, an identity or a generated column *)
}

(* The length or precision a migration gives a type, against the database's:
   the database's, written as the migration would, when they differ. A type
   written without one is compared only where that has one meaning. *)
let mods_differ (ty : Schema.pg_type) (l : live) =
  let some = Option.map string_of_int in
  let written, live =
    if ty.array_dims > 0 then (None, None)
    else
      match (ty.name, ty.mods) with
      | "varchar", [] -> (None, some l.length)
      | "bpchar", [] -> (Some "1", some l.length)
      | ("varchar" | "bpchar"), [ n ] -> (Some (string_of_int n), some l.length)
      | "numeric", [] ->
          ( None,
            Option.map
              (fun p ->
                Printf.sprintf "%d,%d" p (Option.value l.scale ~default:0))
              l.precision )
      | "numeric", [ p ] ->
          ( Some (Printf.sprintf "%d,0" p),
            Option.map
              (fun p ->
                Printf.sprintf "%d,%d" p (Option.value l.scale ~default:0))
              l.precision )
      | "numeric", [ p; sc ] ->
          ( Some (Printf.sprintf "%d,%d" p sc),
            Option.map
              (fun p ->
                Printf.sprintf "%d,%d" p (Option.value l.scale ~default:0))
              l.precision )
      | ("timestamp" | "timestamptz" | "time" | "timetz"), [ p ] ->
          (Some (string_of_int p), some l.time_precision)
      | _ -> (None, None)
  in
  if written = live then None
  else Some (match live with Some m -> "(" ^ m ^ ")" | None -> "")

let differences ~current (st : Schema.t) live keys =
  let schema_of (t : Schema.table) = Option.value t.schema ~default:current in
  let label schema table =
    if schema = current then table else schema ^ "." ^ table
  in
  (* Only the schemas the migrations put tables in are theirs to describe. *)
  let schemas =
    List.sort_uniq compare (current :: List.map schema_of st.tables)
  in
  let live =
    List.filter
      (fun ((s, t), _) -> List.mem s schemas && t <> "qb_migrations")
      live
  in
  let live_tables = List.sort_uniq compare (List.map fst live) in
  let wanted =
    List.map (fun (t : Schema.table) -> (schema_of t, t.name)) st.tables
  in
  let column name (c : Schema.column) (l : live) =
    let at = name ^ "." ^ c.name in
    let ty =
      match (l.udt <> udt_name c.ty, mods_differ c.ty l) with
      | false, None -> []
      | true, _ ->
          [
            Printf.sprintf "%s: %s in the migrations, %s in the database" at
              (Schema.type_sql c.ty) l.udt;
          ]
      | false, Some mods ->
          [
            Printf.sprintf "%s: %s in the migrations, %s%s in the database" at
              (Schema.type_sql c.ty) l.udt mods;
          ]
    in
    let null =
      if l.nullable = c.not_null then
        [
          (if c.not_null then
             at ^ ": NOT NULL in the migrations, nullable in the database"
           else at ^ ": nullable in the migrations, NOT NULL in the database");
        ]
      else []
    in
    (* Which default isn't compared, only whether there is one: an INSERT
       that leaves the column out is typed on that. *)
    let fills = c.has_default || c.generated in
    let default =
      if fills = l.filled then []
      else
        [
          (if fills then
             at ^ ": a default in the migrations, none in the database"
           else at ^ ": no default in the migrations, one in the database");
        ]
    in
    ty @ null @ default
  in
  let key cols =
    match cols with [] -> "none" | cs -> "(" ^ String.concat ", " cs ^ ")"
  in
  let tables =
    List.concat_map
      (fun (t : Schema.table) ->
        let k = (schema_of t, t.name) in
        let name = label (fst k) (snd k) in
        if not (List.mem k live_tables) then
          [ Printf.sprintf "table %s: not in the database" name ]
        else
          let cols =
            List.filter_map
              (fun (k', c) -> if k' = k then Some c else None)
              live
          in
          let live_key =
            List.filter_map
              (fun (k', c) -> if k' = k then Some c else None)
              keys
          in
          List.concat_map
            (fun (c : Schema.column) ->
              match List.find_opt (fun l -> l.column = c.name) cols with
              | None ->
                  [ Printf.sprintf "%s.%s: not in the database" name c.name ]
              | Some l -> column name c l)
            t.columns
          @ List.filter_map
              (fun l ->
                if
                  List.exists
                    (fun (c : Schema.column) -> c.name = l.column)
                    t.columns
                then None
                else
                  Some
                    (Printf.sprintf "%s.%s: not in the migrations" name l.column))
              cols
          @
          if live_key = t.primary_key then []
          else
            [
              Printf.sprintf
                "%s: primary key %s in the migrations, %s in the database" name
                (key t.primary_key) (key live_key);
            ])
      st.tables
  in
  tables
  @ List.filter_map
      (fun ((s, t) as key) ->
        if List.mem key wanted then None
        else Some (Printf.sprintf "table %s: not in the migrations" (label s t)))
      live_tables

let verify (module Db : DB) ms =
  let* current = db_error (Db.find Q.current_schema ()) in
  let* st = schema ~default_schema:current ms in
  let* live = db_error (Db.collect_list Q.columns ()) in
  let* keys = db_error (Db.collect_list Q.primary_keys ()) in
  let live =
    List.map
      (fun ( table,
             ( (column, udt, nullable),
               ((length, precision, scale), (time_precision, filled)) ) ) ->
        ( table,
          {
            column;
            udt;
            nullable;
            length;
            precision;
            scale;
            time_precision;
            filled;
          } ))
      live
  in
  Ok (differences ~current st live keys)

(* ---------- baseline ---------- *)

let baseline ((module Db : DB) as db) ms =
  with_lock db @@ fun () ->
  let* diffs = verify db ms in
  if diffs <> [] then Error (`Not_verified diffs)
  else
    let* () = db_error (Db.exec Q.create_table ()) in
    let* applied = db_error (Db.collect_list Q.applied ()) in
    let* pending = check ~out_of_order:true ms applied in
    let to_record = List.filter (fun m -> List.mem m.name pending) ms in
    if to_record = [] then Ok []
    else
      let* () = db_error (Db.start ()) in
      match
        List.fold_left
          (fun acc m ->
            let* acc = acc in
            let* () = db_error (Db.exec Q.record (m.name, checksum m)) in
            Ok (m.name :: acc))
          (Ok []) to_record
      with
      | Ok recorded ->
          let* () = db_error (Db.commit ()) in
          Ok (List.rev recorded)
      | Error _ as e ->
          ignore (Db.rollback ());
          e
      | exception e ->
          ignore (Db.rollback ());
          raise e

(* ---------- embed ---------- *)

let embed ms =
  "(* Generated by qb-migrate embed. Do not edit. *)\n\n\
   let migrations : Qb_migrate.migration list =\n\
  \  [\n"
  ^ String.concat ""
      (List.map
         (fun m ->
           Printf.sprintf "    { name = %S; sql = %s };\n" m.name
             (Codegen.quoted m.sql))
         ms)
  ^ "  ]\n"
