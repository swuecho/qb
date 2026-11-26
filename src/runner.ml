(* Running queries, changesets, transactions, and raw SQL statements on Caqti. *)

open Ty
open Table
open Query
open Stmt

module type BACKEND = sig
  type +'a fiber
  type (+'a, +'e) stream

  module type CONNECTION =
    Caqti_connection_sig.S
      with type 'a fiber := 'a fiber
       and type ('a, 'e) stream := ('a, 'e) stream

  val return : 'a -> 'a fiber
  val map : ('a -> 'b) -> 'a fiber -> 'b fiber
end

module type S = sig
  type 'a fiber
  type db

  val all :
    db ->
    ('r, 'a, _) query ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val first :
    db ->
    ('r, 'a, _) query ->
    ('a option, [> Caqti_error.call_or_retrieve ]) result fiber

  val count :
    db ->
    ('r, _, _) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert :
    db ->
    'r table ->
    'r ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_returning :
    db ->
    'r table ->
    'r assign list ->
    ('r, 'a) sel ->
    ('a, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_values :
    db -> 'r values -> ('r, [> Caqti_error.call_or_retrieve ]) result fiber

  val update :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete :
    db ->
    ('r, _, filtered * plain) query ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_many :
    db ->
    'r table ->
    'r list ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_count :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_count :
    db ->
    ('r, _, filtered * plain) query ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_returning :
    db ->
    'r assign list ->
    ('r, _, filtered * plain) query ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_returning :
    db ->
    ('r, _, filtered * plain) query ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val transaction :
    db ->
    (unit -> ('a, 'e) result fiber) ->
    ('a, ([> Caqti_error.transact ] as 'e)) result fiber

  val create_table :
    db -> 'r table -> (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_changeset :
    db ->
    'r Changeset.t ->
    ('r, [> Caqti_error.call_or_retrieve | `Invalid of 'r Changeset.t ]) result
    fiber

  val update_changeset :
    db ->
    'r Changeset.t ->
    ( 'r,
      [> Caqti_error.call_or_retrieve | `Invalid of 'r Changeset.t | `Stale ]
    )
    result
    fiber

  val fetch_all :
    db -> 'a stmt -> ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val fetch_opt :
    db -> 'a stmt -> ('a option, [> Caqti_error.call_or_retrieve ]) result fiber

  val fetch_one :
    db -> 'a stmt -> ('a, [> Caqti_error.call_or_retrieve ]) result fiber

  val execute :
    db -> unit stmt -> (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val execute_count :
    db -> unit stmt -> (int, [> Caqti_error.call_or_retrieve ]) result fiber
end

module type GUARDED_BACKEND = sig
  include BACKEND

  val finally : (unit -> 'a fiber) -> (unit -> unit) -> 'a fiber
end

(* The runner; [G.finally] is there for a guarded backend. *)
module Make_with
    (B : BACKEND)
    (G : sig
      type finally = {
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      val finally : finally option
    end) =
struct
  type 'a fiber = 'a B.fiber
  type db = (module B.CONNECTION)

  let out_for (module Db : B.CONNECTION) =
    out
      (match Caqti_driver_info.dialect_tag Db.driver_info with
      | `Sqlite -> Sqlite
      | `Mysql -> Mysql
      | _ -> Postgres)

  (* One debug line for each statement, once it has run: its name (if any),
     its text, its parameters, what came of it and how long it took. Nothing is
     timed or formatted unless the source is at debug level. *)
  let logged ?name sql params describe run =
    if Logs.Src.level Cache.src <> Some Logs.Debug then run ()
    else
      let clock = Mtime_clock.counter () in
      let prefix = match name with Some n -> "[" ^ n ^ "] " | None -> "" in
      run ()
      |> B.map (fun r ->
          Cache.Log.debug (fun m ->
              m "%s%s%a -> %s, %a" prefix sql Cache.pp_params params
                (match r with
                | Ok x -> describe x
                | Error (#Caqti_error.call_or_retrieve as e) ->
                    "failed: " ^ Caqti_error.show e
                | Error _ -> "failed")
                Mtime.Span.pp (Mtime_clock.count clock));
          r)

  let rows n = if n = 1 then "1 row" else string_of_int n ^ " rows"

  let exec (module Db : B.CONNECTION) o =
    match Cache.finish o with
    | sql, ps, Pack (pt, pv) ->
        logged sql ps
          (fun () -> "done")
          (fun () ->
            Db.exec
              (Cache.request ~oneshot:o.variadic Cache.zero pt Caqti_type.unit
                 sql)
              pv)

  (* Every driver qb renders for counts affected rows; one that can't says
     so as an error of the usual kind. *)
  let counted ?name (module Db : B.CONNECTION) ~oneshot sql ps pt pv =
    logged ?name sql ps
      (fun n -> rows n ^ " changed")
      (fun () ->
        Db.exec_with_affected_count
          (Cache.request ~oneshot Cache.zero pt Caqti_type.unit sql)
          pv
        |> B.map (function
          | Ok n -> Ok n
          | Error `Unsupported ->
              Error
                (Caqti_error.response_rejected ~uri:Uri.empty ~query:sql
                   (Caqti_error.Msg "this driver doesn't count affected rows"))
          | Error (#Caqti_error.call_or_retrieve as e) -> Error e))

  let exec_count db o =
    match Cache.finish o with
    | sql, ps, Pack (pt, pv) -> counted db ~oneshot:o.variadic sql ps pt pv

  let collect_list ?name (module Db : B.CONNECTION) ~oneshot sql ps pt pv rt
      decode =
    logged ?name sql ps
      (fun l -> rows (List.length l))
      (fun () ->
        Db.collect_list (Cache.request ~oneshot Cache.zero_or_more pt rt sql) pv)
    |> B.map (Result.map (List.map decode))

  let find_opt ?name (module Db : B.CONNECTION) ~oneshot sql ps pt pv rt decode
      =
    logged ?name sql ps
      (function Some _ -> "1 row" | None -> "no row")
      (fun () ->
        Db.find_opt (Cache.request ~oneshot Cache.zero_or_one pt rt sql) pv)
    |> B.map (Result.map (Option.map decode))

  let find ?name (module Db : B.CONNECTION) ~oneshot sql ps pt pv rt decode =
    logged ?name sql ps
      (fun _ -> "1 row")
      (fun () -> Db.find (Cache.request ~oneshot Cache.one pt rt sql) pv)
    |> B.map (Result.map decode)

  let collect db o returning =
    match decoder returning with
    | Dec (rt, decode, _) -> (
        render_returning o returning;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            collect_list db ~oneshot:o.variadic sql ps pt pv rt decode)

  let all db q =
    match prepare q with
    | Prepared (rt, decode, render) -> (
        let o = out_for db in
        render o;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            collect_list db ~oneshot:o.variadic sql ps pt pv rt decode)

  let first db q =
    match prepare ~limit:1 q with
    | Prepared (rt, decode, render) -> (
        let o = out_for db in
        render o;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            find_opt db ~oneshot:o.variadic sql ps pt pv rt decode)

  let count ((module Db : B.CONNECTION) as db) q =
    let o = out_for db in
    render_count o q;
    match Cache.finish o with
    | sql, ps, Pack (pt, pv) ->
        logged sql ps string_of_int (fun () ->
            Db.find
              (Cache.request ~oneshot:o.variadic Cache.one pt Caqti_type.int sql)
              pv)

  let insert db t row =
    let o = out_for db in
    render_insert o t t.cols
      (List.map (fun (Any c) () -> param o c (c.get row)) t.cols);
    exec db o

  let insert_returning db t assigns returning =
    check_nonempty "Qb.insert_returning" returning;
    match decoder returning with
    | Dec (rt, decode, cols) -> (
        let o = out_for db in
        render_insert o t
          (List.map (fun (Set (c, _)) -> Any c) assigns)
          (List.map (fun (Set (c, v)) () -> param o c v) assigns);
        str o " RETURNING ";
        col_names o cols;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            find db ~oneshot:o.variadic sql ps pt pv rt decode)

  let insert_values db v = insert_returning db v.vtable v.vassigns v.vtable.row

  let update db assigns q =
    match assigns with
    | [] -> B.return (Ok ())
    | _ ->
        let o = out_for db in
        render_update o assigns q;
        exec db o

  let delete db q =
    let o = out_for db in
    render_delete o q;
    exec db o

  let insert_many db t = function
    | [] -> B.return (Ok ())
    | rows ->
        let o = out_for db in
        render_insert_many o t rows;
        exec db o

  let update_count db assigns q =
    match assigns with
    | [] -> B.return (Ok 0)
    | _ ->
        let o = out_for db in
        render_update o assigns q;
        exec_count db o

  let delete_count db q =
    let o = out_for db in
    render_delete o q;
    exec_count db o

  let update_returning db assigns q returning =
    if assigns = [] then invalid_arg "Qb.update_returning: no assignments";
    check_nonempty "Qb.update_returning" returning;
    let o = out_for db in
    render_update o assigns q;
    collect db o returning

  let delete_returning db q returning =
    check_nonempty "Qb.delete_returning" returning;
    let o = out_for db in
    render_delete o q;
    collect db o returning

  (* A constraint the changeset declared becomes an error on it. *)
  let refused cs = function
    | Ok r -> Ok r
    | Error e -> (
        match e with
        | #Caqti_error.call_or_retrieve as ce -> (
            match Changeset.on_error cs ce with
            | Some cs -> Error (`Invalid cs)
            | None -> Error e)
        | _ -> Error e)

  let insert_changeset db (cs : _ Changeset.t) =
    if Option.is_some cs.data then
      invalid_arg
        "Qb.insert_changeset: the changeset is for_update; use update_changeset";
    if not (Changeset.is_valid cs) then B.return (Error (`Invalid cs))
    else insert_values db (values cs.table cs.changes) |> B.map (refused cs)

  let update_changeset db (cs : _ Changeset.t) =
    match cs.data with
    | None ->
        invalid_arg
          "Qb.update_changeset: the changeset is for_insert; use \
           insert_changeset"
    | Some row -> (
        let (Any pk) = Changeset.primary_key cs in
        if not (Changeset.is_valid cs) then B.return (Error (`Invalid cs))
        else
          match cs.changes with
          | [] -> B.return (Ok row)
          | changes ->
              let q = from cs.table |> where Ops.(pk = pk.get row) in
              update_returning db changes q cs.table.row
              |> B.map (function
                | Ok (r :: _) -> Ok r
                | Ok [] -> Error `Stale
                | Error _ as e -> refused cs e))

  (* The connections now inside [transaction]. Caqti doesn't nest them: a
     second BEGIN is ignored by the database and the inner COMMIT then ends
     the outer transaction early, so that is refused here. It takes
     [G.finally] to know when a transaction is over, however it ends. *)
  let in_transaction : Obj.t list ref = ref []

  let transaction ((module Db : B.CONNECTION) as db) f =
    match G.finally with
    | None -> Db.with_transaction f
    | Some { finally } ->
        let key = Obj.repr db in
        if List.memq key !in_transaction then
          invalid_arg
            "Qb.transaction: this connection is already in a transaction, and \
             transactions don't nest";
        in_transaction := key :: !in_transaction;
        finally
          (fun () -> Db.with_transaction f)
          (fun () ->
            in_transaction := List.filter (fun k -> k != key) !in_transaction)

  let create_table db t =
    let o = out_for db in
    str o (create_table_sql ~dialect:o.dialect t);
    exec db o

  let fetch_all db s =
    match (s.row, pack_args (out_for db).dialect s) with
    | Row_dec (rt, decode), (sql, ps, Pack (pt, pv)) ->
        collect_list ?name:s.name db ~oneshot:(variadic s) sql ps pt pv rt
          decode

  let fetch_opt db s =
    match (s.row, pack_args (out_for db).dialect s) with
    | Row_dec (rt, decode), (sql, ps, Pack (pt, pv)) ->
        find_opt ?name:s.name db ~oneshot:(variadic s) sql ps pt pv rt decode

  let fetch_one db s =
    match (s.row, pack_args (out_for db).dialect s) with
    | Row_dec (rt, decode), (sql, ps, Pack (pt, pv)) ->
        find ?name:s.name db ~oneshot:(variadic s) sql ps pt pv rt decode

  let execute ((module Db : B.CONNECTION) as db) s =
    match pack_args (out_for db).dialect s with
    | sql, ps, Pack (pt, pv) ->
        logged ?name:s.name sql ps
          (fun () -> "done")
          (fun () ->
            Db.exec
              (Cache.request ~oneshot:(variadic s) Cache.zero pt Caqti_type.unit
                 sql)
              pv)

  let execute_count db s =
    match pack_args (out_for db).dialect s with
    | sql, ps, Pack (pt, pv) ->
        counted ?name:s.name db ~oneshot:(variadic s) sql ps pt pv
end

module Make (B : BACKEND) =
  Make_with
    (B)
    (struct
      type finally = {
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      let finally = None
    end)

module Make_guarded (B : GUARDED_BACKEND) =
  Make_with
    (B)
    (struct
      type finally = {
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      let finally = Some { finally = B.finally }
    end)

module Blocking_backend = struct
  type 'a fiber = 'a
  type ('a, 'e) stream = ('a, 'e) Caqti_blocking.Stream.t

  module type CONNECTION = Caqti_blocking.CONNECTION

  let return x = x
  let map f x = f x

  let finally f after =
    match f () with
    | x ->
        after ();
        x
    | exception e ->
        after ();
        raise e
end

module Blocking = Make_guarded (Blocking_backend)
