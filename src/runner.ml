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

  val insert_many_values :
    db ->
    'r values list ->
    ('r list, [> Caqti_error.call_or_retrieve ]) result fiber

  val upsert :
    db ->
    'r on_conflict ->
    'r values ->
    ('r option, [> Caqti_error.call_or_retrieve ]) result fiber

  val upsert_many :
    db ->
    'r on_conflict ->
    'r values list ->
    ('r list, [> Caqti_error.call_or_retrieve ]) result fiber

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

  val query_all :
    db ->
    'a Multi.t ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val query_first :
    db ->
    'a Multi.t ->
    ('a option, [> Caqti_error.call_or_retrieve ]) result fiber

  val query_one :
    db -> 'a Multi.t -> ('a, [> Caqti_error.call_or_retrieve ]) result fiber

  val query_count :
    db -> _ Multi.t -> (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val query_exists :
    db -> _ Multi.t -> (bool, [> Caqti_error.call_or_retrieve ]) result fiber

  val insert_select :
    db ->
    'r table ->
    ('r, 'a) sel ->
    'a Multi.t ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_from :
    db ->
    _ Multi.update_plan ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_from_count :
    db ->
    _ Multi.update_plan ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val update_from_returning :
    db ->
    'r Multi.update_plan ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_using :
    db ->
    _ Multi.delete_plan ->
    (unit, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_using_count :
    db ->
    _ Multi.delete_plan ->
    (int, [> Caqti_error.call_or_retrieve ]) result fiber

  val delete_using_returning :
    db ->
    'r Multi.delete_plan ->
    ('r, 'a) sel ->
    ('a list, [> Caqti_error.call_or_retrieve ]) result fiber

  val load_many :
    ?where:'child cond ->
    ?order_by:'child order list ->
    db ->
    ('parent, 'child) Rel.has_many ->
    'parent list ->
    (('parent * 'child list) list, [> Caqti_error.call_or_retrieve ]) result
    fiber

  val load_one :
    ?where:'parent cond ->
    db ->
    ('child, 'parent) Rel.belongs_to ->
    'child list ->
    (('child * 'parent option) list, [> Caqti_error.call_or_retrieve ]) result
    fiber

  val load_has_one :
    ?where:'child cond ->
    db ->
    ('parent, 'child) Rel.has_one ->
    'parent list ->
    (('parent * 'child option) list, [> Caqti_error.call_or_retrieve ]) result
    fiber

  val load_many_to_many :
    ?where:('target Multi.ref -> bool Expr.t) ->
    ?order_by:('target Multi.ref -> Expr.order list) ->
    db ->
    ('source, 'target) Rel.many_to_many ->
    'source list ->
    (('source * 'target list) list, [> Caqti_error.call_or_retrieve ]) result
    fiber
end

module type GUARDED_BACKEND = sig
  include BACKEND

  val bind : 'a fiber -> ('a -> 'b fiber) -> 'b fiber
  val catch : (unit -> 'a fiber) -> (exn -> 'a fiber) -> 'a fiber
  val finally : (unit -> 'a fiber) -> (unit -> unit) -> 'a fiber
end

(* The runner; [G.guard] is there for a guarded backend. *)
module Make_with
    (B : BACKEND)
    (G : sig
      type guard = {
        bind : 'a 'b. 'a B.fiber -> ('a -> 'b B.fiber) -> 'b B.fiber;
        catch : 'a. (unit -> 'a B.fiber) -> (exn -> 'a B.fiber) -> 'a B.fiber;
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      val guard : guard option
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
    let assigns = dedup_assigns assigns in
    match decoder returning with
    | Dec (rt, decode, cols) -> (
        let o = out_for db in
        render_insert o t
          (List.map assign_col assigns)
          (List.map (fun a () -> render_assign_value o a) assigns);
        str o " RETURNING ";
        col_names o cols;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            find db ~oneshot:o.variadic sql ps pt pv rt decode)

  let insert_values db v = insert_returning db v.vtable v.vassigns v.vtable.row

  let insert_many_values db = function
    | [] -> B.return (Ok [])
    | first :: _ as vs ->
        let o = out_for db in
        render_insert_many_values o vs;
        collect db o first.vtable.row

  let upsert db conflict v =
    match decoder v.vtable.row with
    | Dec (rt, decode, cols) -> (
        let o = out_for db in
        render_insert o v.vtable
          (List.map assign_col v.vassigns)
          (List.map (fun a () -> render_assign_value o a) v.vassigns);
        render_on_conflict o v.vtable conflict;
        str o " RETURNING ";
        col_names o cols;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            find_opt db ~oneshot:o.variadic sql ps pt pv rt decode)

  let upsert_many db conflict = function
    | [] -> B.return (Ok [])
    | first :: _ as vs ->
        let o = out_for db in
        render_insert_many_values o vs;
        render_on_conflict o first.vtable conflict;
        collect db o first.vtable.row

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

  (* Active transaction nesting depth per connection. The outermost call uses
     Caqti's [with_transaction]; nested calls use SQL SAVEPOINT / RELEASE /
     ROLLBACK TO so transactional helpers compose cleanly across Postgres,
     SQLite, and MySQL. *)
  let in_transaction_mutex = Mutex.create ()
  let tx_depth : (Obj.t * int) list ref = ref []

  let enter_tx key =
    Cache.with_lock in_transaction_mutex (fun () ->
        let d = Option.value (List.assq_opt key !tx_depth) ~default:0 in
        tx_depth := (key, d + 1) :: List.remove_assq key !tx_depth;
        d)

  let leave_tx key =
    Cache.with_lock in_transaction_mutex (fun () ->
        let d = Option.value (List.assq_opt key !tx_depth) ~default:0 in
        tx_depth :=
          if d <= 1 then List.remove_assq key !tx_depth
          else (key, d - 1) :: List.remove_assq key !tx_depth)

  let exec_sp (module Db : B.CONNECTION) sql =
    Db.exec
      (Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit
         Caqti_mult.zero (fun _ -> Caqti_query.L sql))
      ()

  let transaction ((module Db : B.CONNECTION) as db) f =
    match G.guard with
    | None -> Db.with_transaction f
    | Some { bind; catch; finally } ->
        let key = Obj.repr db in
        let depth = enter_tx key in
        if depth = 0 then
          finally (fun () -> Db.with_transaction f) (fun () -> leave_tx key)
        else
          let sp = Printf.sprintf "qb_sp_%d" depth in
          let rollback () =
            bind
              (exec_sp db ("ROLLBACK TO SAVEPOINT " ^ sp))
              (fun _ -> exec_sp db ("RELEASE SAVEPOINT " ^ sp))
          in
          finally
            (fun () ->
              bind
                (exec_sp db ("SAVEPOINT " ^ sp))
                (function
                  | Error _ as e -> B.return e
                  | Ok () ->
                      catch
                        (fun () ->
                          bind (f ()) (function
                            | Ok x ->
                                bind
                                  (exec_sp db ("RELEASE SAVEPOINT " ^ sp))
                                  (function
                                    | Ok () -> B.return (Ok x)
                                    | Error _ as e -> B.return e)
                            | Error _ as err ->
                                bind (rollback ()) (fun _ -> B.return err)))
                        (fun exn -> bind (rollback ()) (fun _ -> raise exn))))
            (fun () -> leave_tx key)

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

  let query_all db q =
    let name = Multi.name q in
    match Multi.prepare q with
    | Multi.Prepared (rt, decode, render) -> (
        let o = out_for db in
        render o;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            collect_list ?name db ~oneshot:o.variadic sql ps pt pv rt decode)

  let query_first db q =
    let name = Multi.name q in
    match Multi.prepare ~limit:1 q with
    | Multi.Prepared (rt, decode, render) -> (
        let o = out_for db in
        render o;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            find_opt ?name db ~oneshot:o.variadic sql ps pt pv rt decode)

  let query_one db q =
    let name = Multi.name q in
    match Multi.prepare q with
    | Multi.Prepared (rt, decode, render) -> (
        let o = out_for db in
        render o;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            find ?name db ~oneshot:o.variadic sql ps pt pv rt decode)

  let query_count ((module Db : B.CONNECTION) as db) q =
    let name = Multi.name q in
    let o = out_for db in
    Multi.render_count o q;
    match Cache.finish o with
    | sql, ps, Pack (pt, pv) ->
        logged ?name sql ps string_of_int (fun () ->
            Db.find
              (Cache.request ~oneshot:o.variadic Cache.one pt Caqti_type.int sql)
              pv)

  let query_exists ((module Db : B.CONNECTION) as db) q =
    let name = Multi.name q in
    let o = out_for db in
    Multi.render_exists o q;
    match Cache.finish o with
    | sql, ps, Pack (pt, pv) ->
        logged ?name sql ps string_of_bool (fun () ->
            Db.find
              (Cache.request ~oneshot:o.variadic Cache.one pt Caqti_type.bool
                 sql)
              pv)

  let insert_select db target target_sel q =
    let o = out_for db in
    Multi.render_insert_select o target target_sel q;
    exec db o

  let update_from db u =
    let o = out_for db in
    Multi.render_update_from o u;
    exec db o

  let update_from_count db u =
    let o = out_for db in
    Multi.render_update_from o u;
    exec_count db o

  let update_from_returning db u returning =
    match decoder returning with
    | Dec (rt, decode, _) -> (
        let o = out_for db in
        Multi.render_update_from o u;
        Multi.render_returning_sel o u.upd_id returning;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            collect_list db ~oneshot:o.variadic sql ps pt pv rt decode)

  let delete_using db d =
    let o = out_for db in
    Multi.render_delete_using o d;
    exec db o

  let delete_using_count db d =
    let o = out_for db in
    Multi.render_delete_using o d;
    exec_count db o

  let delete_using_returning db d returning =
    match decoder returning with
    | Dec (rt, decode, _) -> (
        let o = out_for db in
        Multi.render_delete_using o d;
        Multi.render_returning_sel o d.del_id returning;
        match Cache.finish o with
        | sql, ps, Pack (pt, pv) ->
            collect_list db ~oneshot:o.variadic sql ps pt pv rt decode)

  let load_many : type p c.
      ?where:c cond ->
      ?order_by:c order list ->
      db ->
      (p, c) Rel.has_many ->
      p list ->
      ((p * c list) list, [> Caqti_error.call_or_retrieve ]) result fiber =
   fun ?where:extra_where ?order_by:ord db rel parents ->
    match parents with
    | [] -> B.return (Ok [])
    | _ -> (
        let run : type k.
            (p, k) col ->
            c table ->
            (k list -> c cond) ->
            (c -> k option) ->
            ((p * c list) list, [> Caqti_error.call_or_retrieve ]) result fiber
            =
         fun parent_key child make_in get_child_key ->
          let seen = Hashtbl.create (List.length parents) in
          let keys =
            List.filter_map
              (fun p ->
                let k = parent_key.get p in
                if Hashtbl.mem seen k then None
                else (
                  Hashtbl.add seen k ();
                  Some k))
              parents
          in
          let q =
            from child |> Query.where (make_in keys) |> where_opt extra_where
            |> fun q ->
            match ord with None -> q | Some o -> Query.order_by o q
          in
          all db q
          |> B.map
               (Result.map (fun children ->
                    let grouped = Hashtbl.create (List.length keys) in
                    List.iter
                      (fun ch ->
                        match get_child_key ch with
                        | Some k ->
                            let cur =
                              Option.value
                                (Hashtbl.find_opt grouped k)
                                ~default:[]
                            in
                            Hashtbl.replace grouped k (ch :: cur)
                        | None -> ())
                      children;
                    List.map
                      (fun p ->
                        let k = parent_key.get p in
                        let cs =
                          Option.value (Hashtbl.find_opt grouped k) ~default:[]
                          |> List.rev
                        in
                        (p, cs))
                      parents))
        in
        match rel with
        | Rel.Has_many { parent_key; child; child_key } ->
            run parent_key child
              (fun keys -> Ops.is_in child_key keys)
              (fun ch -> Some (child_key.get ch))
        | Rel.Has_many_opt { parent_key; child; child_key } ->
            run parent_key child
              (fun keys -> Ops.is_in child_key (List.map Option.some keys))
              (fun ch -> child_key.get ch))

  let load_one : type c p.
      ?where:p cond ->
      db ->
      (c, p) Rel.belongs_to ->
      c list ->
      ((c * p option) list, [> Caqti_error.call_or_retrieve ]) result fiber =
   fun ?where:extra_where db rel children ->
    match children with
    | [] -> B.return (Ok [])
    | _ -> (
        let run : type k.
            (c -> k option) ->
            p table ->
            (p, k) col ->
            ((c * p option) list, [> Caqti_error.call_or_retrieve ]) result
            fiber =
         fun get_child_key parent parent_key ->
          let seen = Hashtbl.create (List.length children) in
          let keys =
            List.filter_map
              (fun ch ->
                match get_child_key ch with
                | None -> None
                | Some k ->
                    if Hashtbl.mem seen k then None
                    else (
                      Hashtbl.add seen k ();
                      Some k))
              children
          in
          match keys with
          | [] -> B.return (Ok (List.map (fun ch -> (ch, None)) children))
          | _ ->
              let q =
                from parent
                |> Query.where (Ops.is_in parent_key keys)
                |> where_opt extra_where
              in
              all db q
              |> B.map
                   (Result.map (fun parents ->
                        let by_key = Hashtbl.create (List.length parents) in
                        List.iter
                          (fun p -> Hashtbl.replace by_key (parent_key.get p) p)
                          parents;
                        List.map
                          (fun ch ->
                            let p =
                              Option.bind (get_child_key ch)
                                (Hashtbl.find_opt by_key)
                            in
                            (ch, p))
                          children))
        in
        match rel with
        | Rel.Belongs_to { child_key; parent; parent_key } ->
            run (fun ch -> Some (child_key.get ch)) parent parent_key
        | Rel.Belongs_to_opt { child_key; parent; parent_key } ->
            run (fun ch -> child_key.get ch) parent parent_key)

  let load_has_one : type p c.
      ?where:c cond ->
      db ->
      (p, c) Rel.has_one ->
      p list ->
      ((p * c option) list, [> Caqti_error.call_or_retrieve ]) result fiber =
   fun ?where:extra_where db rel parents ->
    let as_many =
      match rel with
      | Rel.Has_one { parent_key; child; child_key } ->
          Rel.Has_many { parent_key; child; child_key }
      | Rel.Has_one_opt { parent_key; child; child_key } ->
          Rel.Has_many_opt { parent_key; child; child_key }
    in
    load_many ?where:extra_where db as_many parents
    |> B.map
         (Result.map
            (List.map (fun (p, cs) ->
                 (p, match cs with c :: _ -> Some c | [] -> None))))

  let load_many_to_many : type s t.
      ?where:(t Multi.ref -> bool Expr.t) ->
      ?order_by:(t Multi.ref -> Expr.order list) ->
      db ->
      (s, t) Rel.many_to_many ->
      s list ->
      ((s * t list) list, [> Caqti_error.call_or_retrieve ]) result fiber =
   fun ?where:extra_where ?order_by:ord db rel sources ->
    match sources with
    | [] -> B.return (Ok [])
    | _ -> (
        match rel with
        | Rel.Many_to_many
            {
              source_key;
              through;
              through_source_key;
              through_target_key;
              target;
              target_key;
            } ->
            let seen = Hashtbl.create (List.length sources) in
            let keys =
              List.filter_map
                (fun s ->
                  let k = source_key.get s in
                  if Hashtbl.mem seen k then None
                  else (
                    Hashtbl.add seen k ();
                    Some k))
                sources
            in
            let q =
              Multi.(
                build
                  (let* j = from through in
                   let* tgt =
                     join target ~on:(fun tgt ->
                         Expr.Ops.(tgt.@(target_key) = j.@(through_target_key)))
                   in
                   let* () =
                     where Expr.Ops.(is_in j.@(through_source_key) keys)
                   in
                   let* () = opt extra_where (fun f -> where (f tgt)) in
                   let* () = opt ord (fun f -> order_by (f tgt)) in
                   select
                     (let+ sk = expr j.@(through_source_key)
                      and+ row = whole tgt in
                      (sk, row))))
            in
            query_all db q
            |> B.map
                 (Result.map (fun rows ->
                      let grouped = Hashtbl.create (List.length keys) in
                      List.iter
                        (fun (sk, tgt) ->
                          let cur =
                            Option.value
                              (Hashtbl.find_opt grouped sk)
                              ~default:[]
                          in
                          Hashtbl.replace grouped sk (tgt :: cur))
                        rows;
                      List.map
                        (fun s ->
                          let k = source_key.get s in
                          let ts =
                            Option.value
                              (Hashtbl.find_opt grouped k)
                              ~default:[]
                            |> List.rev
                          in
                          (s, ts))
                        sources)))
end

module Make (B : BACKEND) =
  Make_with
    (B)
    (struct
      type guard = {
        bind : 'a 'b. 'a B.fiber -> ('a -> 'b B.fiber) -> 'b B.fiber;
        catch : 'a. (unit -> 'a B.fiber) -> (exn -> 'a B.fiber) -> 'a B.fiber;
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      let guard = None
    end)

module Make_guarded (B : GUARDED_BACKEND) =
  Make_with
    (B)
    (struct
      type guard = {
        bind : 'a 'b. 'a B.fiber -> ('a -> 'b B.fiber) -> 'b B.fiber;
        catch : 'a. (unit -> 'a B.fiber) -> (exn -> 'a B.fiber) -> 'a B.fiber;
        finally : 'a. (unit -> 'a B.fiber) -> (unit -> unit) -> 'a B.fiber;
      }

      let guard = Some { bind = B.bind; catch = B.catch; finally = B.finally }
    end)

module Blocking_backend = struct
  type 'a fiber = 'a
  type ('a, 'e) stream = ('a, 'e) Caqti_blocking.Stream.t

  module type CONNECTION = Caqti_blocking.CONNECTION

  let return x = x
  let map f x = f x
  let bind x f = f x
  let catch f h = try f () with e -> h e

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
