(* Logging, parameter packing, Caqti query parsing, and prepared-statement cache. *)

open Table

let src = Logs.Src.create "qb" ~doc:"SQL statements run by qb"

module Log = (val Logs.src_log src : Logs.LOG)

(* Fold a reversed parameter list into one nested-tuple Caqti type and value. *)
type packed = Pack : 'a Caqti_type.t * 'a -> packed

let pack_rev params =
  let pack (Pack (ts, vs)) (Param (t, v)) =
    Pack (Caqti_type.t2 t ts, (v, vs))
  in
  List.fold_left pack (Pack (Caqti_type.unit, ())) params

let finish o =
  let sql = Buffer.contents o.buf in
  (* [o.params] is reversed, so [pack_rev] restores SQL order. *)
  (sql, List.rev o.params, pack_rev o.params)

(* The parameters as a log line shows them: [18, "ann@x.org", NULL]. A long
   value is cut short. Caqti prints a value as part of a row, with the
   separator and line break that come before it there. *)
let pp_params ppf = function
  | [] -> ()
  | params ->
      let show (Param (t, v)) =
        let s = Format.asprintf "%a" Caqti_type.pp_value (t, v) in
        let n = String.length s in
        let rec start i =
          if i < n && (s.[i] = ',' || s.[i] = ' ' || s.[i] = '\n') then
            start (i + 1)
          else i
        in
        let s = String.sub s (start 0) (n - start 0) in
        let s = String.concat " " (String.split_on_char '\n' s) in
        if s = "NONE" then "NULL"
        else if String.length s > 80 then String.sub s 0 77 ^ "..."
        else s
      in
      Format.fprintf ppf " [%s]" (String.concat ", " (List.map show params))

(* ---------- SQL text ---------- *)

(* What Caqti is given to send. Caqti's own parser reads more into the text
   than a statement means ([?] as a parameter next to [$1], [$(...)]), and
   raises when it can't; here a parameter is [$n] where the text has any, and
   [?] otherwise, so that Postgres's [?] operators can be written. Nothing in
   a string, a quoted name, a comment or a $$-quoted body is one. *)
let caqti_query sql =
  let toks = Sql_tokens.tokens sql in
  let numbered =
    List.exists
      (fun (t : Sql_tokens.token) ->
        match t.kind with Numbered k -> k >= 1 | _ -> false)
      toks
  in
  let parts = ref [] and text = Buffer.create (String.length sql) in
  let next = ref 0 in
  let param k =
    if Buffer.length text > 0 then (
      parts := Caqti_query.L (Buffer.contents text) :: !parts;
      Buffer.clear text);
    parts := Caqti_query.P k :: !parts
  in
  List.iter
    (fun (t : Sql_tokens.token) ->
      match t.kind with
      | Numbered k when numbered && k >= 1 -> param (k - 1)
      | Question when not numbered ->
          param !next;
          incr next
      | _ -> Buffer.add_string text t.text)
    toks;
  if Buffer.length text > 0 then
    parts := Caqti_query.L (Buffer.contents text) :: !parts;
  Caqti_query.S (List.rev !parts)

(* Requests are cached by SQL text so the driver can keep them prepared.
   An entry is reused only if Caqti confirms its parameter and row types are
   the same as the new ones, so a hit is type-safe. The cache is bounded; past
   the limit, new statements run unprepared. Guarded by a mutex so it is safe
   to share across threads and OCaml 5 domains. *)
type 'm entry =
  | Entry :
      'p Caqti_type.t * 'x Caqti_type.t * ('p, 'x, 'm) Caqti_request.t
      -> 'm entry

type 'm t = {
  mult : 'm Caqti_mult.t;
  mutex : Mutex.t;
  table : (string, 'm entry) Hashtbl.t;
}

(* Compatible with OCaml 4.14 (Mutex.protect was added in 5.1). *)
let with_lock m f =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) f

let limit = 512
let make mult = { mult; mutex = Mutex.create (); table = Hashtbl.create 64 }
let zero : Caqti_mult.zero t = make Caqti_mult.zero
let one : Caqti_mult.one t = make Caqti_mult.one
let zero_or_one : Caqti_mult.zero_or_one t = make Caqti_mult.zero_or_one
let zero_or_more : Caqti_mult.zero_or_more t = make Caqti_mult.zero_or_more

(* [~oneshot]: a statement that isn't worth keeping, whatever room there
   is. *)
let request (type p x) ?(oneshot = false) (c : 'm t) (pt : p Caqti_type.t)
    (rt : x Caqti_type.t) sql : (p, x, 'm) Caqti_request.t =
  let create ~oneshot q =
    Caqti_request.create ~oneshot pt rt c.mult (fun _ -> q)
  in
  let lookup () : [ `Hit of (p, x, 'm) Caqti_request.t | `Oneshot | `Miss ] =
    match Hashtbl.find_opt c.table sql with
    | Some (Entry (pt', rt', r)) -> (
        match (Caqti_type.unify pt' pt, Caqti_type.unify rt' rt) with
        | Some Caqti_type.Equal, Some Caqti_type.Equal -> `Hit r
        (* Same SQL with different types: don't replace the cached entry, which
           would allocate a new prepared statement on every alternating call. *)
        | _ -> `Oneshot)
    | None -> if Hashtbl.length c.table >= limit then `Oneshot else `Miss
  in
  if oneshot then create ~oneshot:true (caqti_query sql)
  else
    match with_lock c.mutex lookup with
    | `Hit r -> r
    | `Oneshot -> create ~oneshot:true (caqti_query sql)
    | `Miss ->
        (* Parse outside the mutex; the resulting Caqti_query.t is immutable,
           avoiding both lock contention and concurrent Lazy.force races. *)
        let q = caqti_query sql in
        with_lock c.mutex (fun () ->
            match lookup () with
            | `Hit r -> r
            | `Oneshot -> create ~oneshot:true q
            | `Miss ->
                let r = create ~oneshot:false q in
                Hashtbl.replace c.table sql (Entry (pt, rt, r));
                r)
