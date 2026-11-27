(* Constraint violation classification and Ecto-style changesets. *)

open Table
open Query

(* ---------- errors ---------- *)

type violation = [ `Unique | `Foreign_key | `Not_null | `Check | `Other ]

let violation = function
  | (`Request_failed _ | `Response_failed _) as e -> (
      match Caqti_error.cause e with
      | `Unique_violation -> Some `Unique
      | `Foreign_key_violation | `Restrict_violation -> Some `Foreign_key
      | `Not_null_violation -> Some `Not_null
      | `Check_violation -> Some `Check
      | #Caqti_error.integrity_constraint_violation -> Some `Other
      | _ -> None)
  | _ -> None

let is_unique_violation e = violation e = Some `Unique

(* ---------- changesets ---------- *)

type kind =
  [ `Required
  | `Cast
  | `Length
  | `Range
  | `Inclusion
  | `Exclusion
  | `Format
  | `Unique
  | `Foreign_key
  | `Check
  | `Custom ]

type error = { field : string; message : string; kind : kind }

(* A constraint the database may refuse a write with, and the error it
   becomes. [named]: the name was given, not the default one. *)
type cons = {
  ckind : [ `Unique | `Foreign_key | `Check ];
  cname : string;
  named : bool;
  ccol : string;
  cmessage : string;
}

type 'r t = {
  table : 'r table;
  data : 'r option;
  changes : 'r assign list; (* in the order made *)
  errors : error list; (* in the order found *)
  params : (string * string) list;
  constraints : cons list;
}

type 'r field = Field : ('r, 'a) col -> 'r field

let field c = Field c

let make table data =
  { table; data; changes = []; errors = []; params = []; constraints = [] }

let for_insert table = make table None
let for_update table row = make table (Some row)
let data cs = cs.data
let changes cs = cs.changes
let errors cs = cs.errors
let is_valid cs = cs.errors = []
let has_error name cs = List.exists (fun e -> e.field = name) cs.errors

let errors_on c cs =
  List.filter_map
    (fun e -> if e.field = c.name then Some e.message else None)
    cs.errors

let add_error ?(kind = `Custom) c message cs =
  { cs with errors = cs.errors @ [ { field = c.name; message; kind } ] }

(* The change made to [c], found by name; Caqti says whether its type is
   [c]'s, so no cast is needed. *)
let get_change : type r a. (r, a) col -> r t -> a option =
 fun c cs ->
  let same_col : type b. (r, b) col -> b -> a option =
   fun c' v ->
    if c'.name <> c.name then None
    else
      match Caqti_type.unify c'.ty.caqti c.ty.caqti with
      | Some Caqti_type.Equal -> Some v
      | None -> None
  in
  List.find_map
    (function Set (c', v) -> same_col c' v | Set_expr _ -> None)
    cs.changes

let get c cs =
  match get_change c cs with
  | Some v -> Some v
  | None -> Option.map c.get cs.data

let drop_change c cs =
  {
    cs with
    changes = List.filter (fun a -> assign_col_name a <> c.name) cs.changes;
  }

let same a b = try a = b with Invalid_argument _ -> false

(* A change to the value a row already has is no change. *)
let put c v cs =
  let cs = drop_change c cs in
  match cs.data with
  | Some row when same (c.get row) v -> cs
  | _ -> { cs with changes = cs.changes @ [ Set (c, v) ] }

let update_change c f cs =
  if has_error c.name cs then cs
  else match get_change c cs with None -> cs | Some v -> put c (f v) cs

let update_change_opt c f cs =
  if has_error c.name cs then cs
  else
    match get_change c cs with
    | None | Some None -> cs
    | Some (Some v) -> put c (Some (f v)) cs

let cast params fields cs =
  let allowed = List.map (fun (Field c) -> c.name) fields in
  let cast_params = List.filter (fun (k, _) -> List.mem k allowed) params in
  let cs =
    {
      cs with
      params =
        cast_params
        @ List.filter
            (fun (k, _) -> not (List.mem_assoc k cast_params))
            cs.params;
    }
  in
  List.fold_left
    (fun cs (Field c) ->
      match List.assoc_opt c.name params with
      | None -> cs
      | Some s -> (
          let parse =
            match c.ty.parse with
            | Some p -> p
            | None ->
                invalid_arg
                  (Printf.sprintf
                     "Qb.Changeset.cast: column %s has no of_string (see Qb.ty)"
                     c.name)
          in
          let cs = drop_change c cs in
          if (not c.ty.nullable) && String.trim s = "" then
            add_error ~kind:`Required c "can't be blank" cs
          else
            match parse s with
            | Some v -> put c v cs
            | None -> add_error ~kind:`Cast c "is invalid" cs))
    cs fields

let validate_required fields cs =
  List.fold_left
    (fun cs (Field c) ->
      if has_error c.name cs then cs
      else
        match get c cs with
        | Some v when not (c.ty.blank v) -> cs
        | _ -> add_error ~kind:`Required c "can't be blank" cs)
    cs fields

type 'a check = show:('a -> string) -> 'a -> (kind * string) option

let run_checks c show checks v cs =
  List.fold_left
    (fun cs (check : _ check) ->
      match check ~show v with
      | Some (kind, message) -> add_error ~kind c message cs
      | None -> cs)
    cs checks

let show_of c = Option.value c.ty.show ~default:(fun _ -> "the value")

(* Like Ecto, a check looks at the change only: what is already stored was
   checked when it was written. A field that failed to cast is left alone. *)
let validate c checks cs =
  if has_error c.name cs then cs
  else
    match get_change c cs with
    | None -> cs
    | Some v -> run_checks c (show_of c) checks v cs

let validate_opt c checks cs =
  if has_error c.name cs then cs
  else
    match get_change c cs with
    | None | Some None -> cs
    | Some (Some v) ->
        let show = show_of c in
        run_checks c (fun v -> show (Some v)) checks v cs

(* Characters, not bytes: UTF-8 is decoded. *)
let utf8_length s =
  let rec go i n =
    if i >= String.length s then n
    else go (i + Uchar.utf_decode_length (String.get_utf_8_uchar s i)) (n + 1)
  in
  go 0 0

let plural n = if n = 1 then "character" else "characters"

let length ?min ?max ?is () : string check =
 fun ~show:_ s ->
  let n = utf8_length s in
  match (is, min, max) with
  | Some k, _, _ when n <> k ->
      Some (`Length, Printf.sprintf "should be %d %s" k (plural k))
  | _, Some k, _ when n < k ->
      Some (`Length, Printf.sprintf "should be at least %d %s" k (plural k))
  | _, _, Some k when n > k ->
      Some (`Length, Printf.sprintf "should be at most %d %s" k (plural k))
  | _ -> None

let range ?gt ?ge ?lt ?le () : _ check =
 fun ~show v ->
  let fails bound ok message =
    match bound with
    | Some b when not (ok (compare v b)) ->
        Some (`Range, Printf.sprintf "must be %s %s" message (show b))
    | _ -> None
  in
  List.find_map Fun.id
    [
      fails gt (fun c -> c > 0) "greater than";
      fails ge (fun c -> c >= 0) "greater than or equal to";
      fails lt (fun c -> c < 0) "less than";
      fails le (fun c -> c <= 0) "less than or equal to";
    ]

let one_of xs : _ check =
 fun ~show:_ v -> if List.mem v xs then None else Some (`Inclusion, "is invalid")

let none_of xs : _ check =
 fun ~show:_ v ->
  if List.mem v xs then Some (`Exclusion, "is reserved") else None

let format ?(message = "has invalid format") ok : string check =
 fun ~show:_ v -> if ok v then None else Some (`Format, message)

let check message ok : _ check =
 fun ~show:_ v -> if ok v then None else Some (`Custom, message)

(* Default names are Postgres's for a constraint on one column. *)
let add_constraint ckind suffix ?name ~message c cs =
  let cname =
    match name with
    | Some n -> n
    | None -> Printf.sprintf "%s_%s_%s" cs.table.tname c.name suffix
  in
  {
    cs with
    constraints =
      cs.constraints
      @ [
          {
            ckind;
            cname;
            named = name <> None;
            ccol = c.name;
            cmessage = message;
          };
        ];
  }

let unique_constraint ?name ?(message = "has already been taken") c cs =
  add_constraint `Unique "key" ?name ~message c cs

let foreign_key_constraint ?name ?(message = "does not exist") c cs =
  add_constraint `Foreign_key "fkey" ?name ~message c cs

let check_constraint ?name ?(message = "is invalid") c cs =
  add_constraint `Check "check" ?name ~message c cs

(* ---- which constraint a driver's message names ---- *)

let index_from s i sub =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then Some i
    else go (i + 1)
  in
  go i

(* Every piece of [s] between [opening] and [closing]. *)
let between opening closing s =
  let rec go i acc =
    match index_from s i opening with
    | None -> List.rev acc
    | Some j -> (
        let start = j + String.length opening in
        match index_from s start closing with
        | None -> List.rev acc
        | Some k -> go (k + 1) (String.sub s start (k - start) :: acc))
  in
  go 0 []

(* The names right after [marker], separated by ", ": "users.email,
   users.org". Caqti goes on after them on the same line. *)
let names_after marker s =
  let ident c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || Sql_tokens.is_digit c || c = '_' || c = '.'
    || Char.code c >= 128
  in
  let rec names i acc =
    let j = ref i in
    while !j < String.length s && ident s.[!j] do
      incr j
    done;
    let acc = if !j > i then String.sub s i (!j - i) :: acc else acc in
    if !j > i && !j + 1 < String.length s && s.[!j] = ',' && s.[!j + 1] = ' '
    then names (!j + 2) acc
    else List.rev acc
  in
  match index_from s 0 marker with
  | None -> []
  | Some j -> names (j + String.length marker) []

let after_dot s =
  match String.rindex_opt s '.' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

(* The names in a driver's message for a refused write:
   - Postgres: ... violates unique constraint "users_email_key"
   - SQLite:   UNIQUE constraint failed: users.email, users.org
               CHECK constraint failed: users_age_check
   - MySQL:    Duplicate entry 'x' for key 'users.users_email_key'
               ... CONSTRAINT `comments_author_id_fkey` FOREIGN KEY ...
               Check constraint 'users_age_check' is violated. *)
let names_in msg =
  let quoted =
    between "constraint \"" "\"" msg
    @ between "for key '" "'" msg
    @ between "CONSTRAINT `" "`" msg
    @ between "Check constraint '" "'" msg
  in
  let sqlite_check = names_after "CHECK constraint failed: " msg in
  let sqlite_unique = names_after "UNIQUE constraint failed: " msg in
  ( List.concat_map (fun n -> [ n; after_dot n ]) (quoted @ sqlite_check),
    sqlite_unique )

let matches cs msg con =
  let names, sqlite_cols = names_in msg in
  List.mem con.cname names
  || con.ckind = `Unique
     && (List.mem (cs.table.tname ^ "." ^ con.ccol) sqlite_cols
        || ((not con.named) && List.mem con.ccol names))

(* SQLite doesn't say which foreign key failed: it is the one declared, if
   only one was. *)
let unnamed_foreign_key cs msg =
  match List.filter (fun c -> c.ckind = `Foreign_key) cs.constraints with
  | [ con ] when index_from msg 0 "FOREIGN KEY constraint failed" <> None ->
      Some con
  | _ -> None

let on_error cs e =
  let found kind =
    let msg = Caqti_error.show e in
    match
      List.find_opt
        (fun con -> con.ckind = kind && matches cs msg con)
        cs.constraints
    with
    | Some con -> Some con
    | None when kind = `Foreign_key -> unnamed_foreign_key cs msg
    | None -> None
  in
  let con =
    match violation e with
    | Some ((`Unique | `Foreign_key | `Check) as kind) -> found kind
    (* SQLite reports duplicate PRIMARY KEY as SQLITE_CONSTRAINT_PRIMARYKEY,
       which Caqti maps to [`Other] rather than [`Unique], even though its
       message is "UNIQUE constraint failed: <table>.<col>". *)
    | Some `Other
      when index_from (Caqti_error.show e) 0 "UNIQUE constraint failed: "
           <> None ->
        found `Unique
    | Some (`Not_null | `Other) | None -> None
  in
  Option.map
    (fun con ->
      {
        cs with
        errors =
          cs.errors
          @ [
              {
                field = con.ccol;
                message = con.cmessage;
                kind = (con.ckind :> kind);
              };
            ];
      })
    con

(* What a form shows for [c]: what was typed, or else the value. *)
let input c cs =
  match List.assoc_opt c.name cs.params with
  | Some s -> s
  | None -> (
      match (get c cs, c.ty.show) with Some v, Some show -> show v | _ -> "")

let primary_key cs =
  match List.filter (fun (Any c) -> c.primary_key) cs.table.cols with
  | [ pk ] -> pk
  | _ ->
      invalid_arg
        (Printf.sprintf
           "Qb.Changeset: table %s needs exactly one primary key column to \
            update a row"
           cs.table.tname)
