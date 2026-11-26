(* Dialects, column types, text conversion for forms, and identifier quoting. *)

type dialect = Postgres | Sqlite | Mysql

(* [parse] and [show] turn a value into text and back, for values that come
   from a form (see Changeset); [blank] says a value counts as missing. *)
type 'a ty = {
  caqti : 'a Caqti_type.t;
  ddl : dialect -> string;
  nullable : bool;
  is_null : 'a -> bool;
  parse : (string -> 'a option) option;
  show : ('a -> string) option;
  blank : 'a -> bool;
}

(* ---------- values as text ---------- *)

(* Decimal only: int_of_string would also take "0x1f", "0b1" and "1_000". *)
let parse_integer of_string s =
  let s = String.trim s in
  let n = String.length s in
  let start = if n > 0 && (s.[0] = '-' || s.[0] = '+') then 1 else 0 in
  if start = n then None
  else if String.for_all Sql_tokens.is_digit (String.sub s start (n - start))
  then of_string s
  else None

(* Decimal only: float_of_string would also take "nan", "inf" and hex. *)
let parse_float s =
  let s = String.trim s in
  if
    String.exists Sql_tokens.is_digit s
    && String.for_all
         (fun c -> Sql_tokens.is_digit c || String.contains "+-.eE" c)
         s
  then float_of_string_opt s
  else None

(* The shortest text that reads back as the same float. *)
let show_float f =
  let rec go p =
    let s = Printf.sprintf "%.*g" p f in
    if p >= 17 || float_of_string s = f then s else go (p + 1)
  in
  go 1

let parse_bool s =
  match String.lowercase_ascii (String.trim s) with
  | "true" | "1" | "on" | "yes" -> Some true
  | "false" | "0" | "off" | "no" -> Some false
  | _ -> None

(* RFC 3339, or what an HTML datetime-local input sends ("2026-11-22T10:30",
   no zone), read as UTC. *)
let parse_ptime s =
  let s = String.trim s in
  let rfc s =
    match Ptime.of_rfc3339 ~strict:false s with
    | Ok (t, _, _) -> Some t
    | Error _ -> None
  in
  match rfc s with
  | Some t -> Some t
  | None -> (
      match String.length s with
      | 16 -> rfc (s ^ ":00Z")
      | 19 -> rfc (s ^ "Z")
      | _ -> None)

let show_ptime t =
  let frac_s = if Ptime.(Span.equal (frac_s t) Span.zero) then 0 else 6 in
  Ptime.to_rfc3339 ~tz_offset_s:0 ~frac_s t

let parse_date s =
  match
    Scanf.sscanf (String.trim s) "%4d-%2d-%2d%!" (fun y m d -> (y, m, d))
  with
  | ymd -> Ptime.of_date ymd
  | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) -> None

let show_date t =
  let y, m, d = Ptime.to_date t in
  Printf.sprintf "%04d-%02d-%02d" y m d

let parse_uuid s =
  let s = String.lowercase_ascii (String.trim s) in
  let hex c = Sql_tokens.is_digit c || (c >= 'a' && c <= 'f') in
  if
    String.length s = 36
    && List.for_all
         (fun i ->
           match i with 8 | 13 | 18 | 23 -> s.[i] = '-' | _ -> hex s.[i])
         (List.init 36 Fun.id)
  then Some s
  else None

(* How to read and show a value of a Caqti type qb knows, found by asking
   Caqti whether the type is one of them. *)
let text_form : type a.
    a Caqti_type.t -> (string -> a option) option * (a -> string) option =
 fun t ->
  let module T = Caqti_type in
  let is u = T.unify u t in
  match is T.string with
  | Some T.Equal -> (Some Option.some, Some Fun.id)
  | None -> (
      match is T.int with
      | Some T.Equal ->
          (Some (parse_integer int_of_string_opt), Some string_of_int)
      | None -> (
          match is T.int64 with
          | Some T.Equal ->
              (Some (parse_integer Int64.of_string_opt), Some Int64.to_string)
          | None -> (
              match is T.int32 with
              | Some T.Equal ->
                  ( Some (parse_integer Int32.of_string_opt),
                    Some Int32.to_string )
              | None -> (
                  match is T.float with
                  | Some T.Equal -> (Some parse_float, Some show_float)
                  | None -> (
                      match is T.bool with
                      | Some T.Equal -> (Some parse_bool, Some string_of_bool)
                      | None -> (
                          match is T.ptime with
                          | Some T.Equal -> (Some parse_ptime, Some show_ptime)
                          | None -> (
                              match is T.pdate with
                              | Some T.Equal -> (Some parse_date, Some show_date)
                              | None -> (
                                  match is T.octets with
                                  | Some T.Equal ->
                                      (Some Option.some, Some Fun.id)
                                  | None -> (None, None)))))))))

(* Text that is only whitespace is blank. *)
let blank_of : type a. a Caqti_type.t -> a -> bool =
 fun t ->
  match Caqti_type.unify Caqti_type.string t with
  | Some Caqti_type.Equal -> fun s -> String.trim s = ""
  | None -> fun _ -> false

let ty ?sqlite ?mysql ?of_string ?to_string caqti ddl =
  let ddl = function
    | Postgres -> ddl
    | Sqlite -> Option.value sqlite ~default:ddl
    | Mysql -> Option.value mysql ~default:ddl
  in
  let parse, show = text_form caqti in
  let parse = match of_string with Some _ -> of_string | None -> parse in
  let show = match to_string with Some _ -> to_string | None -> show in
  {
    caqti;
    ddl;
    nullable = false;
    is_null = (fun _ -> false);
    parse;
    show;
    blank = blank_of caqti;
  }

(* OCaml ints are 63-bit and floats are double precision: Postgres INTEGER
   and REAL would overflow or round them. SQLite's INTEGER is already 64-bit,
   and only "INTEGER PRIMARY KEY" makes an auto-assigned rowid. *)
let int = ty Caqti_type.int "BIGINT" ~sqlite:"INTEGER"
let int64 = ty Caqti_type.int64 "BIGINT" ~sqlite:"INTEGER"

let float =
  ty Caqti_type.float "DOUBLE PRECISION" ~sqlite:"REAL" ~mysql:"DOUBLE"

let text = ty Caqti_type.string "TEXT"
let bool = ty Caqti_type.bool "BOOLEAN"
let octets = ty Caqti_type.octets "BYTEA" ~sqlite:"BLOB" ~mysql:"LONGBLOB"

let ptime =
  ty Caqti_type.ptime "TIMESTAMP WITH TIME ZONE" ~sqlite:"TEXT"
    ~mysql:"DATETIME(6)"

let date = ty Caqti_type.pdate "DATE" ~sqlite:"TEXT"

(* Caqti sends strings to Postgres untyped, so the server converts them to
   uuid / jsonb itself. *)
let uuid =
  ty Caqti_type.string "UUID" ~sqlite:"TEXT" ~mysql:"CHAR(36)"
    ~of_string:parse_uuid

let json = ty Caqti_type.string "JSONB" ~sqlite:"TEXT" ~mysql:"JSON"

(* Blank text is NULL: a form's empty field clears a nullable column. *)
let nullable t =
  {
    caqti = Caqti_type.option t.caqti;
    ddl = t.ddl;
    nullable = true;
    is_null = Option.is_none;
    parse =
      Option.map
        (fun p s ->
          if String.trim s = "" then Some None else Option.map Option.some (p s))
        t.parse;
    show = Option.map (fun f -> function None -> "" | Some v -> f v) t.show;
    blank = (function None -> true | Some v -> t.blank v);
  }

(* Identifiers are always quoted, so names like "user" or "order" work. *)
let quote dialect name =
  match dialect with
  | Mysql -> "`" ^ String.concat "``" (String.split_on_char '`' name) ^ "`"
  | Postgres | Sqlite ->
      "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""
