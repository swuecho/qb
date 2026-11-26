(* libpg_query: parsing and checking SQL. *)

external raw_parse : string -> (string, string * int) result
  = "qb_pg_query_parse"

type error = { message : string; position : int option }

let error_to_string ?sql e =
  match (sql, e.position) with
  | Some sql, Some p when p <= String.length sql ->
      (* Show the line holding the error with a caret under it. *)
      let start =
        match String.rindex_from_opt sql (max 0 (p - 1)) '\n' with
        | Some i when i < p -> i + 1
        | _ -> 0
      in
      let stop =
        Option.value
          (String.index_from_opt sql p '\n')
          ~default:(String.length sql)
      in
      let line_no =
        1
        + String.fold_left
            (fun n c -> if c = '\n' then n + 1 else n)
            0 (String.sub sql 0 start)
      in
      Printf.sprintf "line %d: %s\n  %s\n  %s^" line_no e.message
        (String.sub sql start (stop - start))
        (String.make (p - start) ' ')
  | _, Some p -> Printf.sprintf "%s (at offset %d)" e.message p
  | _, None -> e.message

(* libpg_query's cursorpos is 1-based, 0 when unknown. *)
let error_of (message, cursorpos) =
  { message; position = (if cursorpos > 0 then Some (cursorpos - 1) else None) }

let parse sql =
  match raw_parse sql with
  | Error e -> Error (error_of e)
  | Ok json -> (
      match Yojson.Safe.from_string json with
      | j -> Ok j
      | exception Yojson.Json_error m ->
          Error
            { message = "libpg_query returned bad JSON: " ^ m; position = None }
      )

let statements sql =
  Result.map
    (fun j ->
      match Yojson.Safe.Util.member "stmts" j with
      | `List stmts ->
          List.map (fun s -> Yojson.Safe.Util.member "stmt" s) stmts
      | _ -> [])
    (parse sql)

(* Each statement's own text. libpg_query gives where each starts and its
   length, leaving out a start of 0 and the length of the last one. *)
let split sql =
  Result.map
    (fun j ->
      match Yojson.Safe.Util.member "stmts" j with
      | `List stmts ->
          List.filter_map
            (fun s ->
              let int k =
                match Yojson.Safe.Util.member k s with `Int i -> i | _ -> 0
              in
              let at = int "stmt_location" in
              let len =
                match int "stmt_len" with 0 -> String.length sql - at | n -> n
              in
              match String.trim (String.sub sql at len) with
              | "" -> None
              | text -> Some text)
            stmts
      | _ -> [])
    (parse sql)

(* Caqti writes parameters as [?]; Postgres wants [$1], [$2], ... . Rewrite
   every [?] outside quotes, and remember where each output byte came from so
   error positions point into the original text. *)
let of_caqti sql =
  let n = String.length sql in
  let out = Buffer.create (n + 16) in
  let origin = ref [] in
  let add_char i c =
    Buffer.add_char out c;
    origin := i :: !origin
  in
  let param = ref 0 in
  (* A [?] in a string, a quoted name, a comment or a $$-quoted body is not
     a placeholder: those are tokens of their own. *)
  List.iter
    (fun (t : Sql_tokens.token) ->
      match t.kind with
      | Question ->
          incr param;
          String.iter (add_char t.at) ("$" ^ string_of_int !param)
      | _ -> String.iteri (fun k c -> add_char (t.at + k) c) t.text)
    (Sql_tokens.tokens sql);
  let origin = Array.of_list (List.rev !origin) in
  ( Buffer.contents out,
    !param,
    fun p -> if p < Array.length origin then origin.(p) else n )

let check ?(caqti = false) sql =
  let text, _, back = if caqti then of_caqti sql else (sql, 0, Fun.id) in
  match raw_parse text with
  | Ok _ -> Ok ()
  | Error e ->
      let e = error_of e in
      Error { e with position = Option.map back e.position }

(* pg_dump writes psql meta-commands such as \restrict, which aren't SQL.
   Blank them out, keeping every other byte where it was. *)
let strip_psql_meta sql =
  String.split_on_char '\n' sql
  |> List.map (fun line ->
      if String.length line > 0 && line.[0] = '\\' then
        String.make (String.length line) ' '
      else line)
  |> String.concat "\n"
