(* Scanning :name parameters, splitting -- name: query files, and rewriting
   bare [$n IS NULL] tests with explicit casts. *)

open Schema
open Query_ast

(* Postgres numbers its parameters: each [:name] becomes [$n], the same [n]
   every time the name comes back. [:name?] says the value may be NULL.
   [IN :name] is a list of values: it becomes [IN ($n)], and the list is
   spelled out when the statement runs. Comments are dropped. Returns the
   text, the names with their [?] in order of [n], the names that are
   lists, where each output byte came from, and where each name is first
   written. *)
let number_params sql =
  let n = String.length sql in
  let out = Buffer.create n in
  let origin = ref [] in
  let add i c =
    Buffer.add_char out c;
    origin := i :: !origin
  in
  let params = ref [] in
  (* Where each name is first written. *)
  let first = ref [] in
  (* Each name, and whether it is used as a list. *)
  let kinds = Hashtbl.create 8 in
  (* Whether what has been written so far ends with the keyword IN. *)
  let after_in () =
    let rec back k =
      if
        k > 0
        &&
        match Buffer.nth out (k - 1) with
        | ' ' | '\n' | '\t' | '\r' -> true
        | _ -> false
      then back (k - 1)
      else k
    in
    let k = back (Buffer.length out) in
    k >= 2
    && Char.lowercase_ascii (Buffer.nth out (k - 2)) = 'i'
    && Char.lowercase_ascii (Buffer.nth out (k - 1)) = 'n'
    && (k = 2 || not (Sql_tokens.is_ident (Buffer.nth out (k - 3))))
  in
  let index name =
    let rec go i = function
      | [] -> None
      | (p, _) :: _ when p = name -> Some i
      | _ :: rest -> go (i + 1) rest
    in
    go 1 !params
  in
  let rec go : Sql_tokens.token list -> unit = function
    | [] -> ()
    (* A line comment is dropped, its newline kept; a block comment is a
       space. *)
    | { kind = Comment; text; at } :: rest ->
        if not (String.starts_with ~prefix:"--" text) then add at ' ';
        go rest
    | { kind = Named name; at = i; text } :: rest ->
        (* :name? has its ? right after it. *)
        let optional, rest =
          match rest with
          | { kind = Question; at; _ } :: rest when at = i + String.length text
            ->
              (true, rest)
          | _ -> (false, rest)
        in
        let is_list = after_in () in
        if is_list && optional then
          raise (Fail ("a list can't be NULL: write IN :" ^ name, Some i));
        (match Hashtbl.find_opt kinds name with
        | Some l when l <> is_list ->
            raise
              (Fail
                 ( Printf.sprintf
                     ":%s is a list in one place (IN :%s) and a single value \
                      in another"
                     name name,
                   Some i ))
        | _ -> Hashtbl.replace kinds name is_list);
        let k =
          match index name with
          | Some k ->
              if optional then
                params := List.map (fun (p, o) -> (p, o || p = name)) !params;
              k
          | None ->
              params := !params @ [ (name, optional) ];
              first := (name, i) :: !first;
              List.length !params
        in
        String.iter (add i)
          (if is_list then Printf.sprintf "($%d)" k else "$" ^ string_of_int k);
        go rest
    | { kind = Numbered _; at; _ } :: _ ->
        raise (Fail ("parameters are written :name here, not $1", Some at))
    (* Strings, quoted names and $$-quoted bodies among them: nothing inside
       is a parameter. *)
    | { at; text; _ } :: rest ->
        String.iteri (fun k c -> add (at + k) c) text;
        go rest
  in
  (* A [Fail] from the scan itself is positioned in the input. *)
  go (Sql_tokens.tokens sql);
  let origin = Array.of_list (List.rev !origin) in
  ( Buffer.contents out,
    List.map
      (fun (name, optional) ->
        (name, optional, Hashtbl.find_opt kinds name = Some true))
      !params,
    (fun p -> if p < Array.length origin then origin.(p) else n),
    !first )

(* ---------- files ---------- *)

(* [-- name: get_user] starts a query. *)
let name_line line =
  let line = String.trim line in
  let prefix p s =
    let n = String.length p in
    if String.length s >= n && String.sub s 0 n = p then
      Some (String.trim (String.sub s n (String.length s - n)))
    else None
  in
  Option.bind (prefix "--" line) (prefix "name:")

(* Each query's name and text, with the text's offset in the file. A query
   starts at a line that is a [-- name:] comment and runs to the next one;
   one written inside a string or a longer comment isn't a start. *)
let split text =
  let n = String.length text in
  let toks = Sql_tokens.tokens text in
  let line_start at =
    if at = 0 then 0
    else
      match String.rindex_from_opt text (at - 1) '\n' with
      | Some i -> i + 1
      | None -> 0
  in
  let blank s = String.trim s = "" in
  (* Each name, its line's offset, and where the text after it starts. *)
  let names =
    List.filter_map
      (fun (t : Sql_tokens.token) ->
        let line = line_start t.at in
        match t.kind with
        | Comment when blank (String.sub text line (t.at - line)) ->
            Option.map
              (fun name ->
                (name, line, min n (t.at + String.length t.text + 1)))
              (name_line t.text)
        | _ -> None)
      toks
  in
  (* Before the first name there may be comments, and nothing else. *)
  let first = match names with (_, line, _) :: _ -> line | [] -> n in
  List.iter
    (fun (t : Sql_tokens.token) ->
      if t.at < first && t.kind <> Comment && not (blank t.text) then
        raise
          (Fail
             ( "expected -- name: query_name before this statement",
               Some (line_start t.at) )))
    toks;
  let rec go = function
    | [] -> []
    | (name, line, start) :: rest ->
        let stop = match rest with (_, next, _) :: _ -> next | [] -> n in
        (name, line, start, String.sub text start (max 0 (stop - start)))
        :: go rest
  in
  go names

let valid_name name =
  name <> ""
  && Sql_tokens.is_ident_start name.[0]
  && String.for_all Sql_tokens.is_ident name

(* Trailing space and semicolon aren't part of the statement Caqti sends. *)
let rec trim_end s =
  let s = String.trim s in
  let n = String.length s in
  if n > 0 && s.[n - 1] = ';' then trim_end (String.sub s 0 (n - 1)) else s

(* Postgres can't tell the type of a bare [$1 IS NULL], even when $1 is
   typed elsewhere in the statement, so the type worked out here is written
   in: [CAST($1 AS TEXT) IS NULL]. *)
let cast_null_tests sql (params : param list) null_tests =
  List.sort_uniq (fun (a, _) (b, _) -> compare b a) null_tests
  |> List.fold_left
       (fun sql (at, n) ->
         let token = "$" ^ string_of_int n in
         let len = String.length token in
         match List.nth_opt params (n - 1) with
         | Some p
           when at + len <= String.length sql && String.sub sql at len = token
           ->
             String.sub sql 0 at
             ^ Printf.sprintf "CAST(%s AS %s)" token (type_sql p.ty)
             ^ String.sub sql (at + len) (String.length sql - at - len)
         | _ -> sql)
       sql
