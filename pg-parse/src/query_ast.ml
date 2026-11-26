(* Shared types, errors, and "did you mean" hints for SQL query analysis. *)

open Schema

type param = {
  name : string;
  ty : pg_type;
  nullable : bool;
  list : bool;
  at : int;
}

type column = { name : string; ty : pg_type; nullable : bool }
type item = Column of column | Whole of { table : table; nullable : bool }
type t = { name : string; sql : string; params : param list; row : item list }

(* [qualified] is the EXCLUDED row of ON CONFLICT: its columns are only
   meant when written excluded.c. *)
type source = {
  alias : string;
  table : table;
  outer : bool;
  qualified : bool;
  at : int option;
}

type env = {
  scope : source list;
  names : (string * bool * bool) array;
      (* each $n's name, whether it is written :name?, and IN :name *)
  types : pg_type option array;
  seen : int option array; (* where each $n is first used *)
  null_tests : (int * int) list ref; (* [$n IS NULL]: where the $n is, and n *)
}

(* A message and where in the text being read, if known. *)
exception Fail of string * int option

let fail ?at fmt = Printf.ksprintf (fun m -> raise (Fail (m, at))) fmt

let loc j =
  match field "location" j with
  | Some (`Int i) -> Some i
  | _ -> (
      match node j with
      | Some (_, body) -> (
          match field "location" body with Some (`Int i) -> Some i | _ -> None)
      | None -> None)

(* ---------- "did you mean" ---------- *)

(* Edits (insert, delete, replace, swap two neighbours) between two names. *)
let distance a b =
  let n = String.length a and m = String.length b in
  let d = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = 0 to n do
    d.(i).(0) <- i
  done;
  for j = 0 to m do
    d.(0).(j) <- j
  done;
  for i = 1 to n do
    for j = 1 to m do
      let cost = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      d.(i).(j) <-
        min
          (min (d.(i - 1).(j) + 1) (d.(i).(j - 1) + 1))
          (d.(i - 1).(j - 1) + cost);
      if i > 1 && j > 1 && a.[i - 1] = b.[j - 2] && a.[i - 2] = b.[j - 1] then
        d.(i).(j) <- min d.(i).(j) (d.(i - 2).(j - 2) + 1)
    done
  done;
  d.(n).(m)

(* " (did you mean title?)" for the nearest of [candidates], if one is near:
   a third of the name's letters wrong at most. *)
let hint name candidates =
  let name' = String.lowercase_ascii name in
  let best =
    List.fold_left
      (fun best c ->
        let d = distance name' (String.lowercase_ascii c) in
        match best with Some (_, d') when d' <= d -> best | _ -> Some (c, d))
      None
      (List.sort_uniq compare candidates)
  in
  match best with
  | Some (c, d) when d <= max 1 (String.length name / 3) && c <> name ->
      Printf.sprintf " (did you mean %s?)" c
  | _ -> ""
