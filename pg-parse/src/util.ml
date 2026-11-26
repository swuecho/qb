(* Small helpers shared across qb-pg-parse modules. *)

(* Whether [sub] occurs anywhere in [s]. *)
let contains_sub ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  go 0

(* The first key that appears more than once in [xs], if any. *)
let rec first_dup_by key = function
  | [] -> None
  | x :: rest ->
      let k = key x in
      if List.exists (fun y -> key y = k) rest then Some k
      else first_dup_by key rest

let first_dup xs = first_dup_by Fun.id xs
