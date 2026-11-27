(* Typed table relations for relation-aware joins and batched eager loading. *)

open Table

type ('parent, 'child) has_many =
  | Has_many : {
      parent_key : ('parent, 'k) col;
      child : 'child table;
      child_key : ('child, 'k) col;
    }
      -> ('parent, 'child) has_many
  | Has_many_opt : {
      parent_key : ('parent, 'k) col;
      child : 'child table;
      child_key : ('child, 'k option) col;
    }
      -> ('parent, 'child) has_many

type ('parent, 'child) has_one =
  | Has_one : {
      parent_key : ('parent, 'k) col;
      child : 'child table;
      child_key : ('child, 'k) col;
    }
      -> ('parent, 'child) has_one
  | Has_one_opt : {
      parent_key : ('parent, 'k) col;
      child : 'child table;
      child_key : ('child, 'k option) col;
    }
      -> ('parent, 'child) has_one

type ('child, 'parent) belongs_to =
  | Belongs_to : {
      child_key : ('child, 'k) col;
      parent : 'parent table;
      parent_key : ('parent, 'k) col;
    }
      -> ('child, 'parent) belongs_to
  | Belongs_to_opt : {
      child_key : ('child, 'k option) col;
      parent : 'parent table;
      parent_key : ('parent, 'k) col;
    }
      -> ('child, 'parent) belongs_to

type ('source, 'target) many_to_many =
  | Many_to_many : {
      source_key : ('source, 'sk) col;
      through : 'through table;
      through_source_key : ('through, 'sk) col;
      through_target_key : ('through, 'tk) col;
      target : 'target table;
      target_key : ('target, 'tk) col;
    }
      -> ('source, 'target) many_to_many

let has_many ~parent_key ~child ~child_key =
  base "Qb.Rel.has_many" child;
  Has_many { parent_key; child; child_key }

let has_many_opt ~parent_key ~child ~child_key =
  base "Qb.Rel.has_many_opt" child;
  Has_many_opt { parent_key; child; child_key }

let has_one ~parent_key ~child ~child_key =
  base "Qb.Rel.has_one" child;
  Has_one { parent_key; child; child_key }

let has_one_opt ~parent_key ~child ~child_key =
  base "Qb.Rel.has_one_opt" child;
  Has_one_opt { parent_key; child; child_key }

let belongs_to ~child_key ~parent ~parent_key =
  base "Qb.Rel.belongs_to" parent;
  Belongs_to { child_key; parent; parent_key }

let belongs_to_opt ~child_key ~parent ~parent_key =
  base "Qb.Rel.belongs_to_opt" parent;
  Belongs_to_opt { child_key; parent; parent_key }

let many_to_many ~source_key ~through ~through_source_key ~through_target_key
    ~target ~target_key =
  base "Qb.Rel.many_to_many (through)" through;
  base "Qb.Rel.many_to_many (target)" target;
  Many_to_many
    {
      source_key;
      through;
      through_source_key;
      through_target_key;
      target;
      target_key;
    }

(* Flatten a parent-with-children list so another batched loader can run across
   all children in a single query, then reattach the enriched children back
   onto their parents in O(N) time. *)
let flatten pairs =
  let lengths = List.map (fun (_, cs) -> List.length cs) pairs in
  let flat = List.concat_map Stdlib.snd pairs in
  let take n xs =
    let rec go acc k = function
      | rest when k <= 0 -> (List.rev acc, rest)
      | [] ->
          invalid_arg "Qb.Rel.flatten: reattach list is shorter than expected"
      | x :: rest -> go (x :: acc) (k - 1) rest
    in
    go [] n xs
  in
  let reattach enriched =
    let rec go acc remaining ps ls =
      match (ps, ls) with
      | [], [] ->
          if remaining <> [] then
            invalid_arg "Qb.Rel.flatten: reattach list is longer than expected";
          List.rev acc
      | (p, _) :: ps, n :: ls ->
          let slice, rest = take n remaining in
          go ((p, slice) :: acc) rest ps ls
      | _ -> assert false
    in
    go [] enriched pairs lengths
  in
  (flat, reattach)

let flatten_opt pairs =
  let flat = List.filter_map Stdlib.snd pairs in
  let reattach enriched =
    let rec go acc remaining = function
      | [] ->
          if remaining <> [] then
            invalid_arg
              "Qb.Rel.flatten_opt: reattach list is longer than expected";
          List.rev acc
      | (p, None) :: rest -> go ((p, None) :: acc) remaining rest
      | (p, Some _) :: rest -> (
          match remaining with
          | [] ->
              invalid_arg
                "Qb.Rel.flatten_opt: reattach list is shorter than expected"
          | x :: rem -> go ((p, Some x) :: acc) rem rest)
    in
    go [] enriched pairs
  in
  (flat, reattach)
