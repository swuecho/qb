(* Columns, selections, tables, DDL, and the SQL rendering buffer. *)

open Ty

(* Which side of a join, from the outermost join inwards. *)
type side = L | R

(* [get] reads the field back out of a row; insert uses it. [path] is empty
   for a table's own column, and says which table of a join it belongs to
   for a column lifted with [fst] / [snd]. *)
type ('r, 'a) col = {
  name : string;
  ty : 'a ty;
  get : 'r -> 'a;
  primary_key : bool;
  auto_increment : bool;
  path : side list;
}

let column ?(primary_key = false) ?(auto_increment = false) name ty get =
  { name; ty; get; primary_key; auto_increment; path = [] }

(* A column of one table of a join, moved onto the join's pair rows. *)
type ('r, 's) lift = { lift : 'a. ('r, 'a) col -> ('s, 'a) col }

let lift side get =
  {
    lift =
      (fun c ->
        { c with path = side :: c.path; get = (fun r -> c.get (get r)) });
  }

let fst c = (lift L Stdlib.fst).lift c
let snd c = (lift R Stdlib.snd).lift c
let some c = { c with ty = nullable c.ty; get = (fun r -> Some (c.get r)) }

type 'r any_col = Any : ('r, _) col -> 'r any_col

(* ---------- rendering: SQL text + collected parameters ---------- *)

type param = Param : 'a Caqti_type.t * 'a -> param

type out = {
  dialect : dialect;
  buf : Buffer.t;
  mutable params : param list; (* reversed *)
  mutable prefix : side list;
      (* While rendering a nested join's ON: where that join sits. *)
  mutable table_qual : string option;
      (* While rendering ON CONFLICT ... DO UPDATE WHERE: qualify the target
         table's columns so they don't clash with EXCLUDED. *)
  mutable variadic : bool;
      (* The text depends on how long a list is (IN, several rows): there is
         no end of such statements, so they aren't kept prepared. *)
  ref_aliases : (int, string) Hashtbl.t;
      (* Maps scoped table handle ids to deterministic SQL aliases (t0, t1, ...). *)
  mutable next_ref : int;
}

let out dialect =
  {
    dialect;
    buf = Buffer.create 128;
    params = [];
    prefix = [];
    table_qual = None;
    variadic = false;
    ref_aliases = Hashtbl.create 8;
    next_ref = 0;
  }

let register_ref o id =
  match Hashtbl.find_opt o.ref_aliases id with
  | Some a -> a
  | None ->
      let a = "t" ^ string_of_int o.next_ref in
      o.next_ref <- o.next_ref + 1;
      Hashtbl.replace o.ref_aliases id a;
      a

let lookup_ref o id =
  match Hashtbl.find_opt o.ref_aliases id with
  | Some a -> a
  | None ->
      invalid_arg
        "Qb.Query: table handle used outside the query that introduced it"

let str o s = Buffer.add_string o.buf s
let name o n = str o (quote o.dialect n)

(* Each table of a join is aliased by its path: "l", "r", "lr", ... *)
let alias path =
  String.concat "" (List.map (function L -> "l" | R -> "r") path)

(* A column, qualified by its table's alias when it comes from a join, or by
   [table_qual] inside ON CONFLICT ... DO UPDATE WHERE. *)
let colname o c =
  match o.prefix @ c.path with
  | [] -> (
      match o.table_qual with
      | Some t ->
          name o t;
          str o ".";
          name o c.name
      | None -> name o c.name)
  | p ->
      name o (alias p);
      str o ".";
      name o c.name

let alias_colname o a c =
  name o a;
  str o ".";
  name o c.name

let param_ty o (ty : 'a ty) (v : 'a) =
  o.params <- Param (ty.caqti, v) :: o.params;
  str o "?"

let param o (c : (_, 'a) col) (v : 'a) = param_ty o c.ty v

let sep o sep f xs =
  List.iteri
    (fun i x ->
      if i > 0 then str o sep;
      f x)
    xs

let col_names o cols = sep o ", " (fun (Any c) -> name o c.name) cols
let qualified_names o cols = sep o ", " (fun (Any c) -> colname o c) cols

(* ---------- selection: an applicative over columns ---------- *)

(* A row read with list syntax: [select [ a; b ]] gives [Row.[ x; y ]]. *)
module Row = struct
  type _ t = [] : unit t | ( :: ) : 'a * 'b t -> ('a * 'b) t
end

(* [[]] and [::] live in [Sel], not at the top level, so [open Qb] does not
   shadow ordinary lists. OCaml still finds them from the expected type. *)
module Sel = struct
  type ('r, _) t =
    | [] : ('r, unit Row.t) t
    | ( :: ) : ('r, 'a) col * ('r, 'b Row.t) t -> ('r, ('a * 'b) Row.t) t
    | Expr : ('r, 'a) expr -> ('r, 'a) t

  and ('r, _) expr =
    | One : ('r, 'a) col -> ('r, 'a) expr
    | Map : ('a -> 'b) * ('r, 'a) t -> ('r, 'b) expr
    | Both : ('r, 'a) t * ('r, 'b) t -> ('r, 'a * 'b) expr
    | Opt : ('r, 'a) t -> ('r, 'a option) expr
  (* [None] when every column is NULL: the missing side of a left join. *)
end

type ('r, 'a) sel = ('r, 'a) Sel.t

let col c = Sel.Expr (One c)
let ( let+ ) s f = Sel.Expr (Map (f, s))
let ( and+ ) a b = Sel.Expr (Both (a, b))

(* The raw row Caqti decodes, how to turn it into ['a], and the columns. *)
type ('r, 'a) decoder =
  | Dec : 'x Caqti_type.t * ('x -> 'a) * 'r any_col list -> ('r, 'a) decoder

let rec decoder : type r a. (r, a) sel -> (r, a) decoder = function
  | [] -> Dec (Caqti_type.unit, (fun () -> Row.[]), List.[])
  | c :: rest -> (
      match decoder rest with
      | Dec (t, d, cs) ->
          Dec
            ( Caqti_type.t2 c.ty.caqti t,
              (fun (x, y) -> Row.(x :: d y)),
              List.cons (Any c) cs ))
  | Expr (One c) -> Dec (c.ty.caqti, Fun.id, List.[ Any c ])
  | Expr (Map (f, s)) -> (
      match decoder s with Dec (t, d, cs) -> Dec (t, (fun x -> f (d x)), cs))
  | Expr (Both (a, b)) -> (
      match (decoder a, decoder b) with
      | Dec (ta, da, ca), Dec (tb, db, cb) ->
          Dec (Caqti_type.t2 ta tb, (fun (x, y) -> (da x, db y)), ca @ cb))
  | Expr (Opt s) -> (
      match decoder s with
      | Dec (t, d, cs) -> Dec (Caqti_type.option t, Option.map d, cs))

(* [select []] would render [SELECT  FROM ...]. *)
let check_nonempty fn sel =
  match decoder sel with
  | Dec (_, _, List.[]) -> invalid_arg (fn ^ ": no columns")
  | Dec _ -> ()

let rec lift_sel : type r s a. (r, s) lift -> (r, a) sel -> (s, a) sel =
 fun l -> function
  | [] -> []
  | c :: rest -> l.lift c :: lift_sel l rest
  | Expr (One c) -> Expr (One (l.lift c))
  | Expr (Map (f, s)) -> Expr (Map (f, lift_sel l s))
  | Expr (Both (a, b)) -> Expr (Both (lift_sel l a, lift_sel l b))
  | Expr (Opt s) -> Expr (Opt (lift_sel l s))

(* ---------- tables ---------- *)

(* A join is a table too: its rows are pairs, its columns lifted ones, and
   it renders its FROM clause itself, given where it sits in an outer join. *)
type 'r table = {
  tschema : string option;
  tname : string;
  row : ('r, 'r) sel;
  cols : 'r any_col list;
  joined : (out -> side list -> unit) option;
}

let table ?schema tname row =
  match decoder row with
  | Dec (_, _, cols) -> { tschema = schema; tname; row; cols; joined = None }

(* "schema"."table", each part quoted on its own. *)
let table_name dialect t =
  match t.tschema with
  | None -> quote dialect t.tname
  | Some s -> quote dialect s ^ "." ^ quote dialect t.tname

let column_ddl dialect (Any c) =
  let auto =
    match (c.auto_increment, dialect) with
    | false, _ | true, Sqlite -> ""
    | true, Postgres -> " GENERATED BY DEFAULT AS IDENTITY"
    | true, Mysql -> " AUTO_INCREMENT"
  in
  Printf.sprintf "%s %s%s%s" (quote dialect c.name) (c.ty.ddl dialect) auto
    (if c.primary_key then " PRIMARY KEY"
     else if c.ty.nullable then ""
     else " NOT NULL")

let base fn t =
  if t.joined <> None then invalid_arg (fn ^ ": a join is not a table")

let create_table_sql ?(dialect = Postgres) t =
  base "Qb.create_table_sql" t;
  Printf.sprintf "CREATE TABLE %s (%s)" (table_name dialect t)
    (String.concat ", " (List.map (column_ddl dialect) t.cols))
