(* A schema built by replaying CREATE / ALTER / DROP TABLE statements. *)

module J = Yojson.Safe
module U = Yojson.Safe.Util

type pg_type = { name : string; mods : int list; array_dims : int }

type column = {
  name : string;
  ty : pg_type;
  not_null : bool;
  generated : bool;
  has_default : bool;
}

type table = {
  schema : string option;
  name : string;
  columns : column list;
  primary_key : string list;
}

type t = { tables : table list; warnings : string list }

let empty = { tables = []; warnings = [] }

let type_sql (ty : pg_type) =
  let base =
    match ty.name with
    | "int2" -> "SMALLINT"
    | "int4" -> "INTEGER"
    | "int8" -> "BIGINT"
    | "float4" -> "REAL"
    | "float8" -> "DOUBLE PRECISION"
    | "bool" -> "BOOLEAN"
    | "bpchar" -> "CHAR"
    | "timestamptz" -> "TIMESTAMP WITH TIME ZONE"
    | "timetz" -> "TIME WITH TIME ZONE"
    | n -> String.uppercase_ascii n
  in
  let mods =
    match ty.mods with
    | [] -> ""
    | ms -> "(" ^ String.concat "," (List.map string_of_int ms) ^ ")"
  in
  let dims = String.concat "" (List.init ty.array_dims (fun _ -> "[]")) in
  base ^ mods ^ dims

(* ---------- reading the JSON tree ---------- *)

(* Fields left at their default value are omitted from libpg_query's JSON. *)
let field k j = match j with `Assoc kv -> List.assoc_opt k kv | _ -> None
let str k j = match field k j with Some (`String s) -> Some s | _ -> None
let bool k j = match field k j with Some (`Bool b) -> b | _ -> false
let list k j = match field k j with Some (`List l) -> l | _ -> []

(* A node is a one-key object naming its kind: {"ColumnDef": {...}}. *)
let node j =
  match j with `Assoc [ (kind, body) ] -> Some (kind, body) | _ -> None

let sval j = match node j with Some ("String", s) -> str "sval" s | _ -> None
let names j = List.filter_map sval j

let int_const j =
  match node j with
  | Some ("A_Const", c) -> (
      match field "ival" c with
      | Some iv ->
          Some (match field "ival" iv with Some (`Int i) -> i | _ -> 0)
      | None -> None)
  | Some ("Integer", i) ->
      Some (match field "ival" i with Some (`Int i) -> i | _ -> 0)
  | _ -> None

let pg_type tn : pg_type =
  let name =
    match names (list "names" tn) with
    | [ "pg_catalog"; n ] -> n
    | ns -> String.concat "." ns
  in
  {
    name;
    mods = List.filter_map int_const (list "typmods" tn);
    array_dims = List.length (list "arrayBounds" tn);
  }

(* serial types are an integer plus a sequence default. *)
let serial (ty : pg_type) =
  match ty.name with
  | "serial" | "serial4" -> Some { ty with name = "int4" }
  | "bigserial" | "serial8" -> Some { ty with name = "int8" }
  | "smallserial" | "serial2" -> Some { ty with name = "int2" }
  | _ -> None

let constraints j =
  List.filter_map
    (fun c -> match node c with Some ("Constraint", c) -> Some c | _ -> None)
    (list "constraints" j)

let contype c = Option.value (str "contype" c) ~default:""

(* DEFAULT nextval('seq'): how serial columns look once pg_dump writes them
   out, so the database fills them in like an identity. *)
let is_nextval expr =
  match node expr with
  | Some ("FuncCall", f) -> (
      match List.rev (names (list "funcname" f)) with
      | "nextval" :: _ -> true
      | _ -> false)
  | _ -> false

(* A column, and whether it was declared PRIMARY KEY itself. *)
let column_def cd =
  let ty = pg_type (Option.value (field "typeName" cd) ~default:`Null) in
  let ty, serial =
    match serial ty with Some t -> (t, true) | None -> (ty, false)
  in
  let cs = List.map contype (constraints cd) in
  let has c = List.mem c cs in
  let nextval =
    List.exists
      (fun c ->
        contype c = "CONSTR_DEFAULT"
        && is_nextval (Option.value (field "raw_expr" c) ~default:`Null))
      (constraints cd)
  in
  let pk = has "CONSTR_PRIMARY" in
  ( {
      name = Option.value (str "colname" cd) ~default:"";
      ty;
      not_null =
        pk || serial || has "CONSTR_IDENTITY" || has "CONSTR_NOTNULL"
        || bool "is_not_null" cd;
      generated = serial || nextval || has "CONSTR_IDENTITY";
      has_default = has "CONSTR_DEFAULT" || has "CONSTR_GENERATED";
    },
    pk )

let relation j =
  let r = Option.value (field "relation" j) ~default:`Null in
  (str "schemaname" r, Option.value (str "relname" r) ~default:"")

(* ---------- applying statements ---------- *)

let same_table (schema, name) (t : table) =
  t.name = name
  && Option.value schema ~default:"public"
     = Option.value t.schema ~default:"public"

let qualified (schema, name) =
  match schema with Some s -> s ^ "." ^ name | None -> name

let set_pk keys (t : table) =
  let keys =
    List.fold_left
      (fun ks k -> if List.mem k ks then ks else ks @ [ k ])
      [] keys
  in
  let columns =
    List.map
      (fun (c : column) ->
        if List.mem c.name keys then { c with not_null = true } else c)
      t.columns
  in
  { t with columns; primary_key = keys }

let create st body =
  let rel = relation body in
  let exists = List.exists (same_table rel) st.tables in
  if exists && bool "if_not_exists" body then st
  else
    let st =
      if not exists then st
      else
        {
          tables = List.filter (fun t -> not (same_table rel t)) st.tables;
          warnings =
            st.warnings
            @ [ "CREATE TABLE " ^ qualified rel ^ ": already exists, replaced" ];
        }
    in
    let warnings = ref [] in
    let cols, pk =
      List.fold_left
        (fun (cols, pk) elt ->
          match node elt with
          | Some ("ColumnDef", cd) ->
              let c, is_pk = column_def cd in
              (cols @ [ c ], if is_pk then pk @ [ c.name ] else pk)
          | Some ("Constraint", c) when contype c = "CONSTR_PRIMARY" ->
              (cols, pk @ names (list "keys" c))
          | Some ("Constraint", _) -> (cols, pk)
          | Some (kind, _) ->
              warnings :=
                !warnings
                @ [
                    Printf.sprintf "CREATE TABLE %s: %s ignored" (qualified rel)
                      kind;
                  ];
              (cols, pk)
          | None -> (cols, pk))
        ([], []) (list "tableElts" body)
    in
    let t =
      { schema = fst rel; name = snd rel; columns = cols; primary_key = [] }
    in
    let t = if pk = [] then t else set_pk pk t in
    { tables = st.tables @ [ t ]; warnings = st.warnings @ !warnings }

let update_table st rel f =
  match List.find_opt (same_table rel) st.tables with
  | None ->
      { st with warnings = st.warnings @ [ "unknown table " ^ qualified rel ] }
  | Some _ ->
      let warnings = ref [] in
      let warn m = warnings := !warnings @ [ qualified rel ^ ": " ^ m ] in
      let tables =
        List.map (fun t -> if same_table rel t then f warn t else t) st.tables
      in
      { tables; warnings = st.warnings @ !warnings }

let map_column name (f : column -> column) warn (t : table) =
  if List.exists (fun (c : column) -> c.name = name) t.columns then
    {
      t with
      columns =
        List.map
          (fun (c : column) -> if c.name = name then f c else c)
          t.columns;
    }
  else (
    warn ("unknown column " ^ name);
    t)

let alter_cmd warn (t : table) cmd =
  let name = Option.value (str "name" cmd) ~default:"" in
  let def = Option.value (field "def" cmd) ~default:`Null in
  match str "subtype" cmd with
  | Some "AT_AddColumn" -> (
      match node def with
      | Some ("ColumnDef", cd) ->
          let c, is_pk = column_def cd in
          if List.exists (fun (x : column) -> x.name = c.name) t.columns then (
            if not (bool "missing_ok" cmd) then
              warn ("column " ^ c.name ^ " already exists");
            t)
          else
            let t = { t with columns = t.columns @ [ c ] } in
            if is_pk then set_pk [ c.name ] t else t
      | _ -> t)
  | Some "AT_DropColumn" ->
      if List.exists (fun (c : column) -> c.name = name) t.columns then
        {
          t with
          columns = List.filter (fun (c : column) -> c.name <> name) t.columns;
          primary_key = List.filter (( <> ) name) t.primary_key;
        }
      else (
        if not (bool "missing_ok" cmd) then warn ("unknown column " ^ name);
        t)
  | Some "AT_SetNotNull" ->
      map_column name (fun (c : column) -> { c with not_null = true }) warn t
  | Some "AT_DropNotNull" ->
      map_column name (fun (c : column) -> { c with not_null = false }) warn t
  | Some "AT_AlterColumnType" -> (
      match node def with
      | Some ("ColumnDef", cd) ->
          let ty =
            pg_type (Option.value (field "typeName" cd) ~default:`Null)
          in
          map_column name (fun (c : column) -> { c with ty }) warn t
      | _ -> t)
  | Some "AT_ColumnDefault" ->
      map_column name
        (fun (c : column) ->
          {
            c with
            has_default = def <> `Null;
            generated = (c.generated && def <> `Null) || is_nextval def;
          })
        warn t
  | Some "AT_AddIdentity" ->
      map_column name (fun (c : column) -> { c with generated = true }) warn t
  | Some "AT_DropIdentity" ->
      map_column name (fun (c : column) -> { c with generated = false }) warn t
  | Some "AT_AddConstraint" -> (
      match node def with
      | Some ("Constraint", c) when contype c = "CONSTR_PRIMARY" ->
          set_pk (names (list "keys" c)) t
      | _ -> t)
  | Some "AT_DropConstraint" ->
      (* Constraint names aren't tracked, so a dropped primary key can't be
         told apart from any other constraint. Postgres names primary keys
         <table>_pkey; warn about those, since the schema may now be wrong,
         and let foreign keys and unique constraints go quietly. *)
      let n = String.length name in
      if n >= 5 && String.sub name (n - 5) 5 = "_pkey" then
        warn ("DROP CONSTRAINT " ^ name ^ " not tracked");
      t
  | Some
      ( "AT_EnableRowSecurity" | "AT_DisableRowSecurity" | "AT_ForceRowSecurity"
      | "AT_NoForceRowSecurity" | "AT_ReplicaIdentity" | "AT_ClusterOn"
      | "AT_DropCluster" | "AT_SetRelOptions" | "AT_ResetRelOptions"
      | "AT_ValidateConstraint" | "AT_SetStatistics" | "AT_SetStorage"
      | "AT_SetCompression" | "AT_EnableTrig" | "AT_DisableTrig"
      | "AT_EnableAlwaysTrig" | "AT_EnableReplicaTrig" | "AT_EnableTrigAll"
      | "AT_DisableTrigAll" | "AT_EnableTrigUser" | "AT_DisableTrigUser"
      | "AT_EnableRule" | "AT_DisableRule" | "AT_EnableAlwaysRule"
      | "AT_EnableReplicaRule" | "AT_ChangeOwner" | "AT_SetTableSpace"
      | "AT_SetAccessMethod" | "AT_AttachPartition" | "AT_DetachPartition"
      | "AT_SetLogged" | "AT_SetUnLogged" | "AT_AlterConstraint" ) ->
      (* Nothing here changes a column. *)
      t
  | Some s ->
      warn ("ALTER TABLE " ^ s ^ " ignored");
      t
  | None -> t

let apply st stmt =
  match node stmt with
  | Some ("CreateStmt", body) -> create st body
  | Some ("AlterTableStmt", body) when str "objtype" body = Some "OBJECT_TABLE"
    ->
      let cmds =
        List.filter_map
          (fun c ->
            match node c with Some ("AlterTableCmd", c) -> Some c | _ -> None)
          (list "cmds" body)
      in
      update_table st (relation body) (fun warn t ->
          List.fold_left (alter_cmd warn) t cmds)
  | Some ("RenameStmt", body) -> (
      let rel = relation body in
      let newname = Option.value (str "newname" body) ~default:"" in
      match str "renameType" body with
      | Some "OBJECT_TABLE" ->
          update_table st rel (fun _ t -> { t with name = newname })
      | Some "OBJECT_COLUMN" ->
          let old = Option.value (str "subname" body) ~default:"" in
          update_table st rel (fun warn t ->
              let t =
                map_column old
                  (fun (c : column) -> { c with name = newname })
                  warn t
              in
              {
                t with
                primary_key =
                  List.map
                    (fun k -> if k = old then newname else k)
                    t.primary_key;
              })
      | _ -> st)
  | Some ("DropStmt", body) when str "removeType" body = Some "OBJECT_TABLE" ->
      List.fold_left
        (fun st obj ->
          let rel =
            match obj with
            | `List _ -> names (U.to_list obj)
            | _ -> (
                match node obj with
                | Some ("List", l) -> names (list "items" l)
                | _ -> [])
          in
          let rel =
            match rel with
            | [ s; n ] -> (Some s, n)
            | [ n ] -> (None, n)
            | _ -> (None, "")
          in
          if List.exists (same_table rel) st.tables then
            {
              st with
              tables = List.filter (fun t -> not (same_table rel t)) st.tables;
            }
          else if bool "missing_ok" body then st
          else
            {
              st with
              warnings =
                st.warnings @ [ "DROP TABLE: unknown table " ^ qualified rel ];
            })
        st (list "objects" body)
  | _ -> st

let apply_sql ?(init = empty) sql =
  Result.map (List.fold_left apply init) (Pg.statements sql)

let of_sql sql = apply_sql sql
let find ?schema t name = List.find_opt (same_table (schema, name)) t.tables
