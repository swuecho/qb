(* Qb: a typed, Drizzle-style query builder on Caqti.
   See qb.mli for the interface and documentation. *)

include Ty
include Table
module Expr = Expr
module Raw = Expr.Raw
module Rel = Rel
include Query
module Query = Multi
module Q = Q

type 'a stmt = 'a Stmt.stmt

module Stmt = Stmt

let stmt_name = Stmt.stmt_name
let stmt_sql = Stmt.stmt_sql

type violation = Changeset.violation

let violation = Changeset.violation
let is_unique_violation = Changeset.is_unique_violation

module Changeset = Changeset

let src = Cache.src

include Runner
