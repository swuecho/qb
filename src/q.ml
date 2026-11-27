(* One-stop local-open DSL for multi-table queries: combines Qb.Query,
   Qb.Expr.Ops, and Qb.Expr functions so queries need only [Qb.Q.( ... )]. *)

include Multi
include Expr.Ops

let any = Expr.any
let lit = Expr.lit
let int = Expr.int
let int64 = Expr.int64
let float = Expr.float
let text = Expr.text
let bool = Expr.bool
let ptime = Expr.ptime
let date = Expr.date
let uuid = Expr.uuid
let json = Expr.json
let null = Expr.null
let some = Expr.some
let lower = Expr.lower
let upper = Expr.upper
let trim = Expr.trim
let length = Expr.length
let concat = Expr.concat
let coalesce = Expr.coalesce
let coalesce_opt = Expr.coalesce_opt
let nullif = Expr.nullif
let case_when = Expr.case_when
let case_when_opt = Expr.case_when_opt
let now = Expr.now
let cast = Expr.cast
let count_all = Expr.count_all
let count = Expr.count
let count_distinct = Expr.count_distinct
let sum_int = Expr.sum_int
let sum_int64 = Expr.sum_int64
let sum_float = Expr.sum_float
let avg = Expr.avg
let min = Expr.min
let max = Expr.max
let min_opt = Expr.min_opt
let max_opt = Expr.max_opt
let filter = Expr.filter
let over = Expr.over
let row_number = Expr.row_number
let rank = Expr.rank
let dense_rank = Expr.dense_rank
let lag = Expr.lag
let lag_default = Expr.lag_default
let lead = Expr.lead
let lead_default = Expr.lead_default
let first_value = Expr.first_value
let last_value = Expr.last_value

module Keyset = Expr.Keyset
module Json = Expr.Json
module Pg = Expr.Pg
module Raw = Expr.Raw
