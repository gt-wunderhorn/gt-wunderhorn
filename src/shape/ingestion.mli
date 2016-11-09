type context
type bb

val locations : context -> bb list
val nil_location : context -> bb
val initial_location : context -> bb
val final_location : context -> bb
val location_name : context -> bb -> string
val location_eq : context -> bb -> bb -> bool

val predecessors : context -> bb -> bb list

val result_sort : context -> bb -> Z3.Sort.sort
val nilresult : context -> Z3.Expr.expr

val nwrites : context -> bb -> int
val nreads : context -> bb -> int

type field
val field_eq : field -> field -> bool
val write_field : context -> bb -> int -> field
val read_field : context -> bb -> int -> field

val write_time : context -> bb -> int -> Z3.Expr.expr -> Z3.Expr.expr
val write_src : context -> bb -> int -> Z3.Expr.expr -> Z3.Expr.expr
val read_time : context -> bb -> int -> Z3.Expr.expr -> Z3.Expr.expr
val read_dst : context -> bb -> int -> Z3.Expr.expr -> Z3.Expr.expr

val transition : context -> bb -> bb -> Z3.Expr.expr -> Z3.Expr.expr -> Z3.Expr.expr

val assertion : context -> Z3.Expr.expr -> Z3.Expr.expr
