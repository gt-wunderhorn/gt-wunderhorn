module PG = Program_graph
module G = Graph
module E = Expr
module FD = Z3.FuncDecl

module F = Field

open Context
open Z3_type

type bb = int
type context = t list * ((Z3.Expr.expr -> Z3.Expr.expr) * int)
type field = F.field

let nil_result_sort,nilresults = mk_enum "NilResult" ["nilresult"]

let all_predecessors c = List.map (fun sh -> sh.predecessors) c
let result_sorts c = List.map (fun sh -> sh.result_sort) c

let map_map f c = List.map (List.map f) c

let transitions c = List.map (fun sh -> sh.transition) c

let write_fields cs = List.map (fun c -> List.map (fun w -> w.F.name) c.writes) cs
let read_fields  cs = List.map (fun c -> List.map (fun w -> w.F.name) c.reads) cs
let write_times  cs = List.map (fun c -> List.map (fun w -> w.F.time) c.writes) cs
let read_times   cs = List.map (fun c -> List.map (fun w -> w.F.time) c.reads) cs
let write_srcs   cs = List.map (fun c -> List.map (fun w -> w.F.var) c.writes) cs
let read_dsts    cs = List.map (fun c -> List.map (fun w -> w.F.var) c.reads) cs

let nwrites c = List.map List.length (write_fields c)
let nreads  c = List.map List.length (read_fields c)

let locations (c, _) = List.map (fun sh -> sh.label) c

let nil_location _ = -2
let initial_location _ = 0

let location_name = string_of_int
let location_eq = (=)

let predecessors (c, _) bb = List.nth (all_predecessors c) bb
let result_sort (c, _) bb = List.nth (result_sorts c) bb
let nilresult _ = List.hd nilresults

let nwrites (c, _) bb = List.nth (nwrites c) bb
let nreads (c, _) bb  = List.nth (nreads c) bb

let field_eq = (=)

let write_field (c, _) bb n = List.nth (List.nth (write_fields c) bb) n
let read_field  (c, _) bb n = List.nth (List.nth (read_fields c) bb) n
let write_time  (c, _) bb n = List.nth (List.nth (write_times c) bb) n
let write_src   (c, _) bb n = List.nth (List.nth (write_srcs  c) bb) n
let read_time   (c, _) bb n = List.nth (List.nth (read_times c) bb) n
let read_dst    (c, _) bb n = List.nth (List.nth (read_dsts  c) bb) n

let transition (c, _) bb = List.nth (transitions c) bb

let final_location (_, (a, l)) = l
let assertion (_, (a, l)) = a

let derive = Shape.derive
