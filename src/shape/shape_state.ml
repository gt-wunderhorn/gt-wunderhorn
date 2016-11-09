module PG = Program_graph
module G = Graph
module E = Expr
module FD = Z3.FuncDecl

module F = Field

open Z3_type

type zexpr = Z3.Expr.expr
type bb = int
type field = F.field

type context =
  { label : bb
  ; predecessors : bb list
  ; result_sort : Z3.Sort.sort
  ; reads  : ((zexpr -> zexpr) F.field_desc) list
  ; writes : ((zexpr -> zexpr) F.field_desc) list
  ; transition : zexpr -> zexpr -> zexpr
  }

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

let locations c = List.map (fun sh -> sh.label) c

let nil_location _ = -2
let initial_location _ = 0

let location_name = string_of_int
let location_eq = (=)

let predecessors c bb = List.nth (all_predecessors c) bb
let result_sort c bb = List.nth (result_sorts c) bb
let nilresult _ = List.hd nilresults


let nwrites c bb = List.nth (nwrites c) bb
let nreads c bb  = List.nth (nreads c) bb

let field_eq = (=)
let write_field c bb = List.nth (write_fields c) bb
let read_field c bb  = List.nth (read_fields c) bb

let write_time c bb n = List.nth (List.nth (write_times c) bb) n
let write_src  c bb n = List.nth (List.nth (write_srcs  c) bb) n
let read_time  c bb n = List.nth (List.nth (read_times c) bb) n
let read_dst   c bb n = List.nth (List.nth (read_dsts  c) bb) n

let transition c bb = List.nth (transitions c) bb

let final_location c = assert false (* TODO *)
let assertion c = assert false (* TODO *)
(* val assertion : context -> Z3.Expr.expr -> Z3.Expr.expr *)
