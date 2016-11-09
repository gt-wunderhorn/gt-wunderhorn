module PG = Program_graph
module G = Graph
module E = Expr
module FD = Z3.FuncDecl

module F = Field

open Z3_type

type zexpr = Z3.Expr.expr
type bb = PG.lbl

type shape =
  { label : int
  ; predecessors : int list
  ; s1 : Z3.Sort.sort
  ; s2 : Z3.Sort.sort
  ; t1 : FD.func_decl list
  ; t2 : FD.func_decl list
  ; reads  : ((zexpr -> zexpr) F.field_desc) list
  ; writes : ((zexpr -> zexpr) F.field_desc) list
  ; transition : zexpr -> zexpr -> zexpr
  }

let field_eq = (=)

let label_of_node (l, _, _) = l
let sort_of_node (_, s, _) = s
let type_of_node (_, _, t) = t

let nil_location = -2
let locations graph =
  G.nodes graph
  |> List.sort_uniq (fun n1 n2 -> compare (label_of_node n1) (label_of_node n2))
  |> List.map label_of_node

let location_name = string_of_int
let location_eq = (=)

let nil_result_sort,nilresults = mk_enum "NilResult" ["nilresult"]
let nilresult = List.hd nilresults

let result_sort graph n =
  if n = nil_location then nil_result_sort
  else
    graph
    |> G.nodes
    |> List.filter (fun node -> label_of_node node =  n)
    |> List.hd
    |> sort_of_node

let result_type graph n =
  graph
  |> G.nodes
  |> List.filter (fun node -> label_of_node node =  n)
  |> List.hd
  |> type_of_node

let predecessors shs = List.map (fun sh -> sh.predecessors) shs

let reads shs  = List.map (fun sh -> sh.reads) shs
let writes shs = List.map (fun sh -> sh.writes) shs
let transitions shs = List.map (fun sh -> sh.transition) shs

let map_map f shs = List.map (List.map f) shs

let write_times shs = map_map (fun f -> f.F.time) (writes shs)
let read_times  shs = map_map (fun f -> f.F.time) (reads shs)

let write_srcs shs = map_map (fun f -> f.F.var) (writes shs)
let read_dsts  shs = map_map (fun f -> f.F.var) (reads shs)

let write_fields shs = map_map (fun f -> f.F.name) (writes shs)
let read_fields  shs = map_map (fun f -> f.F.name) (reads shs)

let nwrites shs : int list = List.map List.length (write_fields shs)
let nreads  shs : int list = List.map List.length (read_fields shs)

(* let write_times shs = *)
(*   shs.writes |> List.map (fun (lbl, _, _, time) -> *)
(*       assign time (result_type shs lbl)) *)

(* let write_sources shs = *)
(*   shs.writes |> List.map (fun (lbl, _, src, _) -> *)
(*       assign src (result_type shs lbl)) *)

(* let nwrites shs lbl = *)
(*   writes_for shs lbl |> List.length *)
