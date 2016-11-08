module PG = Program_graph
module G = Graph
module E = Expr
module FD = Z3.FuncDecl

open Z3_type

type zexpr = Z3.Expr.expr
type bb = PG.lbl
type field = string
type write = PG.lbl * field * E.var * E.var
type state =
  { graph  : (PG.lbl * Z3.Sort.sort * FD.func_decl list, zexpr -> zexpr -> zexpr) G.t
  ; writes : write list
  }

let field_eq = (=)

let label_of_node (l, _, _) = l
let sort_of_node (_, s, _) = s
let type_of_node (_, _, t) = t

let nil_location = -2
let locations st =
  G.nodes st.graph
  |> List.sort_uniq (fun n1 n2 -> compare (label_of_node n1) (label_of_node n2))
  |> List.map label_of_node

let location_name = string_of_int
let location_eq = (=)

let predecessors st n = List.map label_of_node (G.parents st.graph n)

let nil_result_sort,nilresults = mk_enum "NilResult" ["nilresult"]
let nilresult = List.hd nilresults

let result_sort st n =
  if n = nil_location then nil_result_sort
  else
    st.graph
    |> G.nodes
    |> List.filter (fun node -> label_of_node node =  n)
    |> List.hd
    |> sort_of_node

let result_type st n =
  st.graph
  |> G.nodes
  |> List.filter (fun node -> label_of_node node =  n)
  |> List.hd
  |> type_of_node

let transition st lbl1 lbl2 =
  st.graph |> G.conns
  |> List.filter (fun ((lbl1', _, _), (lbl2', _, _), e) ->
      lbl1 = lbl1' && lbl2 = lbl2')
  |> List.map (fun (_, _, e) -> e)

let writes_for st lbl =
  st.writes |> List.filter (fun (l, _, _, _) -> lbl = l)

let write_times st =
  st.writes |> List.map (fun (lbl, _, _, time) ->
      assign time (result_type st lbl))

let write_sources st =
  st.writes |> List.map (fun (lbl, _, src, _) ->
      assign src (result_type st lbl))

let nwrites st lbl =
  writes_for st lbl |> List.length
