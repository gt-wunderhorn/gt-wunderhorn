module G = Graph
module L = Lang

let collapse g =
  let collapse' (p, c, e) =
    L.mk_and (Relation
  in
  List.map collapse' (G.conns g)
