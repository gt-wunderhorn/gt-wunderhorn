module PG = Program_graph
module G = Graph
module E = Expr
module LS = Lang_state

type bb = int
module BB_map = Map.Make(struct type t = bb;; let compare = compare end)

type context =
  { graph : (PG.lbl, E.t) G.t
  ; mapping : E.t BB_map.t
  }

let nil_location = -1
let locations m = BB_map.bindings m |> List.map fst
let location_name = string_of_int
let location_eq = (=)

let predecessors g = G.parents g

let ts = ("TS", E.Int)

let store_timestamps (this, next, i) = match i with
  | Ir.ArrAssign (arr, idx, e) ->
    let src = ("wsrc", E.sort_of e) in

    let body = [ LS.update_arr arr idx (E.Var ts)
               ; ts, E.mk_add (E.Var ts) (E.Int_lit 1)
               ; src, e
               ] in

    Some (G.singleton (this, next, PG.Body (E.True, body)))

  | _ -> None

let derive classpath class_name =
  let graph = Inspect.inspect store_timestamps classpath class_name in
  let exprs = G.edges graph in

  let add_to_mapping (m, k) v = (BB_map.add k v m, k+1) in

  let mapping = fst (List.fold_left add_to_mapping (BB_map.empty, 0) exprs) in

  let context =
    { graph
    ; mapping
    } in

  graph
  |> Graph.edges
  |> Print_clauses.print
  |> Printf.printf "%s\n"
