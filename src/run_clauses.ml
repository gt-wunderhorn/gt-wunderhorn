module P = Z3.Params
module S = Z3.Symbol
module FP = Z3.Fixedpoint

module G = Graph
module PG = Program_graph
module Set = Core.Std.Set.Poly

let query fp (q, at) =
  match FP.query_r fp [q] with
  | Z3.Solver.SATISFIABLE   -> Printf.printf "%s unsafe\n" (PG.show_assert_type at)
  | Z3.Solver.UNSATISFIABLE -> Printf.printf "%s safe\n" (PG.show_assert_type at)
  | _ -> Printf.printf "unknown\n"

let run graph =
  let module CZ = Clauses_to_z3 in

  let z3_state = CZ.translate graph in
  let c = z3_state.CZ.context in

  let fp = FP.mk_fixedpoint c in
  let r  = P.mk_params c in
  P.add_int r (S.mk_string c "fixedpoint.timeout") 120000;
  P.add_symbol r (S.mk_string c "fixedpoint.engine") (S.mk_string c "duality");
  FP.set_parameters fp r;

  List.iter (fun v -> FP.register_variable fp v) z3_state.CZ.vars;
  G.iter_edges (fun clause -> FP.add_rule fp clause None) z3_state.CZ.graph;
  List.iter (query fp) z3_state.CZ.queries
