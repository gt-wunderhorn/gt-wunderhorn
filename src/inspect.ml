module JP = Sawja_pack.JProgram
module JB = Javalib_pack.JBasics

let to_clauses g =
  g

  (* Transform the edges from conjunctions to horn clauses by bringing the
     pre and post conditions in from the nodes. *)
  |> Exprs_to_clauses.translate

  (* Simplify certain clauses by substituting simple equality statements. *)
  |> Simplify.remove_simple_equalities


let inspect special_graph_formulation classpath class_name =
  let cn  = JB.make_cn class_name in
  let cms = JB.make_cms cn JP.main_signature in
  let proc_id = ref (0) in
  let parse = Parse.parse proc_id classpath cn in

  (* Create an IR procedure representing the entire program starting
     from the entrypoint. *)
  Jbir_to_ir.mk_proc parse cms

  (* The IR procedure can be converted into a graph which shows the control
     flow of the program. The edges of the graph are lists of instructions and
     the nodes are program locations.*)
  |> Ir_to_graph.translate special_graph_formulation

  (* Perform some graph simplifications to reduce the number of edges. *)
  |> Simplify.concatenate_consecutive_paths
  |> Simplify.remove_empty_paths
  |> Simplify.remove_non_asserting_nodes

  (* Annotate each program node with the variables relevant across that node's
     program location. *)
  |> Variable_analysis.annotate_nodes

  (* Transform the graph by:
     1) Changing each edge by converting a list of instructions into a single
        conjunction.
     2) Aliasing all appropriate variables in the nodes. *)
  |> Path_to_expr.translate

  (* Remove any trivial edges (those which are simply `True`). *)
  |> Simplify.remove_empty_exprs

let print classpath class_name =
  inspect Special.no_special classpath class_name
  |> to_clauses
  |> Graph.edges
  |> Print_clauses.print
  |> Printf.printf "%s\n"

let run classpath class_name =
  inspect Special.no_special classpath class_name
  |> to_clauses
  |> Variable_analysis.unannotate_nodes
  |> Run_clauses.run
