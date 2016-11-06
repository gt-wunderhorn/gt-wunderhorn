module G = Graph
module E = Expr
module LS = Lang_state
module PG = Program_graph
module Set = Core.Std.Set.Poly

let path_uses_before_assigns p =
  let loop (a_set, u_set) (v, e) =
    (* Add the variables in e to the use set if they were not in the assignment set. *)
    let augment e =
      Set.union
        (Set.diff (E.vars e) a_set)
        u_set in

    (Set.add a_set v, augment e)
  in

  match p with
  | PG.Assert (e, _) -> E.vars e
  | PG.Body (e, assigns) ->
    Set.union
      (E.vars e)
      (snd (List.fold_left loop (Set.empty, Set.empty) assigns))

let path_assigns = function
  | PG.Assert _ -> Set.empty
  | PG.Body (e, assigns) -> List.map fst assigns |> Set.of_list

let nested_map f xs =
  List.map (List.map f) xs

let critical_vars g n =
  let coming = G.walks_to g n |> nested_map G.edge in
  let going = G.walks_from g n |> nested_map G.edge in

  ((if List.length going = 0 then
     (nested_map path_assigns coming |> List.concat |> Set.union_list)
   else
     Set.inter
       (nested_map path_assigns coming |> List.concat |> Set.union_list)
       (nested_map path_uses_before_assigns going |> List.concat|> Set.union_list))
  |> Set.elements) @ LS.mandatory_vars

let annotate_nodes g =
  let ann n = (n, critical_vars g n) in
  G.map_nodes ann g
