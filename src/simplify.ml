module L = Lang

(** If a variable `v` is assigned to an expression `e`
    `v := e`
    and `v` is only used once after the assignment
    replace the instance of `v with `e` and  remove the assignment. *)
let remove_redundant_assignments instrs =
  let remove = () in
  let remove_redundant (v, e) =
    if count_var_usage v = 0
    then
      remove (Assign (v, e))
    else if count_var_usage v = 1
    then
      remove (Assign (v, e));
      replace v e (find_user v)
  in

  find_assignments instrs
  |> remove_redundant
