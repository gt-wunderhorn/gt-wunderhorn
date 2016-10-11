module L = Lang
module J = Sawja_pack.JBir

module V_map = Map.Make(struct type t = L.var;; let compare = compare end)

type t =
  { id      : string
  ; params  : J.var list
  ; content : J.instr list
  ; mutable v_count : int
  ; mutable assignments : L.var V_map.t
  }

let var st v =
  if not (V_map.mem (J.var_name v) st.assignments)
  then
    let new_name = "v_" ^ st.id ^ string_of_int st.v_count in
    st.v_count <- st.v_count + 1;
    st.assignments <- V_map.add (J.var_name v) new_name st.assignments;
    ;
  V_map.find (J.var_name v) st.assignments

