module L = Lang
module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

module V_map = Map.Make(struct type t = J.var;; let compare = compare end)

type t =
  { id       : string
  ; params   : (JB.value_type * J.var) list
  ; content  : J.instr list
  ; ret_type : JB.value_type option
  ; sign     : JB.method_signature
  ; cl_name  : JB.class_name
  ; mutable v_count : int
  ; mutable assignments : string V_map.t
  }

let var st v =
  if not (V_map.mem v st.assignments)
  then
    let new_name = "v_" ^ st.id ^ string_of_int st.v_count in
    st.v_count <- st.v_count + 1;
    st.assignments <- V_map.add v new_name st.assignments;
    new_name
  else
    V_map.find v st.assignments

