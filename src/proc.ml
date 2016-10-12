module L = Lang
module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

module V_map = Map.Make(struct type t = J.var;; let compare = compare end)

type t =
  { id      : string
  ; params  : (JB.value_type * J.var) list
  ; content : J.instr list
  ; mutable v_count : int
  ; mutable assignments : L.var V_map.t
  }

let rec sort = function
  | JB.TBasic t -> (match t with
      | `Bool   -> L.Bool
      | `Byte   -> assert false (* TODO *)
      | `Char   -> assert false (* TODO *)
      | `Double -> L.Real
      | `Float  -> assert false (* TODO *)
      | `Int    -> L.Int
      | `Long   -> L.Int
      | `Short  -> L.Int)
  | JB.TObject t -> L.Int

let var st v s =
  if not (V_map.mem v st.assignments)
  then
    let new_name = "v_" ^ st.id ^ string_of_int st.v_count in
    st.v_count <- st.v_count + 1;
    st.assignments <- V_map.add v (new_name, s) st.assignments;
    ;
    V_map.find v st.assignments

