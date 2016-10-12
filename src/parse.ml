module JL = Javalib_pack.Javalib
module JP = Sawja_pack.JProgram
module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

module JC = Sawja_pack.JControlFlow

module Cmm = JB.ClassMethodMap
module Mm = JB.MethodMap

type parse = { cms_lookup :
                 JB.class_method_signature ->
                 Proc.t

; virtual_lookup :
    J.virtual_call_kind ->
    JB.method_signature ->
    Proc.t list
             }
module Cms_map = Map.Make(
  struct type t = JB.class_method_signature;; let compare = compare end)

type method_map =
  JB.class_method_signature ->
  J.t

let parse id classpath cn =
  let (prta,instantiated_classes) =
    Sawja_pack.JRTA.parse_program classpath
      (JB.make_cms cn JP.main_signature) in

  let program = JP.map_program2
      (fun _ -> J.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[])
      (Some (fun code pp -> (J.pc_ir2bc code).(pp)))
      prta in

  let methods = program.JP.parsed_methods in

  let parse_method cm =
    let sign = cm.JL.cm_signature in
    id := !id + 1;
    match cm.JL.cm_implementation with
    | JL.Native -> assert false
    | JL.Java x ->
      { Proc.id = "p" ^ string_of_int !id ^ "_"
      ; Proc.params = J.params (Lazy.force x)
      ; Proc.content = Array.to_list (J.code (Lazy.force x))
      ; Proc.ret_type = JB.ms_rtype sign
      ; Proc.v_count = 0
      ; Proc.assignments = Proc.V_map.empty
        (* ; ret_sort = Ir.Int (1* TODO, where can I can get sort from? *1) *)
      } in


  let virtual_lookup target ms =
    let nodes = match target with
      | J.VirtualCall obj  -> JC.static_lookup_virtual program obj ms
      | J.InterfaceCall cn -> JC.static_lookup_interface program cn ms in

    let node_impl = function
      | JP.Interface _ -> [] (* We only need to consider concrete implementations *)
      | JP.Class node  -> [Mm.find ms (node.JP.c_info.JL.c_methods)] in

    let get_inner_method = function
      | JL.AbstractMethod _ -> assert false (* TODO *)
      | JL.ConcreteMethod m -> m in

    nodes
    |> List.map node_impl
    |> List.concat
    |> List.map (fun m -> parse_method (get_inner_method m)) in

  let cms_map = ref (Cms_map.empty) in

  let cms_lookup cms =
    if Cms_map.mem cms !cms_map
    then
      Cms_map.find cms !cms_map
    else
      let meth = parse_method (snd (Cmm.find cms methods)) in
      cms_map := Cms_map.add cms meth !cms_map;
      meth
  in

  { cms_lookup
  ; virtual_lookup
  }
