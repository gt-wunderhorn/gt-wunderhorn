module JL = Javalib_pack.Javalib
module JP = Sawja_pack.JProgram
module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

module JC = Sawja_pack.JControlFlow

module Cmm = JB.ClassMethodMap
module Mm = JB.MethodMap

type 'a parse = { cms_lookup :
                    JB.class_method_signature ->
                    'a Ir.procedure

                ; virtual_lookup :
                    J.virtual_call_kind ->
                    JB.method_signature ->
                    ('a Ir.procedure) list
                }

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
    id := !id + 1;
    match cm.JL.cm_implementation with
    | JL.Native -> assert false
    | JL.Java x ->
      { Ir.id     = "p" ^ string_of_int !id
      ; params   = List.map (Jbir_to_ir.tvar) (J.params (Lazy.force x))
      ; ret_sort = Ir.Int (* TODO, where can I can get sort from? *)
      ; content  = Array.to_list (J.code (Lazy.force x))
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

  let cms_lookup cms=
    parse_method (snd (Cmm.find cms methods)) in

  { cms_lookup
  ; virtual_lookup
  }
