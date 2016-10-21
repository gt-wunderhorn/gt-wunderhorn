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
                 JB.method_signature ->
                 int ->
                 Proc.t list

             ; class_id : JB.class_name -> int
             }
module Cms_map = Map.Make(
  struct type t = JB.class_method_signature;; let compare = compare end)

module Cn_map = Map.Make(
  struct type t = JB.class_name;; let compare = compare end)

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

  (* let print_cms cms _ = *)
  (*   let (cn, ms) = JB.cms_split cms in *)
  (*   Printf.eprintf "%s %s\n" (JB.cn_name cn) (JB.ms_name ms); *)
  (* in *)
  (* Cmm.iter print_cms methods; *)

  let parse_method cm =
    let sign = cm.JL.cm_signature in
    id := !id + 1;
    match cm.JL.cm_implementation with
    | JL.Native -> assert false
    | JL.Java x ->
      { Proc.id          = "p" ^ string_of_int !id ^ "_"
      ; Proc.params      = J.params (Lazy.force x)
      ; Proc.content     = Array.to_list (J.code (Lazy.force x))
      ; Proc.ret_type    = JB.ms_rtype sign
      ; Proc.sign        = sign
      ; Proc.cl_name     = fst (JB.cms_split (cm.JL.cm_class_method_signature))
      } in

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

  let virtual_lookup ms line =
    program.JP.static_lookup_method cn ms line
    |> JB.ClassMethodSet.elements
    |> List.map cms_lookup
  in

  let cn_map = ref (Cn_map.empty) in
  let cn_id = ref 0 in
  let class_id cn =
    if not (Cn_map.mem cn !cn_map)
    then
      cn_map := Cn_map.add cn !cn_id !cn_map;
    cn_id := !cn_id + 1;
    Cn_map.find cn !cn_map

  in

  { cms_lookup
  ; virtual_lookup
  ; class_id
  }
