open Javalib_pack
open Javalib
open JBasics
open Sawja_pack
open JProgram

type method_map =
  Javalib_pack.JBasics.class_method_signature ->
  Sawja_pack.A3Bir.t

let parse classpath cl =
  let cn = make_cn cl in

  let (prta,instantiated_classes) =
    JRTA.parse_program classpath
      (JBasics.make_cms cn JProgram.main_signature) in

  let pbir = JProgram.map_program2
      (fun _ -> A3Bir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[]) 
      (Some (fun code pp -> (A3Bir.pc_ir2bc code).(pp)))
      prta in

  let cms = make_cms cn JProgram.main_signature in

  let methods = pbir.parsed_methods in

  let meth = ClassMethodMap.find cms methods in
  match (snd meth).cm_implementation with
    | Native -> assert false
    | Java x -> Array.to_list (A3Bir.code (Lazy.force x))
