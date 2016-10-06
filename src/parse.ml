open Javalib_pack
open Javalib
open JBasics
open Sawja_pack
open JProgram
open Procedure

type method_map =
  Javalib_pack.JBasics.class_method_signature ->
  Sawja_pack.JBir.t

let parse id classpath cn =
  let (prta,instantiated_classes) =
    JRTA.parse_program classpath
      (JBasics.make_cms cn JProgram.main_signature) in

  let pbir = JProgram.map_program2
      (fun _ -> JBir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[]) 
      (Some (fun code pp -> (JBir.pc_ir2bc code).(pp)))
      prta in

  let methods = pbir.parsed_methods in

  fun cms ->
    id := !id + 1;
    let meth = ClassMethodMap.find cms methods in
    let cm = snd meth in
    match cm.cm_implementation with
    | Native -> assert false
    | Java x ->
      { id       = "p" ^ string_of_int !id
      ; params   = List.map (Jbir_to_ir.tvar) (JBir.params (Lazy.force x))
      ; ret_sort = Ir.Int (* TODO, where can I can get sort from? *)
      ; content  = Array.to_list (JBir.code (Lazy.force x))
      }
