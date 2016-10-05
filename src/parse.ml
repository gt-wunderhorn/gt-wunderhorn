open Javalib_pack
open Javalib
open JBasics
open Sawja_pack
open JProgram
open Procedure

type method_map =
  Javalib_pack.JBasics.class_method_signature ->
  Sawja_pack.A3Bir.t

let parse id classpath cn =
  let (prta,instantiated_classes) =
    JRTA.parse_program classpath
      (JBasics.make_cms cn JProgram.main_signature) in

  let pbir = JProgram.map_program2
      (fun _ -> A3Bir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[]) 
      (Some (fun code pp -> (A3Bir.pc_ir2bc code).(pp)))
      prta in

  let methods = pbir.parsed_methods in

  fun cms ->
    id := !id + 1;
    let meth = ClassMethodMap.find cms methods in
    let cm = snd meth in
    match cm.cm_implementation with
    | Native -> assert false
    | Java x ->
      { id      = "p" ^ string_of_int !id
      ; params  = List.map (fun v -> Ir.Variable (A3Bir.var_name (snd v))) (A3Bir.params (Lazy.force x))
      ; content = Array.to_list (A3Bir.code (Lazy.force x))
      }
