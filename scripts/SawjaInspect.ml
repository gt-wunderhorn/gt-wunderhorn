open Javalib_pack
open Javalib
open JBasics
open Sawja_pack
open JProgram

let classpath =
  let default_cp = [
    (Sys.getcwd ());
  ] in
  let with_env_vars = try
    (Sys.getenv "CLASSPATH") :: default_cp
  with
    Not_found -> default_cp
  in
  String.concat ":" with_env_vars

let (prta,instantiated_classes) =
  JRTA.parse_program classpath
    (JBasics.make_cms
       (JBasics.make_cn "Test") JProgram.main_signature)

let pbir = JProgram.map_program2
    (fun _ -> JBir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[])
    (Some (fun code pp -> (JBir.pc_ir2bc code).(pp)))
    prta

let () =
  JBir.print_program pbir "./jbir-html";
