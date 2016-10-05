open Javalib_pack
open Javalib
open JBasics
open Sawja_pack
open JProgram

let classpath =
  "/home/david/Workspace/path-interpolation/bin:/usr/lib/jvm/java-7-openjdk-amd64/jre/lib/rt.jar"

let (prta,instantiated_classes) =
  JRTA.parse_program classpath
    (JBasics.make_cms
       (JBasics.make_cn "Test") JProgram.main_signature)

let pbir = JProgram.map_program2
    (fun _ -> A3Bir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[]) 
    (Some (fun code pp -> (A3Bir.pc_ir2bc code).(pp)))
    prta

let () =
  A3Bir.print_program pbir "./out";
