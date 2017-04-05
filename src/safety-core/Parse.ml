module JL = Javalib_pack.Javalib
module JCode = Javalib_pack.JCode
module JP = Sawja_pack.JProgram
module J = Sawja_pack.JBir
module JB = Javalib_pack.JBasics

module JC = Sawja_pack.JControlFlow

module Cmm = JB.ClassMethodMap
module Mm = JB.MethodMap

module QID = QualifiedIdentity

type t = { cms_lookup :
             JB.class_method_signature ->
             Proc.t list

         ; has_cms :
             JB.class_method_signature ->
             bool

         ; virtual_lookup :
             JB.class_name ->
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

let native = JB.make_cn "MyNative"
let native_entry =
  let ms = JB.make_ms "entry" [] None in
  JB.make_cms native ms

let local_var_table classpath cname =
  let jl_class_or_iface = JL.get_class (JL.class_path classpath) cname in

  let get_concrete ms jl_method map = match jl_method with
    | JL.AbstractMethod _ -> map
    | JL.ConcreteMethod ({JL.cm_implementation}) ->
      (match cm_implementation with
        | JL.Native -> map
        | JL.Java lazy_code ->
          let ({JCode.c_local_variable_table}) = Lazy.force lazy_code in
          (match c_local_variable_table with
            | None -> map
            | Some vars -> JB.MethodMap.add ms vars map
          )
      )
  in

  match jl_class_or_iface with
    | JL.JClass ({JL.c_methods}) ->
      JB.MethodMap.fold (get_concrete) c_methods (JB.MethodMap.empty)
    | _ -> JB.MethodMap.empty

let parse id classpath cms =
  let (prta,instantiated_classes) =
    Sawja_pack.JRTA.parse_program
      ~other_entrypoints:[native_entry; cms]
      classpath
      cms in

  let program = JP.map_program2
      (fun _ -> J.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[])
      (Some (fun code pp -> (J.pc_ir2bc code).(pp)))
      prta in

  let methods = program.JP.parsed_methods in
  let cms_map = ref (Cms_map.empty) in

  let rec cms_lookup cms =
    if Cms_map.mem cms !cms_map
    then [Cms_map.find cms !cms_map]
    else if Cmm.mem cms methods
    then
      let meth = parse_method (snd (Cmm.find cms methods)) in
      cms_map := Cms_map.add cms meth !cms_map;
      [meth]
    else []

  and parse_method cm =
    let sign = cm.JL.cm_signature in
    let cl_name = fst (JB.cms_split (cm.JL.cm_class_method_signature)) in
    (* TODO: shouldn't read whole class every time
       idea, maybe make a map of QID -> type *)
    let var_tables = local_var_table classpath cl_name in
    let opt_vartable = try
      Some (JB.MethodMap.find sign var_tables)
    with
      | _ -> None
    in
    id := !id + 1;
    match cm.JL.cm_implementation with
    | JL.Java x ->
      { Proc.name     = QID.of_list [JB.cn_name cl_name; JB.ms_name sign]
      ; Proc.params   =
               if JB.ms_name sign = "<init>"
               then List.tl (J.params (Lazy.force x))
               else (J.params (Lazy.force x))
      ; Proc.content  = Array.to_list (J.code (Lazy.force x))
      ; Proc.ret_type = JB.ms_rtype sign
      ; Proc.sign     = sign
      ; Proc.vartable = opt_vartable
      ; Proc.cl_name  = fst (JB.cms_split (cm.JL.cm_class_method_signature))
      }

    | JL.Native ->
      let (cn, ms) = JB.cms_split (cm.JL.cm_class_method_signature) in

      let alternate_sig = JB.make_cms native ms in
      List.hd (cms_lookup alternate_sig)
  in

  let has_cms cms =
    Cmm.mem cms methods
  in

  let virtual_lookup cn ms line =
    program.JP.static_lookup_method cn ms line
    |> JB.ClassMethodSet.elements
    |> List.map cms_lookup
    |> List.concat
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
  ; has_cms
  ; virtual_lookup
  ; class_id
  }
