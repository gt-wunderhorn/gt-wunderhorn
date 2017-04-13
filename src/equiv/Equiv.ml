module JB = Javalib_pack.JBasics
module JP = Sawja_pack.JProgram
module QID = QualifiedIdentity
module E = Expr

module JBir = JBirToIr

let equiv class_name params return name1 name2 input_vars constraints =
  let mk_sig name = JB.make_ms name params (Some return) in

  let clauses_for name =
    let classpath = Sys.argv.(1) in
    let meth = mk_sig name in
    Inspect.gen_ir classpath class_name meth |> Inspect.inspect
  in

  let exprs = clauses_for name1 @ clauses_for name2 in

  let out_for name = E.Var (Var.Mk (QID.QID ["out"; name], JBir.typ return)) in

  let rel_for name =
    let args = input_vars @ [out_for name] in

    let type_list = List.map JBir.typ (params @ [return]) in

    let r = (Lbl.At (QID.QID [class_name; name], Lbl.Exit), type_list) in
    E.Relation (r, args)
  in

  let query =
    E.mk_impl
      (E.mk_and
        (constraints @
        [ rel_for name1
        ; rel_for name2
        ; E.mk_not (E.mk_eq (out_for name1) (out_for name2))
        ]))
      (E.Query ((Lbl.Nowhere, E.Equivalence), (E.Bool true)))
  in

  exprs @ [query]


let usage _ =
  Printf.eprintf "usage: %s <classpath> [print|run]\n" Sys.argv.(0)

let mk_sig name =
  JB.make_ms name [(JB.TBasic `Int)] (Some (JB.TBasic `Int))

let run class_name params return name1 name2 input_vars constraints =
  if (Array.length Sys.argv < 3)
  then
    usage ()
  else
    let exprs = equiv class_name params return name1 name2 input_vars constraints in
    if Sys.argv.(2) = "print"
    then
      Printf.printf "%s\n" (PrintClauses.print exprs)

    else if Sys.argv.(2) = "run"
    then ClausesToZ3.run exprs

    else
      ( Printf.eprintf "unknown run type: %s\n" Sys.argv.(3)
      ; usage ()
      )
