open Program

let line_sep = String.concat "\n"
let space_sep = String.concat " "
let parens strs = "(" ^ space_sep strs ^ ")"

let header =
  line_sep
    [ "(set-option :fixedpoint.engine \"duality\")"
    ; "(declare-rel __THE_QUERY__ (Int))"
    ]

let show_var (Variable v) = v

let interpret_var v =
  parens ["declare-var"; show_var v ; "Int"]

let rel_name lbl = "Line" ^ string_of_int lbl

let interpret_rel (lbl, vs) =
  parens ["declare-rel"; rel_name lbl; parens (List.map (fun _ -> "Int") vs)]

let interpret_expr expr =
  let rec ex = function
    | Relation (lbl, vs) -> parens (rel_name lbl :: List.map show_var vs)
    | Query v            -> parens ["__THE_QUERY__"; show_var v]
    | Int_lit i          -> string_of_int i
    | Var v              -> show_var v
    | Eq (e1, e2)        -> parens ["="; ex e1; ex e2]
    | Implies (e1, e2)   -> parens ["=>"; ex e1; ex e2]
    | And es             -> parens ("and" :: List.map ex es)
    | Not e              -> parens ["not"; ex e]
    | Invoke _           -> assert false (** TODO *) in
  parens ["rule"; ex expr]

let query =
  "(query __THE_QUERY__)"

let interpret exprs =
  let vars = Var_set.unions_map expr_vars exprs in
  let rels = Rel_set.unions_map expr_rels exprs in

  line_sep
    [ header
    ; line_sep (List.map interpret_var (Var_set.elements vars))
    ; line_sep (List.map interpret_rel (Rel_set.elements rels))
    ; line_sep (List.map interpret_expr exprs)
    ; query
    ]
