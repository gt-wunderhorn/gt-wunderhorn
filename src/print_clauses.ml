open Ir

let line_sep = String.concat "\n"
let space_sep = String.concat " "
let parens strs = "(" ^ space_sep strs ^ ")"

let header =
  line_sep
    [ "(set-option :fixedpoint.engine \"duality\")" ]

let show_var (Variable v) = v

let print_var v =
  parens ["declare-var"; show_var v ; "Int"]

let print_rel (lbl, vs) =
  parens ["declare-rel"; lbl; parens (List.map (fun _ -> "Int") vs)]

let print_expr query_count expr =
  let rec ex = function
    | Relation (lbl, vs) -> parens (lbl :: List.map show_var vs)
    | Query v            ->
      query_count := !query_count + 1;
      parens ["q" ^ string_of_int !query_count; show_var v]
    | Var v              -> show_var v
    | Add (e1, e2)       -> parens ["+";  ex e1; ex e2]
    | Eq (e1, e2)        -> parens ["=";  ex e1; ex e2]
    | Ge (e1, e2)        -> parens [">="; ex e1; ex e2]
    | Gt (e1, e2)        -> parens [">";  ex e1; ex e2]
    | Le (e1, e2)        -> parens ["<="; ex e1; ex e2]
    | Lt (e1, e2)        -> parens ["<";  ex e1; ex e2]
    | Implies (e1, e2)   -> parens ["=>"; ex e1; ex e2]
    | And es             -> parens ("and" :: List.map ex es)
    | Not e              -> parens ["not"; ex e]
    | Int_lit i          -> string_of_int i
    | True               -> "true"
    | False              -> "false" in
  parens ["rule"; ex expr]

let rec range start stop =
  if start == stop then [] else start :: (range (start+1) stop)

let declare_query n = parens ["declare-rel"; "q" ^ string_of_int n; parens ["Int"]]
let query n = parens ["query"; "q" ^ string_of_int n]

let print exprs =

  let vars = Var_set.unions_map expr_vars exprs in
  let rels = Rel_set.unions_map expr_rels exprs in

  let query_count = ref (-1) in

  line_sep
    [ header
    ; line_sep (List.map print_var (Var_set.elements vars))
    ; line_sep (List.map print_rel (Rel_set.elements rels))
    ; line_sep (List.map declare_query (range 0 (num_queries exprs)))
    ; line_sep (List.map (print_expr query_count) exprs)
    ; line_sep (List.map query (range 0 (num_queries exprs)))
    ]
