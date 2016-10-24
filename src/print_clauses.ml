module L = Lang

let line_sep = String.concat "\n"
let space_sep = String.concat " "
let parens strs = "(" ^ space_sep strs ^ ")"

let header = line_sep
    [ "(set-option :fixedpoint.engine \"duality\")" ]

let rec sort = function
  | L.Int      -> "Int"
  | L.Bool     -> "Bool"
  | L.Real     -> "Real"
  | L.String   -> "String"
  | L.Array s  -> parens ["Array Int"; sort s]

let print_var (v, s) =
  parens ["declare-var"; v ; sort s]

let print_rel (lbl, es) =
  parens ["declare-rel"; "r_" ^ string_of_int lbl; parens (List.map (fun e -> sort (L.expr_sort e)) es)]

let show_un_op = function
  | L.Not -> "not"
  | L.Neg -> "-"

let show_bi_op = function
  | L.Add  -> "+"
  | L.Mul  -> "*"
  | L.Rem  -> "rem"
  | L.Div  -> "/"
  | L.Eq   -> "="
  | L.Ge   -> ">="
  | L.Gt   -> ">"
  | L.Le   -> "<="
  | L.Lt   -> "<"
  | L.Impl -> "=>"

let show_many_op = function
  | L.And -> "and"

let print_expr expr =
  let rec ex = function
    | L.Query (lbl, e)         -> parens ["q_" ^ string_of_int lbl; "true"]
    | L.Relation (lbl, es)     -> parens (("r_" ^ string_of_int lbl) :: (List.map ex es))
    | L.Var v                  -> fst v
    | L.Un_op (op, e)          -> parens [show_un_op op; ex e]
    | L.Bi_op (op, e1, e2)     -> parens [show_bi_op op; ex e1; ex e2]
    | L.Many_op (op, es)       -> parens (show_many_op op :: List.map ex es)
    | L.ArrStore (arr, idx, e) -> parens ["store"; ex arr; ex idx; ex e]
    | L.ArrSelect (e1, e2)     -> parens ["select"; ex e1; ex e2]
    | L.Int_lit i              -> string_of_int i
    | L.Real_lit f             -> string_of_float f
    | L.Str_lit s              -> "\"" ^ s ^ "\""
    | L.True                   -> "true"
    | L.False                  -> "false" in
  parens ["rule"; ex expr]

let declare_query (lbl, _) = parens ["declare-rel"; "q_" ^ string_of_int lbl; parens ["Bool"]]

let query (lbl, _) = parens ["query"; "q_" ^ string_of_int lbl]

let print exprs =
  let vars = L.V_set.unions_map L.expr_vars exprs in
  let rels = L.R_set.unions_map L.expr_rels exprs in
  let queries = L.Q_set.elements (L.Q_set.unions_map L.queries exprs) in

  line_sep
    [ header
    ; line_sep (List.map print_var (L.V_set.elements vars))
    ; line_sep (List.map print_rel (L.R_set.elements rels))
    ; line_sep (List.map declare_query queries)
    ; line_sep (List.map print_expr exprs)
    ; line_sep (List.map query queries)
    ]
