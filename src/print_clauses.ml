module L = Lang

let line_sep = String.concat "\n"
let space_sep = String.concat " "
let parens strs = "(" ^ space_sep strs ^ ")"

let header = line_sep
    [ "(set-option :fixedpoint.engine \"duality\")" ]

let rec sort = function
  | L.Int -> "Int"
  | L.Bool -> "Bool"
  | L.Array s -> parens ["Array Int"; sort s]

let print_var (v, s) =
  parens ["declare-var"; v ; sort s]

let print_rel (lbl, vs) =
  parens ["declare-rel"; "r_" ^ lbl; parens (List.map (fun (_, s) -> sort s) vs)]

let show_un_op = function
  | L.Not -> "not"

let show_bi_op = function
  | L.Add  -> "+"
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
    | L.Query (lbl, e)         -> parens ["q_" ^ lbl; ex e]
    | L.Relation (lbl, vs)     -> parens (("r_" ^ lbl) :: (List.map fst vs))
    | L.Var v                  -> fst v
    | L.Un_op (op, e)          -> parens [show_un_op op; ex e]
    | L.Bi_op (op, e1, e2)     -> parens [show_bi_op op; ex e1; ex e2]
    | L.Many_op (op, es)       -> parens (show_many_op op :: List.map ex es)
    | L.ArrStore (arr, idx, e) -> parens ["store"; ex arr; ex idx; ex e]
    | L.ArrSelect (e1, e2)     -> parens ["select"; ex e1; ex e2]
    | L.Int_lit i              -> string_of_int i
    | L.True                   -> "true"
    | L.False                  -> "false" in
  parens ["rule"; ex expr]

let declare_query (lbl, _) = parens ["declare-rel"; "q_" ^ lbl; parens ["Bool"]]

let query (lbl, _) = parens ["query"; "q_" ^ lbl]

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
