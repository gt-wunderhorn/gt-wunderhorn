module L = Lang
module Set = Core.Std.Set.Poly

let union_map_list f xs = Set.union_list (List.map f xs)

let line_sep = String.concat "\n"
let space_sep = String.concat " "
let parens strs = "(" ^ space_sep strs ^ ")"

let header = line_sep
    [ "(set-option :fixedpoint.engine \"duality\")" ]

let rec sort = function
  | L.Int      -> "Int"
  | L.Bool     -> "Bool"
  | L.Real     -> "Real"
  | L.Array s  -> parens ["Array Int"; sort s]

let print_var (v, s) =
  parens [ "declare-var"; v ; sort s ]

let print_rel (lbl, es) =
  parens [ "declare-rel"
         ; "r_" ^ string_of_int lbl
         ; parens (List.map (fun e -> sort (L.expr_sort e)) es)
         ]

let show_un_op = function
  | L.Not -> "not"

let show_bi_op op e1 e2 =
  let bitwise o =
    let to_bv e = parens ["(_ int2bv 64)"; e] in
    parens ["bv2int"; parens [o; to_bv e1; to_bv e2]]
  in

  let apply o = parens [o; e1; e2] in

  match op with
  | L.Add  -> apply "+"
  | L.Sub  -> apply "-"
  | L.Mul  -> apply "*"
  | L.Rem  -> apply "rem"
  | L.Div  -> apply "/"
  | L.Eq   -> apply "="
  | L.Ge   -> apply ">="
  | L.Gt   -> apply ">"
  | L.Le   -> apply "<="
  | L.Lt   -> apply "<"
  | L.Impl -> apply "=>"
  | L.BAnd  -> bitwise "bvand"
  | L.BOr   -> bitwise "bvor"
  | L.BXor  -> bitwise "bvxor"
  | L.BShl  -> bitwise "bvshl"
  | L.BLShr -> bitwise "bvlshr"
  | L.BAShr -> bitwise "bvashr"

let show_many_op = function
  | L.And -> "and"

let print_expr expr =
  let rec ex = function
    | L.Var v                  -> fst v
    | L.Un_op (op, e)          -> parens [show_un_op op; ex e]
    | L.Bi_op (op, e1, e2)     -> show_bi_op op (ex e1) (ex e2)
    | L.Many_op (op, es)       -> parens (show_many_op op :: List.map ex es)
    | L.ArrStore (arr, idx, e) -> parens ["store"; ex arr; ex idx; ex e]
    | L.ArrSelect (e1, e2)     -> parens ["select"; ex e1; ex e2]
    | L.Int_lit i              -> string_of_int i
    | L.Real_lit f             -> string_of_float f
    | L.True                   -> "true"
    | L.False                  -> "false"
    | L.Any s                  -> "" (* TODO *)
  in
  parens ["rule"; ex expr]

let declare_query (lbl, _, _) = parens ["declare-rel"; "q_" ^ string_of_int lbl; parens ["Bool"]]

let query (lbl, _, _) = parens ["query"; "q_" ^ string_of_int lbl]

let print exprs =
  let vars = union_map_list L.expr_vars exprs in
  let rels = union_map_list L.expr_rels exprs in
  let queries = Set.elements (Set.union_map_list L.queries exprs) in

  line_sep
    [ header
    ; line_sep (List.map print_var (Set.elements vars))
    ; line_sep (List.map print_rel (Set.elements rels))
    ; line_sep (List.map declare_query queries)
    ; line_sep (List.map print_expr exprs)
    ; line_sep (List.map query queries)
    ]
