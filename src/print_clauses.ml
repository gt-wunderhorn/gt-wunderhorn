module E = Expr
module Set = Core.Std.Set.Poly

let line_sep = String.concat "\n"
let space_sep = String.concat " "
let parens strs = "(" ^ space_sep strs ^ ")"

let header = line_sep
    [ "(set-option :fixedpoint.engine \"duality\")" ]

let rec sort = function
  | E.Int      -> "Int"
  | E.Bool     -> "Bool"
  | E.Real     -> "Real"
  | E.Array s  -> parens ["Array Int"; sort s]

let print_var (v, s) =
  parens [ "declare-var"; v ; sort s ]

let print_rel (lbl, es) =
  parens [ "declare-rel"
         ; "r_" ^ string_of_int lbl
         ; parens (List.map (fun e -> sort (E.sort_of e)) es)
         ]

let show_un_op = function
  | E.Not -> "not"

let show_bi_op op e1 e2 =
  let bitwise o =
    let to_bv e = parens ["(_ int2bv 64)"; e] in
    parens ["bv2int"; parens [o; to_bv e1; to_bv e2]]
  in

  let apply o = parens [o; e1; e2] in

  match op with
  | E.Add  -> apply "+"
  | E.Sub  -> apply "-"
  | E.Mul  -> apply "*"
  | E.Rem  -> apply "rem"
  | E.Div  -> apply "/"
  | E.Eq   -> apply "="
  | E.Ge   -> apply ">="
  | E.Gt   -> apply ">"
  | E.Le   -> apply "<="
  | E.Lt   -> apply "<"
  | E.Impl -> apply "=>"
  | E.BAnd  -> bitwise "bvand"
  | E.BOr   -> bitwise "bvor"
  | E.BXor  -> bitwise "bvxor"
  | E.BShl  -> bitwise "bvshl"
  | E.BLShr -> bitwise "bvlshr"
  | E.BAShr -> bitwise "bvashr"

let show_many_op = function
  | E.And -> "and"

let print_expr expr =
  let rec ex = function
    | E.Query (lbl, e, at)     -> parens ["q_" ^ string_of_int lbl; "true"]
    | E.Relation (lbl, es)     -> parens (("r_" ^ string_of_int lbl) :: (List.map ex es))
    | E.Var v                  -> fst v
    | E.Un_op (op, e)          -> parens [show_un_op op; ex e]
    | E.Bi_op (op, e1, e2)     -> show_bi_op op (ex e1) (ex e2)
    | E.Many_op (op, es)       -> parens (show_many_op op :: List.map ex es)
    | E.ArrStore (arr, idx, e) -> parens ["store"; ex arr; ex idx; ex e]
    | E.ArrSelect (e1, e2)     -> parens ["select"; ex e1; ex e2]
    | E.FieldStore (v, i, e)   -> parens ["store"; ex (E.Var v); ex i; ex e]
    | E.FieldSelect (v, e)     -> parens ["select"; ex (E.Var v); ex e]
    | E.Int_lit i              -> string_of_int i
    | E.Real_lit f             -> string_of_float f
    | E.True                   -> "true"
    | E.False                  -> "false"
    | E.Any s                  -> "" (* TODO *)
  in
  parens ["rule"; ex expr]

let declare_query (lbl, _, _) = parens ["declare-rel"; "q_" ^ string_of_int lbl; parens ["Bool"]]

let query (lbl, _, _) = parens ["query"; "q_" ^ string_of_int lbl]

let union_map_list f xs = Set.union_list (List.map f xs)
let r_union_map_list f xs = List.fold_left E.R_set.union E.R_set.empty (List.map f xs)

let print exprs =
  let vars = union_map_list E.vars exprs in
  let rels = r_union_map_list E.rels exprs in
  let queries = Set.elements (union_map_list E.queries exprs) in

  line_sep
    [ header
    ; line_sep (List.map print_var (Set.elements vars))
    ; line_sep (List.map print_rel (E.R_set.elements rels))
    ; line_sep (List.map declare_query queries)
    ; line_sep (List.map print_expr exprs)
    ; line_sep (List.map query queries)
    ]
