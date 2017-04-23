module QID = QualifiedIdentity
module A = Algorithm

module T = Type
module E = Expr

let header = A.unlines
    [ "(set-option :fixedpoint.engine \"duality\")" ]

let rec sort = function
  | T.Unit     -> "Int"
  | T.Int      -> "Int"
  | T.Bool     -> "Bool"
  | T.Real     -> "Real"
  | T.Array s  -> A.parens ["Array Int"; sort s]

let var (Var.Mk (name, typ)) =
  A.parens [ "declare-var"; QID.as_path name; sort typ ]

let qid = QID.as_path

let rel (l, ss) =
  A.parens [ "declare-rel"
           ; Lbl.name l
           ; A.parens (List.map sort ss)
           ]

let op o es =
  let apply o = A.parens (o :: es) in
  let bitwise o =
    let to_bv e = A.parens ["(_ int2bv 64)"; e] in
    A.parens ["bv2int"; A.parens (o :: List.map to_bv es)]
  in
  match o with
  | E.Unop o -> (match o with
    | E.Not -> apply "not")
  | E.Biop o -> (match o with
    | E.IAdd | E.RAdd -> apply "+"
    | E.ISub | E.RSub -> apply "-"
    | E.IMul | E.RMul -> apply "*"
    | E.IRem | E.RRem -> apply "rem"
    | E.IDiv          -> apply "div"
    | E.RDiv          -> apply "/"
    | E.IGe  | E.RGe  -> apply ">="
    | E.IGt  | E.RGt  -> apply ">"
    | E.ILe  | E.RLe  -> apply "<="
    | E.ILt  | E.RLt  -> apply "<"
    | E.Eq            -> apply "="
    | E.Impl          -> apply "=>"
    | E.BAnd          -> bitwise "bvand"
    | E.BOr           -> bitwise "bvor"
    | E.BXor          -> bitwise "bvxor"
    | E.BShl          -> bitwise "bvshl"
    | E.BLShr         -> bitwise "bvlshr"
    | E.BAShr         -> bitwise "bvashr")
  | E.Manyop o -> (match o with
    | E.And -> apply "and"
    | E.Or  -> apply "or")

let expr e =
  let var (Var.Mk (name, _)) = QID.as_path name in
  let rec ex = function
    | E.Query (l, at, e)       -> Lbl.name l
    | E.Relation ((l, ss), es) -> A.parens (Lbl.name l :: (List.map ex es))
    | E.Var v                  -> var v
    | E.Apply (o, es)          -> op o (List.map ex es)
    | E.Int i                  -> string_of_int i
    | E.Real f                 -> string_of_float f
    | E.Bool true              -> "true"
    | E.Bool false             -> "false"
    | E.Store (v, i, e)        -> A.parens ["store"; ex v; ex i; ex e]
    | E.Select (v, e)          -> A.parens ["select"; ex v; ex e]
    | E.Allocate _             -> assert false
  in
  A.parens ["rule"; ex e]

let declare_query (l, _) = A.parens ["declare-rel"; Lbl.name l; A.parens []]

let query (l, _) = A.parens ["query"; Lbl.name l]

let print exprs =
  let vars    = E.exprs_access E.vars    exprs in
  let rels    = E.exprs_access E.rels    exprs in
  let queries = E.exprs_access E.queries exprs in

  A.unlines
    [ header
    ; A.unlines (List.map var vars)
    ; A.unlines (List.map rel rels)
    ; A.unlines (List.map declare_query queries)
    ; A.unlines (List.map expr exprs)
    ; A.unlines (List.map query queries)
    ]
