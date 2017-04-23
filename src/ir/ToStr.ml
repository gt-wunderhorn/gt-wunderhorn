module L = Lbl
module V = Var
module E = Expr
module I = Instr

let fmt = Printf.sprintf

let qid = QualifiedIdentity.as_path

let commasep = String.concat ", "

let lbl = function
  | L.At (q, (L.Line lineno)) -> fmt "Lbl %s:%u" (qid q) lineno
  | L.At (q, (L.Entrance))        -> fmt "Lbl %s:Entrance" (qid q)
  | L.At (q, (L.Exit))            -> fmt "Lbl %s:Exit" (qid q)

let rec typ = function
  | Type.Unit -> "Unit"
  | Type.Int  -> "Int"
  | Type.Bool -> "Bool"
  | Type.Real -> "Real"
  | Type.Array (kind) -> fmt "[%s]" (typ kind)

let var (V.Mk (q, kind)) = fmt "Mk (%s, %s)" (qid q) (typ kind)

let unop E.Not = "Not"

let biop = function
  | E.BAnd  -> "BAnd"
  | E.BOr   -> "BOr"
  | E.BXor  -> "BXor"
  | E.BShl  -> "BShl"
  | E.BLShr -> "BLShr"
  | E.BAShr -> "BAShr"
  | E.Eq    -> "Eq"
  | E.Impl  -> "Impl"
  | E.IAdd  -> "IAdd"
  | E.IDiv  -> "IDiv"
  | E.IMul  -> "IMul"
  | E.ISub  -> "ISub"
  | E.IRem  -> "IRem"
  | E.IGe   -> "IGe"
  | E.IGt   -> "IGt"
  | E.ILe   -> "ILe"
  | E.ILt   -> "ILt"
  | E.RAdd  -> "RAdd"
  | E.RDiv  -> "RDiv"
  | E.RMul  -> "RMul"
  | E.RSub  -> "RSub"
  | E.RRem  -> "RRem"
  | E.RGe   -> "RGe"
  | E.RGt   -> "RGt"
  | E.RLe   -> "RLe"
  | E.RLt   -> "RLt"

let manyop = function
  | E.And -> "And"
  | E.Or  -> "Or"

let assert_t = function
  | Assert.Div0        -> "Div0"
  | Assert.Null        -> "Null"
  | Assert.NegArray    -> "NegArray"
  | Assert.ArrayBound  -> "ArrayBound"
  | Assert.User        -> "User"

let op = function
  | E.Unop op   -> unop op
  | E.Biop op   -> biop op
  | E.Manyop op -> manyop op

let query (E.QueryInfo (asrt, fname, line)) =
  fmt "Query (%s, %s, %d)" (assert_t asrt) fname line

let rel = function
  | (label, ts) -> fmt "Rel (%s, [%s])" (lbl label)
      (List.map (typ) ts |> commasep)

let rec expr = function
  | E.Var v             -> fmt "Var (%s)" (var v)
  | E.Int num           -> fmt "Int (%d)" num
  | E.Real num          -> fmt "Real (%f)" num
  | E.Bool flag         -> fmt "Bool (%B)" flag
  | E.Store (a, b, c)   -> fmt "Store (%s, %s, %s)" (expr a) (expr b) (expr c)
  | E.Select (a, b)     -> fmt "Select (%s, %s)" (expr a) (expr b)
  | E.Apply (o, ts)     -> fmt "Apply (%s, [%s])" (op o) (List.map expr ts |> commasep)
  | E.Relation (r, ts)  -> fmt "Relation (%s, [%s])" (rel r) (List.map expr ts |> commasep)
  | E.Query (l, q, a)   -> fmt "Query (%s, %s, %s)" (lbl l) (query q) (expr a)
  | E.Allocate a        -> fmt "Allocate (%s)" (expr a)

let rec instr (I.Instr (label, i)) = fmt "Instr (%s, %s)" (lbl label) (ir i)

and ir = function
  | I.Assign (v, e) -> fmt "Assign (%s, %s)" (var v) (expr e)
  | I.Goto label    -> fmt "Goto (%s)" (lbl label)
  | I.If (e, label) -> fmt "If (%s, %s)" (expr e) (lbl label)
  | I.Return e      -> fmt "Return (%s)" (expr e)
  | I.Invoke (p, v, es) -> fmt "Invoke (%s, %s, [%s])"
                               (proc p)
                               (var v)
                               (List.map expr es |> commasep)
  | I.Assert   (e, q) -> fmt "Assert (%s, %s)"
                             (expr e) (query q)

(* TODO: print content in some way *)
and proc { I.id; I.params; I.ret_type; I.content } =
  fmt "Proc {id: %s; params: [%s]; ret_type: %s; content: _}"
    (qid id)
    (List.map var params |> commasep)
    (typ ret_type)

and class_proc (class_type, p) = fmt "(%s, %s)" (expr class_type) (proc p)

let instructions intrs = (List.map instr intrs |> commasep)
