module G = Graph
module QID = QualifiedIdentity
module E = Expr

type 'a instr_path =
  | Assert   of E.t * E.assert_type
  | Body     of E.t * (Var.t * E.t) list
  | ScopeIn  of QID.t * E.t * (Var.t * E.t) list
  | ScopeOut of QID.t
  | CallLink of
    Lbl.t
    * 'a                        (* proc entr *)
    * 'a                        (* proc exit *)
    * (Var.t * E.t) list        (* arguments *)
    * Var.t                     (* value *)
  | Return of
    'a                          (* proc entr *)
    * 'a                        (* proc exit *)
    * Var.t list                (* params *)
    * E.t * (Var.t * E.t) list  (* body *)
    * E.t                       (* value *)

type linked_path = Lbl.t instr_path
type annotated_path = (Var.t list) instr_path

type expr_path =
  | EAssert of E.t * E.assert_type
  | EBody of E.t
  | EReturn of E.t * E.t
