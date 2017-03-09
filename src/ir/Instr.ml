module E = Expr
module P = Printf

(** SimplIr is intended to be an extremely bare-bones language which can be used
    for analysis. *)

type class_type = E.t

type 'a proc =
  { id       : QualifiedIdentity.t
  ; params   : Var.t list
  ; ret_type : Type.t option
  ; content  : ('a list) Lazy.t
  ; class_t  : class_type
  }

and ir =
  | Assign      of Var.t * E.t
  | Goto        of Lbl.t
  | If          of E.t * Lbl.t
  | Return      of E.t
  | Invoke      of t proc * Var.t * E.t list
  | Dispatch    of E.t * (class_type * t proc) list * Var.t * E.t list
  | Assert      of E.t * E.assert_type

and t = Instr of Lbl.t * ir

let rec instr_to_str = function Instr (label, ir) ->
  Printf.sprintf "Instr (%s, %s)" (Lbl.lbl_to_str label) (ir_to_str ir)

and ir_to_str = function
  | Assign   (var, expr)                 -> P.sprintf "Assign (%s, %s)"
                                            (Var.var_to_str var)
                                            (E.expr_to_str expr)

  | Goto     (label)                     -> P.sprintf "Goto (%s)"
                                            (Lbl.lbl_to_str label)

  | If       (expr, label)               -> P.sprintf "If (%s, %s)"
                                            (E.expr_to_str expr)
                                            (Lbl.lbl_to_str label)

  | Return   (expr)                      -> P.sprintf "Return (%s)"
                                            (E.expr_to_str expr)

  | Invoke   (proc, var, exprs)          -> P.sprintf "Invoke (%s, %s, [%s])"
                                            (proc_to_str proc)
                                            (Var.var_to_str var)
                                            (List.map (E.expr_to_str) exprs |> String.concat ", ")

  | Dispatch (expr, classes, var, exprs) -> P.sprintf "Dispatch (%s, [%s], %s, [%s])"
                                            (E.expr_to_str expr)
                                            (List.map (class_proc_to_str) classes |> String.concat ", ")
                                            (Var.var_to_str var)
                                            (List.map (E.expr_to_str) exprs |> String.concat ", ")

  | Assert   (expr, assert_type)         -> P.sprintf "Assert (%s, %s)"
                                            (E.expr_to_str expr) (E.assert_t_to_str assert_type)

(* TODO: print content in some way *)
and proc_to_str = function { id; params; ret_type; content; class_t } ->
  P.sprintf "Proc {id: %s; params: [%s]; ret_type: %s; content: _; class: %s}"
    (QualifiedIdentity.as_path id)
    (List.map (Var.var_to_str) params |> String.concat ", ")
    (Option.map_default (Type.type_to_str) "Unknown" ret_type)
    (E.expr_to_str class_t)

and class_proc_to_str = function (class_type, proc) ->
  P.sprintf "(%s, %s)" (E.expr_to_str class_type) (proc_to_str proc)


let rec instructions_to_str = function intrs ->
  (List.map (instr_to_str) intrs |> String.concat "\n")


let entrance proc = Lbl.At (proc.id, Lbl.Entrance)
let exit     proc = Lbl.At (proc.id, Lbl.Exit)

let mk_assign v e = (v, e)
