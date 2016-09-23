module AL = Abstr_lang

type relation = Relation of string * Al.variable

type condition =
  | Expr of AL.expr
  | Rel  of relation

type clause = Rule of condition list * relation
type query = Query of relation * Al.variable

type program = clause list * query
