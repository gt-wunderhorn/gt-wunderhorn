module Set = Core.Std.Set.Poly
module T = Type

type unop = Not

type biop =
  | BAnd  | BOr   | BXor  | BShl  | BLShr | BAShr
  | Eq    | Impl
  | IAdd  | IDiv  | IMul  | ISub  | IRem | IGe   | IGt   | ILe   | ILt
  | RAdd  | RDiv  | RMul  | RSub  | RRem | RGe   | RGt   | RLe   | RLt

type manyop = And | Or

type op =
  | Unop   of unop
  | Biop   of biop
  | Manyop of manyop

type query = QueryInfo of Assert.t * string * int
type rel = Lbl.t * T.t list

type t =
  | Var       of Var.t
  | Int       of int
  | Real      of float
  | Bool      of bool
  | Store     of t * t * t
  | Select    of t * t
  | Apply     of op * t list
  | Relation  of rel * t list
  | Query     of Lbl.t * query * t
  | Allocate  of t

let to_horn_clause = function
  | Apply (Biop Impl, [lhs; rhs]) ->
    Some (lhs, rhs)
  | _ -> None

let from_horn_clause lhs rhs =
  Apply (Biop Impl, [lhs; rhs])

let op_type = function
  | Unop o -> (match o with
      | Not -> T.Bool)
  | Biop o -> (match o with
      | BAnd  | BOr   | BXor  | BShl  | BLShr | BAShr -> T.Int
      | Eq    | Impl                                  -> T.Bool
      | IAdd  | ISub  | IDiv  | IMul  | IRem          -> T.Int
      | IGe   | IGt   | ILe   | ILt                   -> T.Bool
      | RAdd  | RSub  | RDiv  | RMul  | RRem          -> T.Real
      | RGe   | RGt   | RLe   | RLt                   -> T.Bool)
  | Manyop o -> (match o with
      | And | Or -> T.Bool)

let rec type_of e =
  let var_t (Var.Mk (_, t)) = t in
  match e with
  | Var v         -> var_t v
  | Apply (op, _) -> op_type op
  | Int _         -> T.Int
  | Real _        -> T.Real
  | Bool _        -> T.Bool
  | Relation _    -> T.Bool
  | Query _       -> T.Bool
  | Store (v,_,_) -> type_of v
  | Select (v,_)  -> T.inner (type_of v)
  | Allocate _    -> T.Bool

let mk_not = function
  | Apply (Unop Not, es) -> List.hd es
  | e                    -> Apply (Unop Not, [e])

let mk_biop op e1 e2 = Apply (Biop op, [e1; e2])

let mk_iadd  = mk_biop IAdd
let mk_radd  = mk_biop RAdd
let mk_imul  = mk_biop IMul
let mk_rmul  = mk_biop RMul
let mk_isub  = mk_biop ISub
let mk_rsub  = mk_biop RSub
let mk_idiv  = mk_biop IDiv
let mk_rdiv  = mk_biop RDiv
let mk_irem  = mk_biop IRem
let mk_rrem  = mk_biop RRem
let mk_ile   = mk_biop ILe
let mk_ilt   = mk_biop ILt
let mk_ige   = mk_biop IGe
let mk_igt   = mk_biop IGt
let mk_rle   = mk_biop RLe
let mk_rlt   = mk_biop RLt
let mk_rge   = mk_biop RGe
let mk_rgt   = mk_biop RGt
let mk_impl  = mk_biop Impl
let mk_bshl  = mk_biop BShl
let mk_blshr = mk_biop BLShr
let mk_bashr = mk_biop BAShr
let mk_band  = mk_biop BAnd
let mk_bor   = mk_biop BOr
let mk_bxor  = mk_biop BXor
let mk_eq    = mk_biop Eq
let mk_neq x y = mk_not (mk_biop Eq x y)

let mk_ineg = mk_isub (Int  0  )
let mk_rneg = mk_rsub (Real 0.0)

let is_true  e = e = Bool true
let is_false e = e = Bool false

let is_var = function | Var _ -> true | _ -> false

let is_simp = function
  | Var _ | Int _ | Real _ | Bool _ -> true
  | _ -> false

let mk_and es = Apply (Manyop And, es)
let mk_or  es = Apply (Manyop Or , es)

let rec map special e =
  let ex = map special in
  let base = function
    | Relation (r, es)  -> Relation (r, List.map ex es)
    | Query (lbl, q, e) -> Query (lbl, q, ex e)
    | Apply (op, es)    -> Apply (op, (List.map ex es))
    | Store (f, i, e)   -> Store (f, ex i, ex e)
    | Select (f, e)     -> Select (f, ex e)
    | e                 -> e
  in
  Algorithm.specialize base special e

(** A common pattern is to recurse over subexpressions to calculate some value.
    `fold` encapsulates this recursion. The user provides a `special_case`
    function which indicates when an expression yields a result. When
    `special_case` returns `None`, fold recursively applies to the
    subexpressions and combines the results using `f`. If there are no
    subexpressions, then returns `zero`. *)
let rec fold special f zero e =
  let ex = fold special f zero in
  let base = function
    | Relation (_, es) -> List.fold_left f zero (List.map ex es)
    | Query (_, _, e)  -> ex e
    | Apply (_, es)    -> List.fold_left f zero (List.map ex es)
    | Store (v, i, e)  -> f (ex i) (ex e)
    | Select (v, e)    -> ex e
    | _                -> zero
  in
  Algorithm.specialize base special e

let contains f =
  let check_used = function
    | e when (f e) -> Some(true)
    | _ -> None
  in
  fold check_used (||) false

let rec optimize e =
  let optimize' e =
    let reduce e =
      let special = function
        | Apply (op, es) -> (
            let es = List.map optimize es in
            match op with
            | Unop op -> (
                let e = List.hd es in
                match (op, e) with
                | (Not, (Apply (Unop Not, es))) -> Some (List.hd es)
                | (Not, (Bool x)) -> Some (Bool (not x))
                | _ -> None)
            | Biop op -> (
                let e1 = List.hd es in
                let e2 = List.hd (List.tl es) in
                match (op, e1, e2) with
                | (IAdd, Int i1, Int i2) -> Some (Int (i1 + i2))
                | (ISub, Int i1, Int i2) -> Some (Int (i1 - i2))
                | (IMul, Int i1, Int i2) -> Some (Int (i1 * i2))
                | (IDiv, Int i1, Int i2) -> Some (Int (i1 / i2))
                | (IRem, Int i1, Int i2) -> Some (Int (i1 mod i2))
                | (IGt,  Int i1, Int i2) -> Some (Bool (i1 >  i2))
                | (IGe,  Int i1, Int i2) -> Some (Bool (i1 >= i2))
                | (ILt,  Int i1, Int i2) -> Some (Bool (i1 <  i2))
                | (ILe,  Int i1, Int i2) -> Some (Bool (i1 <= i2))

                | (RAdd, Real r1, Real r2) -> Some (Real (r1 +. r2))
                | (RSub, Real r1, Real r2) -> Some (Real (r1 -. r2))
                | (RMul, Real r1, Real r2) -> Some (Real (r1 *. r2))
                | (RDiv, Real r1, Real r2) -> Some (Real (r1 /. r2))
                | (RRem, Real r1, Real r2) -> Some (Real (mod_float r1 r2))
                | (RGt,  Real r1, Real r2) -> Some (Bool (r1 >  r2))
                | (RGe,  Real r1, Real r2) -> Some (Bool (r1 >= r2))
                | (RLt,  Real r1, Real r2) -> Some (Bool (r1 <  r2))
                | (RLe,  Real r1, Real r2) -> Some (Bool (r1 <= r2))

                | (Eq, x, y) when x = y -> Some (Bool true)
                | (Eq, Var v, Int 1) when Var.is_bool v -> Some (Var v)
                | (Eq, Var v, Int 0) when Var.is_bool v -> Some (Var v)
                | (Eq, Int 1, Var v) when Var.is_bool v -> Some (Var v)
                | (Eq, Int 0, Var v) when Var.is_bool v -> Some (Var v)

                | (Impl, Bool true, y)  -> Some y
                | (Impl, Bool false, _) -> Some (Bool true)
                | _ -> None)
            | Manyop op ->
              let associate op es =
                let sub_args e = match e with
                  | Apply (Manyop op', es) -> if op = op' then es else [e]
                  | e'                     -> [e'] in
                List.concat (List.map sub_args es) in
              (match (op, es) with
               | (And, es) when List.exists is_false es -> Some (Bool false)
               | (And, [])  -> Some (Bool true)
               | (And, [e]) -> Some e
               | (And, es)  -> Some (
                   (Apply (Manyop And,
                           associate And (List.filter ((<>) (Bool true)) es))))
               | (Or, es) when List.exists is_true es -> Some (Bool true)
               | (Or, [])  -> Some (Bool false)
               | (Or, [e]) -> Some e
               | (Or, es)  -> Some (
                   Apply (Manyop Or,
                          associate Or (List.filter ((<>) (Bool false)) es)))))
        | _ -> None
      in
      map special e
    in

    let simple_assignments e =
      let special e = match to_horn_clause e with
        | Some (Apply (Manyop And, es), rhs) ->
          let folder acc = function
            | (Apply (Biop Eq, [Var v; e])) when is_simp e -> (v, e) :: acc
            | (Apply (Biop Eq, [e; Var v])) when is_simp e -> (v, e) :: acc
            | _ -> acc in
          Some (List.fold_left folder [] es)
        | _ -> None in
      fold special (@) [] e in

    let replace_simple_assignments bindings e =
      let special = function
        | Var v when List.mem_assoc v bindings -> Some (List.assoc v bindings)
        | _ -> None in
      map special e in

    let e' = reduce e in
    let bindings = simple_assignments e' in
    replace_simple_assignments bindings e'
  in
  Algorithm.converge (=) optimize' e

let fold_union sc = fold sc Set.union Set.empty

(** Find the variables in an expression *)
let rec vars e = fold_union (function
    | Var v           -> Some (Set.singleton v)
    | Store (v, i, e) -> Some (Algorithm.union_map_list vars [v;i;e])
    | Select (v, i)   -> Some (Set.union (vars i) (vars v))
    | _               -> None) e

(** Find the queries in an expression *)
let queries = fold_union (function
    | Query (lbl, q, _) -> Some (Set.singleton (lbl, q))
    | _                 -> None)

(** Find the relations in an expression *)
let rels = fold_union (function
    | Relation (r, _) -> Some (Set.singleton r)
    | _               -> None)

let stored = fold_union (function
    | Store (v, _, _) -> Some (Set.singleton v)
    | _               -> None)

let exprs_access f exprs = Set.elements (Algorithm.union_map_list f exprs)
