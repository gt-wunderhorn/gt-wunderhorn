module G = Graph
module PG = Program_graph
module E = Expr

type field = string
type 'a field_desc =
  { name : field
  ; var  : 'a
  ; time : 'a
  }

let map_desc f d = { name = d.name; var = f d.var; time = f d.time }

let modify_uses g =
  let current_read = ref 0 in
  let current_write = ref 0 in
  let extra_vars = ref [] in

  let modify_use e =
    let ts_count = ref (-1) in
    let ts _ = E.Var ("TS" ^ string_of_int !ts_count, E.Int) in
    let nts _ = E.Var ("TS" ^ string_of_int (!ts_count + 1), E.Int) in

    let reads = ref [] in
    let writes = ref [] in
    let extra_eqs = ref [] in
    let special = function
      (** Record any array selects. They should also be uniquely named. The selection
          itself should be replaced with a new read destination. *)
      | E.FieldSelect ((name, s), e) ->
        let suffix = string_of_int !current_read in
        let field = name in
        let dst = ("rdst" ^ suffix, s) in
        let time = ("rtime" ^ suffix, E.Int) in

        current_read := !current_read + 1;
        reads := { name = field; var = dst; time = time } :: !reads;
        extra_vars := [dst; time] @ !extra_vars;

        extra_eqs := E.mk_eq (E.Var time) (E.FieldSelect ((name, s), e)) :: !extra_eqs;

        Some (E.Var dst)

      | E.FieldStore ((name, s), i, e) ->
        let suffix = string_of_int !current_write in
        let field = name in
        let src = ("wsrc" ^ suffix, E.sort_of e) in
        let time = ("wtime" ^ suffix, E.Int) in

        current_write := !current_write + 1;
        ts_count := !ts_count + 1;
        writes := { name = field; var = src; time = time } :: !writes;
        extra_vars := [src; time] @ !extra_vars;

        extra_eqs :=
          [ E.mk_eq (E.Var time) (ts ())
          ; E.mk_eq (nts ()) (E.mk_add (ts ()) (E.Int_lit 1))
          ; E.mk_eq (E.Var src) e
          ] @ !extra_eqs;
        Some (E.FieldStore ((name, s), i, (ts ())))

      | _ -> None
    in

    let change e =
      let e' = E.map special e in
      match e' with
        | E.Bi_op (E.Impl, e1, e2) -> E.mk_impl (E.mk_and (e1 :: !extra_eqs)) e2
        | e -> E.mk_and (e :: !extra_eqs)
    in


    let e' = match e with
    | PG.Assert (e, at) -> PG.Assert (change e, at)
    | PG.Body e -> PG.Body (change e) in
    (!reads, !writes, e')

  in
  let g = G.map_edges modify_use g in

  G.map_nodes (fun (lbl, vs) -> (lbl, !extra_vars @ vs)) g
