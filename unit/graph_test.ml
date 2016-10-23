module G = Graph.Make(
  struct
    type node = int
    type edge = char
  end)

let print_g = G.display string_of_int (String.make 1)

let test_merge_strictly_connected =
  let original = G.of_list
      [ (1, 3, 'a')
      ; (2, 3, 'b')
      ; (3, 4, 'c')
      ; (4, 5, 'd')
      ; (4, 6, 'e')
      ] in

  let expected = G.of_list
      [ (1, 7, 'a')
      ; (2, 7, 'b')
      ; (7, 5, 'd')
      ; (7, 6, 'e')
      ] in

  let merged =
    G.merge_strictly_connected (fun (i, t, e) -> Some (i + t)) original
  in
  G.diff merged expected = G.empty

