module Set = Core.Std.Set.Poly

(** `converge` repeatedly applies a function `f` to an argument `x` until
    the change in `x` caused by `f` is stable. *)
let converge is_stable f x =
  let rec converge' x x' =
    if is_stable x x' then x'
    else converge' x' (f x')
  in
  converge' x (f x)

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let no_special _ = None

let specialize base special x =
  match special x with
  | Some y -> y
  | None -> base x

let union_map_list f xs = Set.union_list (List.map f xs)

let nub l = l |> Set.of_list |> Set.to_list

let lines = Core.Std.String.split_lines
let unlines = String.concat "\n"
let unwords = String.concat " "

let parens ss = "(" ^ unwords ss ^ ")"

let indent s =
  s |> lines |> List.map (fun s -> "  " ^ s) |> unlines

let block title ss =
  title ^ " {\n" ^ indent (unlines ss) ^ "\n}"

let rec span f = function
  | []        -> ([], [])
  | (x :: xs) ->
    if f x
    then let (ys, zs) = span f xs in (x :: ys, zs)
    else ([], x :: xs)

let rec groupBy f = function
  | []      -> []
  | (x::xs) ->
    let (ys, zs) = span (f x) xs in
    (x :: ys) :: groupBy f zs

let rec init = function
  | []        -> []
  | [x;y]     -> [x]
  | (x :: xs) -> x :: init xs

let rec last = function
  | []        -> assert false
  | [x]       -> x
  | (x :: xs) -> last xs

