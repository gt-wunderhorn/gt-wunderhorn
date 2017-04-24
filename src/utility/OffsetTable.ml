type t = (int * int) list

let mk off : t = [(0, off)]

let add index off table =
  if off = 0 then table
  else match table with
  | ((idx', off') :: rest) -> (index, off + off') :: (idx', off') :: rest
  | []                     -> [index, off; 0, 0]

(**
 * find the first offset in the table which has a matching index at or below
 * the query
 *)
let rec lookup table idx =
  match table with
  | ((idx', off) :: rest) ->
    if idx >= idx' then off
    else lookup rest idx
  | _ -> 0
