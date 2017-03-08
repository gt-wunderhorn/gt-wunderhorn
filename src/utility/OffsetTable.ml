type t = (int * int) list

let mk : t = [(0, 0)]

let add index off = function
  | ((idx', off') :: rest) -> (index, off + off') :: (idx', off') :: rest
  | []                     -> [index, off; 0, 0]

(**
 * find the first offset in the table which has a matching index at or below
 * the query
 *)
let rec lookup table idx = match table with
  | ((idx', off) :: rest) ->
    if idx >= idx' then off
    else lookup rest idx
  | _ -> 0
