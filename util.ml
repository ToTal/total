let rec zip l1 l2 = match l1, l2 with
  | [], [] -> []
  | x::l1', y::l2' -> (x,y):: zip l1' l2'
  | _ -> Error.violation "zip called with lists of different length"
