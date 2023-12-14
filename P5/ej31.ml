let g1 n =
  if n >= 0 then
    if n mod 2 = 0 then
      true
    else
      false
  else
    n mod 2 = -1;;

let g2 n =
  match n >= 0, n mod 2 with
  | true, 0 | false, -1 -> true
  | _ -> false;;