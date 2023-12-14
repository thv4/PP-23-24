let rec power x y = if y = 0 then 1 else x  * power x (y - 1);;

let rec power' x y = if y = 0 then 1 else if y mod 2 = 0 then power (x * x) (y/2) else x * power (x * x) (y/2);;

let rec powerf (x : float) (n : int) : float = if n = 0 then 1.0 else x *. powerf x (n - 1);;

