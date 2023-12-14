let p x = 2. *. 3.1415 *. x;;

let area x = 3.1415 *. x *. x;;

let absf x = if x >= 0. then x else -.x;;

let even x = (x mod 2) = 0;;

let next3 x = if x mod 3 = 0 then x else x + (3 - (x mod 3));;

let is_a_letter x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z');;

let string_of_bool x = if x then "verdadero" else "falso";;

