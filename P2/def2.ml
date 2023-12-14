let p = function x -> 2. *. 3.1415 *. x;;

let area = function x -> 3.1415 *. x *. x;;

let absf = function x -> if x >= 0. then x else -.x;;

let even = function x -> (x mod 2) = 0;;

let next3 = function x -> if x mod 3 = 0 then x else x + (3 - (x mod 3));;

let is_a_letter x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z');;

let string_of_bool x = if x then "verdadero" else "falso";;
