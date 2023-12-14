let rec sumto = function 0 -> 0 | n -> n + sumto (n-1);;

let rec exp = function 0 -> 1 | n -> 10 * exp (n - 1);;

let rec num_cifras = function 0 -> 0 | n -> 1 + num_cifras(Int.abs(n)/10);;

let rec sum_cifras = function 0 -> 0 | n -> (Int.abs(n) mod 10) + sum_cifras(Int.abs(n)/10);;