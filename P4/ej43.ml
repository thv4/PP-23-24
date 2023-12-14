let rec reverse n =
  if n < 10 then n
  else
    let ultimo_digito = n mod 10 in
    let digitos_restantes = n / 10 in
    let reverse_restantes = reverse digitos_restantes in
    let reverse_strings = string_of_int ultimo_digito ^ string_of_int reverse_restantes in
    int_of_string reverse_strings;;

let rec palindromo s =
  let len = String.length s in
  if len <= 1 then true 
  else
    s.[0] = s.[len - 1] && palindromo (String.sub s 1 (len - 2));;

let rec mcd (x, y) =
  if y = 0 || x = 0 then x  
  else mcd ( x mod y,y);;  

