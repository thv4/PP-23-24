let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1

let rec orbit n = if n = 1 then "1" else string_of_int n ^ ", " ^ orbit(f n)

let rec length n = if n = 1 then 0 else 1 + length(f n)

let rec top n = if n = 1 then 0 else max n (top(f n))

let rec length'n'top n =
    if n = 1 then (0,1)
    else let largo, maximo = length'n'top (f n)
    in (largo + 1, max n maximo)

let rec longest_in m n =
  if m > n then
    failwith "Invalid input: m should be less than or equal to n"
  else if m = n then
    (m, length m)
  else
    let (len_m, _) = length'n'top m in
    let (len_n, _) = length'n'top n in
    if len_m >= len_n then
      (m, len_m)
    else
      longest_in (m + 1) n

let rec highest_in m n =
  if m > n then
    failwith "Invalid input: m should be less than or equal to n"
  else if m = n then
    (m, top m)
  else
    let (_, top_m) = length'n'top m in
    let (_, top_n) = length'n'top n in
    if top_m >= top_n then
      (m, top_m)
    else
      highest_in (m + 1) n