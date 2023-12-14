let hd lista =
  match lista with
  | [] -> failwith "Lista vacía, no hay cabeza"
  | x :: _ -> x;;

let tl lista =
  match lista with
  | [] -> failwith "Lista vacía, no hay cola"
  | _ :: resto -> resto;;

let length lista =
  let rec length_aux acc = function
    | [] -> acc
    | _ :: resto -> length_aux (acc + 1) resto
  in
  length_aux 0 lista;;

let rec compare_lengths lista1 lista2 =
  match (lista1, lista2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | _ :: resto1, _ :: resto2 -> compare_lengths resto1 resto2;;

let compare_length_with lista n =
  let rec length_compare acc = function
    | 0 -> acc
    | m when m > 0 -> length_compare (acc + 1) (m - 1)
    | _ -> invalid_arg "compare_length_with: argumento negativo"
  in
  length_compare 0 n - length lista;;

let init n f =
  let rec aux acc i =
    if i < n then
      aux (f i :: acc) (i + 1)
    else
      acc
  in
  aux [] 0;;
  
let rec nth lista n =
  match (lista, n) with
  | [], _ -> failwith "Índice fuera de rango"
  | x :: _, 0 -> x
  | _ :: resto, n when n > 0 -> nth resto (n - 1)
  | _ -> failwith "Índice fuera de rango";;

let rec append lista1 lista2 =
  match lista1 with
  | [] -> lista2
  | x :: resto -> x :: append resto lista2;;

let rev_append lista1 lista2 =
  let rec aux acc = function
    | [] -> acc
    | x :: resto -> aux (x :: acc) resto
  in
  aux lista2 lista1;;

let rev lista =
  let rec aux acc = function
    | [] -> acc
    | x :: resto -> aux (x :: acc) resto
  in
  aux [] lista;;

let rec concat listas =
  let rec append acc lista =
    match lista with
    | [] -> acc
    | x :: resto -> append (acc @ [x]) resto
  in
  match listas with
  | [] -> []
  | lista :: resto -> append lista (concat resto);;

let rec flatten listas =
  match listas with
  | [] -> []
  | lista :: resto -> append lista (flatten resto)
and append lista1 lista2 =
  match lista1 with
  | [] -> lista2
  | x :: resto -> x :: append resto lista2;;

let rec split lista =
  match lista with
  | [] -> ([], [])
  | (x, y) :: resto ->
      let xs, ys = split resto in
      (x :: xs, y :: ys);;

let rec combine lista1 lista2 =
  match (lista1, lista2) with
  | ([], []) -> []
  | (x::xs, y::ys) -> (x, y) :: combine xs ys
  | _ -> failwith "Listas de longitudes diferentes";;

let rec map f lista =
  match lista with
  | [] -> []
  | x :: resto -> f x :: map f resto;;

let rec map2 f lista1 lista2 =
  match (lista1, lista2) with
  | ([], []) -> []
  | (x :: resto1, y :: resto2) -> f x y :: map2 f resto1 resto2
  | _ -> failwith "Listas de longitudes diferentes";;

let rev_map f lista =
  let rec aux acc = function
    | [] -> acc
    | x :: resto -> aux (f x :: acc) resto
  in
  aux [] lista;;

let rec for_all f lista =
  match lista with
  | [] -> true
  | x :: resto -> f x && for_all f resto;;

let rec exists f lista =
  match lista with
  | [] -> false
  | x :: resto -> f x || exists f resto;;

let rec mem elemento lista =
  match lista with
  | [] -> false
  | x :: resto -> x = elemento || mem elemento resto;;

let rec find condicion lista =
  match lista with
  | [] -> raise Not_found
  | x :: resto -> if condicion x then x else find condicion resto;;

let rec filter condicion lista =
  match lista with
  | [] -> []
  | x :: resto -> if condicion x then x :: filter condicion resto else filter condicion resto;;

let find_all = filter;;

let partition condicion lista =
  let rec aux acc_true acc_false = function
    | [] -> (List.rev acc_true, List.rev acc_false)
    | x :: resto ->
        if condicion x then
          aux (x :: acc_true) acc_false resto
        else
          aux acc_true (x :: acc_false) resto
  in
  aux [] [] lista;;

let rec fold_left f acumulador lista =
  match lista with
  | [] -> acumulador
  | x :: resto -> fold_left f (f acumulador x) resto;;

let rec fold_right f lista acumulador =
  match lista with
  | [] -> acumulador
  | x :: resto -> f x (fold_right f resto acumulador);;

let rec assoc clave lista =
  match lista with
  | [] -> raise Not_found
  | (k, v) :: resto -> if k = clave then v else assoc clave resto;;

let rec mem_assoc clave lista =
  match lista with
  | [] -> false
  | (k, _) :: resto -> k = clave || mem_assoc clave resto;;

let rec remove_assoc clave lista =
  match lista with
  | [] -> []
  | (k, v) :: resto ->
      if k = clave then
        resto
      else
        (k, v) :: remove_assoc clave resto;;




