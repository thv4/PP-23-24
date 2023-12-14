
type 'a bintree = Empty | Node of 'a * 'a bintree * 'a bintree

(* Función in_order *)
let rec in_order tree =
  match tree with
  | Empty -> []
  | Node (r, i, d) -> in_order i @ [r] @ in_order d

(* Función insert *)
let rec insert ord tree x =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (r, i, d) ->
    if ord x r then Node (r, insert ord i x, d)
    else Node (r, i, insert ord d x)

(* Función bst *)
let bst ord l =
  List.fold_left (insert ord) Empty l

(* Ejemplo de uso de bst *)
let example_tree = bst (<=) ['c'; 'a'; 's'; 'u'; 'a'; 'l']

(* Función qsort *)
let rec qsort ord lst =
  match lst with
  | [] -> []
  | x :: xs ->
    let smaller = List.filter (fun y -> ord y x) xs in
    let larger = List.filter (fun y -> not (ord y x)) xs in
    qsort ord smaller @ [x] @ qsort ord larger

(* Ejemplo de uso de qsort *)
let sorted_list = qsort (>=) [1; 0; 2; -1; 3; 10; 100]
