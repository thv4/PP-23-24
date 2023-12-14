let rec es_seguro fila col sol =
  let es_seguro_fila r c = not (List.exists (fun (x, y) -> x = r) sol) in
  let es_seguro_col r c = not (List.exists (fun (x, y) -> y = c) sol) in
  let es_seguro_diag r c = not (List.exists (fun (x, y) -> abs (x - r) = abs (y - c)) sol) in
  es_seguro_fila fila col && es_seguro_col fila col && es_seguro_diag fila col

let rec ubicar_reinas n fila sol =
  if fila > n then [sol]
  else
    List.fold_left
      (fun acc col ->
        if es_seguro fila col sol then
          acc @ ubicar_reinas n (fila + 1) (sol @ [(fila, col)])
        else
          acc)
      [] (List.init n (fun x -> x + 1))

let queens n =
  if n < 0 then [[]]
  else ubicar_reinas n 1 []

let is_queens_sol n sol =
  List.length sol = n && List.for_all (fun (fila, col) -> 1 <= fila && fila <= n && 1 <= col && col <= n) sol
    && List.length (List.sort_uniq compare sol) = n
    && List.for_all (fun reina -> es_seguro (fst reina) (snd reina) sol) sol

let verificar n = List.for_all (is_queens_sol n) (queens n);;

List.map (fun i -> i, List.length (queens i)) (List.init 13 abs);;
