exception Not_found

(*
Stack overflow during evaluation (looping recursion?). 
en las siguientes llamadas:
min_tour 5 6 [(3,1); (2,3)] (1,2) (1,1);;
min_tour 5 6 [(3,1); (2,3); (3,2); (2, 4)] (1,2) (1,1);;
min_tour 5 6 [] (0,1) (4,4);;
*)

let tour m n obstaculos ini fin =
  let movimientos = [(2, 1); (2, -1); (-2, 1); (-2, -1); (1, 2); (-1, 2); (1, -2); (-1, -2)] in
  
  let es_valido (x, y) =
    x >= 0 && x < m && y >= 0 && y < n && not (List.mem (x, y) obstaculos) in
  
  let rec recorrido camino actual =
    if actual = fin then
      camino 
    else
      let siguientes = List.filter (fun (dx, dy) ->
        let siguiente = (fst actual + dx, snd actual + dy) in
        es_valido siguiente && not (List.mem siguiente camino)
      ) movimientos in
      
      match siguientes with
      | [] -> raise Not_found
      | _ ->
        List.fold_left (fun acc mov ->
          try
            recorrido (acc @ [actual]) (fst actual + fst mov, snd actual + snd mov)
          with
            Not_found -> acc
        ) [] siguientes
  in
  
  recorrido [] ini




let min_tour m n obstaculos ini fin =
  let movimientos = [(2, 1); (2, -1); (-2, 1); (-2, -1); (1, 2); (-1, 2); (1, -2); (-1, -2)] in
  
  let es_valido (x, y) =
    x >= 0 && x < m && y >= 0 && y < n && not (List.mem (x, y) obstaculos) in
  
  let rec recorrido camino actual =
    if actual = fin then
      camino
    else
      let siguientes = List.filter (fun (dx, dy) ->
        let siguiente = (fst actual + dx, snd actual + dy) in
        es_valido siguiente && not (List.mem siguiente camino)
      ) movimientos in
      
      match siguientes with
      | [] -> raise Not_found
      | _ ->
        List.fold_left (fun acc mov ->
          try
            recorrido (acc @ [actual]) (fst actual + fst mov, snd actual + snd mov)
          with
            Not_found -> acc
        ) [] siguientes
  in
  
  let todos_los_caminos = ref [] in
  
  try
    todos_los_caminos := recorrido [] ini :: !todos_los_caminos;
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        if es_valido (i, j) && (i, j) <> ini then
          todos_los_caminos := recorrido [] (i, j) :: !todos_los_caminos;
      done;
    done;
    let caminos_ordenados = List.sort (fun camino1 camino2 ->
      compare (List.length camino1) (List.length camino2)
    ) !todos_los_caminos in
    List.hd caminos_ordenados
  with
    Not_found -> raise Not_found

(* Ejemplos *)
let resultado1 = min_tour 5 6 [] (1, 2) (1, 1);;
let resultado2 = min_tour 5 6 [(3, 1)] (1, 2) (1, 1);;
let resultado3 = min_tour 5 6 [(3, 1); (2, 3)] (1, 2) (1, 1);;
let resultado4 = min_tour 5 6 [(3, 1); (2, 3); (3, 2); (2, 4)] (1, 2) (1, 1);;
let resultado5 = min_tour 5 6 [] (0, 1) (4, 4);;
