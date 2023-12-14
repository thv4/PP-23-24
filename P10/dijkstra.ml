       
exception Invalid_argument of string

let dijkstra graph =
  let n = Array.length graph in
  (* Verificar que la matriz sea cuadrada *)
  if not (Array.for_all (fun row -> Array.length row = n) graph) then
    raise (Invalid_argument "dijkstra: Matriz no cuadrada")
  else
    (* Verificar que no haya valores negativos en la matriz *)
    if Array.exists (fun row -> Array.exists (fun weight -> match weight with Some w -> w < 0 | None -> false) row) graph then
      raise (Invalid_argument "dijkstra: Matriz con valores negativos")
   else
      let dist = Array.make_matrix n n None in
      let rec dijkstra_util visited current =
        if not visited.(current) then begin
          visited.(current) <- true;
          for neighbor = 0 to n - 1 do
            match graph.(current).(neighbor) with
            | Some weight ->
              (match dist.(0).(current) with
               | Some d ->
                 (match dist.(0).(neighbor) with
                  | Some d' when d + weight < d' -> dist.(0).(neighbor) <- Some (d + weight)
                  | None -> dist.(0).(neighbor) <- Some (d + weight)
                  | _ -> ())
               | None -> ());
              dijkstra_util visited neighbor
            | None -> ()
          done
        end
      in
      for start_node = 0 to n - 1 do
        let visited = Array.make n false in
        dist.(0).(start_node) <- Some 0;
        dijkstra_util visited start_node;
      done;
      dist 
