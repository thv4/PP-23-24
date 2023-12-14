let rec insert_t x lst = match lst with
  | [] -> [x]
  | h::t -> if x <= h then x :: lst
            else h :: insert_t x t

let rec isort_t lst = match lst with
  | [] -> []
  | h::t -> insert_t h (isort_t t)

let rec rlist n =
  if n <= 0 then []
  else Random.int 100 :: rlist (n - 1)

let lc1 = List.init 10000 (fun x -> x)
let lc2 = List.init 20000 (fun x -> x)
let ld1 = List.init 10000 (fun x -> 10000 - x)
let ld2 = List.init 20000 (fun x -> 20000 - x)
let lr1 = rlist 10000
let lr2 = rlist 20000

let bigl = rlist 10000

let isort_g cmp lst =
  let rec insert_g x lst = match lst with
    | [] -> [x]
    | h::t -> if cmp x h then x :: h :: t
              else h :: insert_g x t
  in
  let rec isort_g' lst acc = match lst with
    | [] -> acc
    | h::t -> isort_g' t (insert_g h acc)
  in
  isort_g' lst []

let bigl2 = rlist 10000

let rec split_t lst = match lst with
  | h1::h2::t -> 
    let lst1, lst2 = split_t t in
    (h1::lst1, h2::lst2)
  | _ -> ([], [])

let rec merge_t (lst1, lst2) acc = match lst1, lst2 with
  | [], l | l, [] -> List.rev_append acc l
  | h1::t1, h2::t2 -> if h1 <= h2 then merge_t (t1, lst2) (h1::acc)
                     else merge_t (lst1, t2) (h2::acc)

let rec msort' lst = match lst with
  | [] | [_] -> lst
  | _ -> let l1, l2 = split_t lst [] [] in
  merge_t (msort' l1, msort' l2) [];

let bigl3 = [];

let msort_g cmp lst =
  let rec msort_g' lst = match lst with
    | [] | [_] -> lst
    | _ -> let l1, l2 = split_t lst in
           merge_t (msort_g' l1, msort_g' l2)
  in msort_g' lst;
