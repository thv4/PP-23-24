let rec fib n =
	if n <= 1 then n
	else fib (n-1) + fib (n-2)

let print_fibonacci n =
  let rec print_terminos i =
    if i <= n then begin
      print_int (fib i);
      print_newline ();
      print_terminos (i + 1)
    end
  in
  print_terminos 0

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.printf "fibto: Argumento invalido\n";
    exit 1
  end else begin
    let n = int_of_string Sys.argv.(1) in
    print_fibonacci n
  end