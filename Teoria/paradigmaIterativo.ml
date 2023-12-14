(*variables en Ocaml*)

ref;;

let n = ref 0;;

(*! se indica el valor ej -> !n + 1
 := para asignar*)

let next () = 
        let _ = n := !n + 1 in !n;;

let next2 () = 
        n := !n + 1;
        !n;;
