(*funciones recursivas mutuamente relacionadas*)

let rec par n = 
        n = 0 || impar(n-1)
and impar n = 
        n <> 0 && par(n-1);;
