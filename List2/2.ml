let rec fun_ = function
    | 0 -> 1
    | 1 -> 2
    | n -> 2 * fun_ (n - 2) - fun_ (n - 1) + 1
;;

let fun2_ n =
    let rec fun2_tail n prev prev2 = match n with
        | 0 -> prev2
        | n -> fun2_tail (n - 1) (2 * prev2 - prev + 1) prev
    in fun2_tail n 2 1
;;

fun2_ 64;;
fun_ 64;;
