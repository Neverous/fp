let counter =
    let x = ref 0
    in fun n ->
        x := !x + n;
        !x
;;

let fresh str = str ^ string_of_int (counter 1)
let reset value = counter (value - (counter 0)); ();;
