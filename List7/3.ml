type 'a array = Array of 'a list;;
let aempty = Array [];;
let afind (Array arr) arg =
    let rec _afind arr arg = match arr with
        | []                    -> None
        | (key, value) :: arr   -> if key = arg then Some (value) else _afind arr arg
    in _afind arr arg
;;

let aadd (Array arr) key value = Array ((key, value) :: arr);;

let rec fib n = if n < 2 then n else (fib (n - 2)) + (fib (n - 1));;
let fib_memo =
    let memo = ref aempty
    in let rec fib n =
        if n < 2 then n
        else
            let value = afind !memo n
            in match value with
                | None ->
                    let result = (fib (n - 2)) + (fib (n - 1))
                    in let () = memo := aadd !memo n result
                    in result
                | Some result -> result
    in fib
;;
