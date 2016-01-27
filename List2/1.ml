let maprev function_ list_ =
    let rec maprev_tail list_ acc = match list_ with
        | []      -> acc
        | x :: t  -> maprev_tail t (function_ x :: acc)
    in maprev_tail list_ []
;;

let rev xs =
  let rec rev_tail xs acc = match xs with
    | []      -> acc
    | x :: xs -> rev_tail xs (x :: acc)
  in rev_tail xs []
;;

let map cmp list_ = rev (maprev cmp list_)
;;

let rec subseqs = function
    | []        -> [[]]
    | x :: t    ->
        let st = subseqs t
        in map (fun y -> x :: y) st @ st
;;

subseqs [1;2;3;4;5];;
