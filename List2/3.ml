let maprev function_ list_ =
    let rec maprev_tail list_ acc = match list_ with
        | []      -> acc
        | x :: t  -> maprev_tail t (function_ x :: acc)
    in maprev_tail list_ []
;;

let rec maprev2 function_ = function
    | []        -> []
    | x :: t    -> maprev2 function_ t @ [function_ x]
;;

maprev (fun x -> 2 * x) [1;2;3;4;5;6;7;8;9;10];;
maprev2 (fun x -> 2 * x) [1;2;3;4;5;6;7;8;9;10];;
