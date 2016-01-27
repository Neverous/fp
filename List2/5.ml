let rev xs =
  let rec rev_tail xs acc = match xs with
    | []      -> acc
    | x :: xs -> rev_tail xs (x :: acc)
  in rev_tail xs []
;;

let maprev function_ list_ =
    let rec maprev_tail list_ acc = match list_ with
        | []      -> acc
        | x :: t  -> maprev_tail t (function_ x :: acc)
    in maprev_tail list_ []
;;

let maprev_fold function_ list_ =
    let rec maprev_tail list_ acc = match list_ with
        | []      -> acc
        | x :: t  -> maprev_tail t (function_ x @ acc)
    in maprev_tail list_ []
;;

let rec split xs =
    let rec splitrev_tail ls rs acc = match ls with
        | []        -> ([], rs) :: acc
        | k :: ks   -> splitrev_tail ks (k :: rs) (((rev ls), rs) :: acc)
    in rev (splitrev_tail (rev xs) [] [])
;;

let insert1 a xs =
    let splits = split xs
    in maprev (fun (ls, rs) -> ls @ (a :: rs)) splits
;;

let insert2 a xss = rev (maprev_fold (fun xs -> insert1 a xs) xss)
;;

let rec perm = function
    | []        -> [[]]
    | x :: xs   -> insert2 x (perm xs);;

rev [1;2;3;4;5];;
split [1;2;3];;
insert1 0 [1;2;3];;
insert2 0 [[1]; [2;3]; []];;
perm [1;2;3];;
