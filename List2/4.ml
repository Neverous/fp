let rev xs =
  let rec rev_tail xs acc = match xs with
    | []      -> acc
    | x :: xs -> rev_tail xs (x :: acc)
  in rev_tail xs []
;;

let rec merge cmp xs ys =
  match (xs, ys) with
    | ([], zs)              -> zs
    | (zs, [])              -> zs
    | (x :: xss, y :: yss)  ->
        if cmp x y then x :: merge cmp xss ys
                   else y :: merge cmp xs yss
;;

let merge2 cmp xs ys =
  let rec merge2_tail xs ys res =
    match (xs, ys) with
      | ([], [])              -> rev res
      | ([], z :: zs)         -> merge2_tail [] zs (z :: res)
      | (z :: zs, [])         -> merge2_tail [] zs (z :: res)
      | (x :: xss, y :: yss)  ->
            if cmp x y then merge2_tail xss ys (x :: res)
                       else merge2_tail xs yss (y :: res)
  in merge2_tail xs ys []
;;

let rec halve = function
  | []        -> [], []
  | [_] as xs -> xs, []
  | x :: xs   ->
        let p1, p2 = halve xs
        in x :: p2, p1
;;

let rec merge_sort cmp = function
  | []        -> []
  | [_] as xs -> xs
  | xs        ->
        let p1, p2 = halve xs
        in merge cmp (merge_sort cmp p1) (merge_sort cmp p2)
;;

merge (<=) [1;3;5] [2;4;6];;
merge2 (<=) [1;3;5] [2;4;6];;
merge_sort (<=) [1;7;9;2;11;5;8;4;3;2];;
