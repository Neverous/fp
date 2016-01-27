let rev xs =
  let rec rev_tail xs acc = match xs with
    | []      -> acc
    | x :: xs -> rev_tail xs (x :: acc)
  in rev_tail xs []
;;

let suf xs =
  let rec sufrev_tail xs acc = match xs with
    | []        -> []
    | [_]       -> acc
    | _ :: xs   -> sufrev_tail xs (xs :: acc)
  in rev (sufrev_tail xs [xs])
;;

let pref xs =
  let rec pref_tail xs acc last = match xs with
    | []      -> acc
    | x :: xs ->
        let last = last @ [x]
        in pref_tail xs (last :: acc) last
  in rev (pref_tail xs [] [])
;;

rev [];;
rev [1];;
rev [1;2;3;4;5];;

suf [];;
suf [1];;
suf [1;2];;
suf [1;2;3];;

pref [];;
pref [1];;
pref [1;2];;
pref [1;2;3];;
