let rec range a b =
    if a > b then []
    else a :: (range (a + 1) b)
;;

let isQueenSafe oldqs newq =
    let rec nodiag = function
        | (i, [])       -> true
        | (i, q :: qs)  -> abs(newq - q) <> i && nodiag (i + 1, qs)
    in not (List.mem newq oldqs) && nodiag (1, oldqs)
;;
let nextQueen n qs = List.map (function h -> h :: qs) (List.filter (isQueenSafe qs) (range 1 n));;
let isSolution n qs = List.length qs = n;;

let findQueens n =
    let rec queens n result =
        if List.length result > n then []
        else if isSolution n result then [result]
        else List.concat (List.map (queens n) (nextQueen n result))
    in queens n []
;;
