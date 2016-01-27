type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfilter pred = function
    | LNil                  -> LNil
    | LCons (x, lazy xs)    ->
        if pred x then LCons(x, lazy (lfilter pred xs))
        else lfilter pred xs
;;

let rec ltake = function
    | (0, _)                    -> []
    | (_, LNil)                 -> []
    | (n, LCons (x, lazy xs))   -> x :: ltake (n - 1, xs)
;;

let depthFirst next x =
    let rec dfs = function
        | []        -> LNil
        | x :: xs   -> LCons (x, lazy (dfs ((next x) @ xs)))
    in dfs [x]
;;

let breadthFirst next x =
    let rec bfs = function
        | []        -> LNil
        | x :: xs   -> LCons (x, lazy (bfs (xs @ (next x))))
    in bfs [x]
;;

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
let breadthQueen n = lfilter (isSolution n) (breadthFirst (nextQueen n) []);;
let depthQueen n = lfilter (isSolution n) (depthFirst (nextQueen n) []);;

ltake (10, breadthQueen 8);;
ltake (10, depthQueen 8);;
