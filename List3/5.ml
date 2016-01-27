let rec find_step compare hd tl = match (hd, tl) with
    | (_, [])               -> ([], 0, [])
    | ([], t :: tl_)        -> (tl_, t, [])
    | (h :: hd_, t :: tl_)  ->
        if compare h t < 0 then (hd_, h, tl)
        else find_step compare hd_ (h :: tl)
;;

let rec cut compare xs min (cur, rest) = match xs with
    | []        -> (cur, rest)
    | x :: xs   ->
        if compare x cur < 0 && compare min x < 0 then cut compare xs min (x, cur :: rest)
        else cut compare xs min (cur, x :: rest)
;;

let next_permutation compare = function
    | []        -> []
    | p :: ps   ->
        let (keep, point, rest) = find_step compare ps [p]
        in match rest with
            | []        -> point :: keep
            | r :: rest ->
                let (min, rest) = cut compare rest point (r, [point])
                in List.rev (List.fast_sort compare rest) @ [min] @ keep
;;

let permutations compare start =
    let rec perm start cur result =
        if start = cur then result
        else perm start (next_permutation compare cur) (cur :: result)
    in perm start (next_permutation compare start) [start]
;;

next_permutation compare [3;2;1];;
next_permutation compare [2;3;1];;
next_permutation compare [3;1;2];;
next_permutation compare [1;3;2];;
next_permutation compare [2;1;3];;
next_permutation compare [1;2;3];;

permutations compare [3;2;1];;
