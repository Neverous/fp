type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree

let ex1 = Node (Node (Leaf 1, Leaf 2), Leaf 3);;
let ex2 = Node (Leaf 1, Node (Leaf 2, Leaf 3));;
let ex3 = Node (Leaf 7, Node (Leaf 2, Leaf 3));;

let border_list tree =
    let rec border_list_ tree res = match tree with
        | Leaf x                -> x :: res
        | Node (left, right)    ->
            let leftres = border_list_ left res
            in border_list_ right leftres
    in List.rev (border_list_ tree []);;

let same_border tree1 tree2 = (border_list tree1) = (border_list tree2);;
let samefringe tree1 tree2 =
    let rec getleaf tree trees = match tree with
        | Leaf x                -> (x, trees)
        | Node (left, right)    -> getleaf left (right :: trees)
    in let getnext = function
        | []            -> failwith "getnext"
        | tree :: trees -> getleaf tree trees
    in let rec samefringe_ tree trees = match tree with
        | Leaf x                ->
            let (y, trees) = getnext trees
            in if x != y then (false, [])
            else (true, trees)
        | Node (left, right)    ->
            let (res, trees) = samefringe_ left trees
            in if not res then (false, [])
            else samefringe_ right trees
    in let (result, _) = samefringe_ tree1 [tree2]
    in result;;

border_list ex1;;
border_list ex2;;
same_border ex1 ex2;;
samefringe ex1 ex2;;
samefringe ex1 ex3;;
