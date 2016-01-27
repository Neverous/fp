type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree

let ex1 = Node (Node (Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e');;
let ex2 = Node (Leaf 'e', 'd', Node (Leaf 'a', 'b', Leaf 'c'));;

let preorder tree =
    let rec preorder_ tree idx = match tree with
        | Leaf _                -> (Leaf idx, idx + 1)
        | Node (left, _, right) ->
            let (left, idxl) = preorder_ left (idx + 1)
            in let (right, idxr) = preorder_ right idxl
            in (Node(left, idx, right), idxr)
    in let (result, _) = preorder_ tree 1
    in result;;

preorder ex1;;

let bfsorder tree =
    let rec bfsorder_ trees id cont = match trees with
        | []            -> cont id []
        | tree :: trees -> (match tree with
            | Leaf _                ->
                bfsorder_ trees (id + 1)
                    (fun id order -> cont (id - 1) (order @ [Leaf id]))
            | Node (left, _, right) ->
                bfsorder_ (trees @ [left; right]) (id + 1)
                    (fun id (right :: left :: order) -> cont (id - 1) (order @ [Node (left, id, right)]))
        )
    in bfsorder_ [tree] 0 (fun _ [order] -> order);;

bfsorder ex1;;
bfsorder ex2;;
