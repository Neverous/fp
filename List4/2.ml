type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let balanced tree =
    let rec balanced_ tree = match tree with
        | Leaf                  -> (true, 0)
        | Node(left, _, right)  ->
            let (left_result, left_count) = balanced_ left
            and (right_result, right_count) = balanced_ right
            in (left_result &&
                right_result &&
                left_count - right_count <= 1 &&
                right_count - left_count <= 1, left_count + right_count + 1)
    in let (result, _ ) = balanced_ tree
    in result
;;

balanced Leaf;;
balanced (Node(Leaf, 0, Node(Leaf, 0, Leaf)));;
balanced (Node(Node(Leaf, 0, Leaf), 0, Node(Leaf, 0, Leaf)));;

let list2tree xs =
    let rec list2tree_ xs length = match length with
        | 0         -> (xs, Leaf)
        | length    ->
            let (next, left) = list2tree_ (List.tl xs) (length / 2)
            in let (next, right) = list2tree_ next ((length - 1) / 2)
            in (next, Node(left, List.hd xs, right))
    in let (_, tree) = list2tree_ xs (List.length xs)
    in tree
;;

list2tree [];;
list2tree [1];;
list2tree [1;2];;
list2tree [1;2;3];;
list2tree [1;2;3;4];;
list2tree [1;2;3;4;5];;
balanced (list2tree [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16]);;
