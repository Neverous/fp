type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let prod tree =
    let rec prod_cont tree cont = match tree with
        | Leaf                      -> cont 1
        | Node(left, node, right)   -> prod_cont right (fun res -> cont (res * node * (prod_cont left (fun res -> res))))
    in prod_cont tree (fun res -> res)
;;

let prod2 tree =
    let rec prod_cont tree cont = match tree with
        | Leaf                      -> cont 1
        | Node(left, node, right)   ->
            if node = 0 then 0
            else prod_cont right (fun res -> cont (res * node * (prod_cont left (fun res -> res))))
    in prod_cont tree (fun res -> res)
;;

let t1 = Node(Leaf, 7, Leaf);;
let t2 = Node(Node(Leaf, 3, Leaf), 2, Leaf);;
let t3 = Node(Node(Leaf, 1, Node(Leaf, 2, Leaf)), 3, Node(Leaf, 4, Leaf));;
let t4 = Node(Node(Leaf, 1, Node(Leaf, 2, Leaf)), 3, Node(Leaf, 4, Node(Leaf, 5, Leaf)));;
let t5 = Node(Node(Leaf, 1, Node(Leaf, 0, Leaf)), 3, Node(Leaf, 4, Node(Leaf, 5, Leaf)));;

prod t1;;
prod t2;;
prod t3;;
prod t4;;
prod t5;;

prod2 t1;;
prod2 t2;;
prod2 t3;;
prod2 t4;;
prod2 t5;;
