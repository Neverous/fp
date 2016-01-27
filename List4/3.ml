type 'a mtree = MNode of 'a * 'a forest
and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest;;

type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list;;

let dfs tree =
    let rec search acc = function
        | []                                                -> List.rev acc
        | EmptyForest :: tail                               -> search acc tail
        | Forest(MNode(tag, children), siblings) :: tail    -> search (tag :: acc) (children :: siblings :: tail)
    in search [] [Forest(tree, EmptyForest)]
;;

let bfs tree =
    let rec search acc = function
        | []                                                -> List.rev acc
        | EmptyForest :: tail                               -> search acc tail
        | Forest(MNode(tag, children), siblings) :: tail    -> search (tag :: acc) (tail @ [siblings; children])
    in search [] [Forest(tree, EmptyForest)]
;;

let t1 = MNode(1, EmptyForest);;
let t2 = MNode(1, Forest(
    MNode(2, EmptyForest),
    Forest(
        MNode(3, Forest(
            MNode(4, EmptyForest),
            EmptyForest
        )),
        Forest(
            MNode(5, EmptyForest),
            EmptyForest
        ))
    )
);;

dfs t1;;
dfs t2;;
bfs t1;;
bfs t2;;

let dfs2 tree =
    let rec search acc = function
        | []                            -> List.rev acc
        | MTree(tag, children) :: tail  -> search (tag :: acc) (children @ tail)
    in search [] [tree]
;;

let bfs2 tree =
    let rec search acc = function
        | []                            -> List.rev acc
        | MTree(tag, children) :: tail  -> search (tag :: acc) (tail @ children)
    in search [] [tree]
;;

let t1 = MTree(1, []);;
let t2 = MTree(1, [
    MTree(2, [
        MTree(3, []);
        MTree(4, []);
        MTree(5, []);
    ]);
    MTree(6, [
        MTree(7, []);
    ]);
    MTree(8, [
        MTree(9, []);
    ]);
]);;

dfs2 t1;;
dfs2 t2;;
bfs2 t1;;
bfs2 t2;;
