type hufftree = Leaf of int * char | HuffNode of int * hufftree * hufftree;;

module HSet = Set.Make
    (struct
        type t = hufftree
        let compare = compare
    end)
;;

let build_hufftree source =
    let leaves = HSet.of_list (List.map (fun (x, y) -> Leaf (x, y)) source)
    in let rec build_tree trees =
        let first = HSet.min_elt trees
        in let trees1 = HSet.remove first trees
        in if HSet.is_empty trees1 then
            first
        else
            let second = HSet.min_elt trees1
            in let trees2 = HSet.remove second trees1
            in let c1 = match first with
                | Leaf (f1, _)          -> f1
                | HuffNode (f1, _, _)   -> f1
            in let c2 = match second with
                | Leaf (f2, _)          -> f2
                | HuffNode (f2, _, _)   -> f2
            in let concat = HuffNode(c1 + c2, first, second)
            in let trees3 = HSet.add concat trees2
            in build_tree trees3
    in
    build_tree leaves
;;

module Encoder = Map.Make(Char);;

let build_encoder tree =
    let rec build_ tree current result = match tree with
        | Leaf (_, data)            -> Encoder.add data (List.rev current) result
        | HuffNode (_, left, right) ->
            let result1 = build_ left (0 :: current) result
            in build_ right (1 :: current) result1
    in build_ tree [] Encoder.empty
;;

let encode encoder sequence = List.flatten (List.map (fun key -> Encoder.find key encoder) sequence);;

let decode tree sequence =
    let rec decode_ tree current result = function
        | []        -> List.rev result
        | x :: xs   -> match current with
            | Leaf (_, data)            -> decode_ tree tree (data :: result) xs
            | HuffNode (_, left, right) -> (
                let next = if x == 0 then left else right
                in match next with
                    | Leaf (_, data)    -> decode_ tree tree (data :: result) xs
                    | HuffNode _        -> decode_ tree next result xs)
    in decode_ tree tree [] sequence
;;

let tree = build_hufftree [(1, 'a'); (1, 'b')];;
let encoder = build_encoder tree;;

let a = encode encoder ['b'; 'a'; 'b'; 'a'];;
let b = decode tree [1;0;1;0];;
decode tree a;;
encode encoder b;;
