type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;
type 'a array = Array of 'a btree * int;;

let aempty = Array (Leaf, 0);;
let a = aempty;;

let asub arr id =
    let Array (arr, maxid) = arr
    in let rec asub_ node id =
        let Node (left, value, right) = node
        in match id with
            | 1 -> value
            | _ ->
                if id mod 2 = 0 then asub_ left (id / 2)
                else asub_ right (id / 2)

    in if 0 >= id || id > maxid then failwith "asub"
    else asub_ arr id;;

let b = asub (Array (Node (Leaf, 7, Leaf), 1)) 1;;

let aupdate arr id value =
    let Array (arr, maxid) = arr
    in let rec aupdate_ node id value =
        let Node (left, orig, right) = node
        in match id with
            | 1 -> Node (left, value, right)
            | _ ->
                if id mod 2 = 0 then Node (aupdate_ left (id / 2) value, orig, right)
                else Node (left, orig, aupdate_ right (id / 2) value)

    in if 0 >= id || id > maxid then failwith "aupdate"
    else Array (aupdate_ arr id value, maxid);;

let c = aupdate (Array (Node (Leaf, 7, Leaf), 1)) 1 1;;

let ahiext arr value =
    let Array (arr, maxid) = arr
    in let rec ahiext_ node id value = match id with
        | 1 ->
            if node != Leaf then failwith "ahiext_"
            else Node (Leaf, value, Leaf)
        | _ ->
            let Node (left, orig, right) = node
            in if id mod 2 = 0 then Node (ahiext_ left (id / 2) value, orig, right)
            else Node (left, orig, ahiext_ right (id / 2) value)

    in let newmax = maxid + 1
    in Array (ahiext_ arr newmax value, newmax);;

let d = ahiext (Array (Leaf, 0)) 9;;

let ahirem arr =
    let Array (arr, maxid) = arr
    in let rec ahirem_ node id = match id with
        | 1 ->
            let Node (left, orig, right) = node
            in if left != Leaf || right != Leaf then failwith "ahirem_"
            else Leaf
        | _ ->
            let Node (left, orig, right) = node
            in if id mod 2 = 0 then Node (ahirem_ left (id / 2), orig, right)
            else Node (left, orig, ahirem_ right (id / 2))

    in Array (ahirem_ arr maxid, maxid - 1);;

let e = ahirem (Array (Node (Leaf, 7, Leaf), 1));;
let f = ahiext e 42;;
let g = asub f 1;;
let h = aupdate f 1 66;;
let i = asub h 1;;
let j = ahirem h;;
