module type PQUEUE =
sig
    type priority
    type 'a t

    exception EmptyPQueue

    val empty: 'a t
    val insert: 'a t -> priority -> 'a -> 'a t
    val remove: 'a t -> priority * 'a * 'a t
end;;

module PQueue : PQUEUE with type priority = int  =
struct
    type priority = int
    type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

    exception EmptyPQueue

    let empty = Empty;;
    let rec insert que prio elem = match que with
        | Empty                                     -> Node (prio, elem, Empty, Empty)
        | Node (node_prio, node_elem, left, right)  ->
            if prio <= node_prio then Node (node_prio, node_elem, insert left prio elem, right)
            else Node (node_prio, node_elem, left, insert right prio elem)

    let rec remove que = match que with
        | Empty -> raise EmptyPQueue
        | Node (prio, elem, Empty, right)   -> (prio, elem, right)
        | Node (prio, elem, left, right)    ->
            let (res_prio, res_elem, res_left) = remove left
            in (res_prio, res_elem, Node (prio, elem, res_left, right))
end;;

let queue       = PQueue.empty;;
let queue'      = PQueue.insert queue 7 "test7";;
let queue''     = PQueue.insert queue' 8 "test8";;
let queue'''    = PQueue.insert queue'' 5 "test5";;
let (prio, elem, _) = PQueue.remove queue''';;
let (prio, elem, _) = PQueue.remove queue'';;
let (prio, elem, _) = PQueue.remove queue';;

let sort xs =
    let rec fill xs que = match xs with
        | []        -> que
        | x :: xs   -> fill xs (PQueue.insert que x x)

    in let rec drain que xs =
        try
            let (prio, elem, que) = PQueue.remove que
            in drain que (elem :: xs)

        with
            PQueue.EmptyPQueue -> List.rev xs

    in drain (fill xs PQueue.empty) [];;

sort [1;5;8;2;3;7;4];;


module type ORDTYPE =
sig
    type t
    val compare: t -> t -> int
end;;

module PQueue (OrdType: ORDTYPE) : PQUEUE with type priority = OrdType.t =
struct
    type priority = OrdType.t
    type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

    exception EmptyPQueue

    let empty = Empty;;
    let rec insert que prio elem = match que with
        | Empty                                     -> Node (prio, elem, Empty, Empty)
        | Node (node_prio, node_elem, left, right)  ->
            if OrdType.compare prio node_prio <= 0 then Node (node_prio, node_elem, insert left prio elem, right)
            else Node (node_prio, node_elem, left, insert right prio elem)

    let rec remove que = match que with
        | Empty -> raise EmptyPQueue
        | Node (prio, elem, Empty, right)   -> (prio, elem, right)
        | Node (prio, elem, left, right)    ->
            let (res_prio, res_elem, res_left) = remove left
            in (res_prio, res_elem, Node (prio, elem, res_left, right))
end;;

module IntOrd =
struct
    type t = int
    let compare a b = a - b
end;;

module IntPQueue = PQueue(IntOrd);;

let sort xs =
    let rec fill xs que = match xs with
        | []        -> que
        | x :: xs   -> fill xs (IntPQueue.insert que x x)

    in let rec drain que xs =
        try
            let (prio, elem, que) = IntPQueue.remove que
            in drain que (elem :: xs)

        with
            IntPQueue.EmptyPQueue -> List.rev xs

    in drain (fill xs IntPQueue.empty) [];;

sort [1;5;8;2;3;7;4];;
