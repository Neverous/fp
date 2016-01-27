type expression =
    | Var of string
    | Not of expression
    | And of expression * expression
    | Or of expression * expression
;;

let variables_of_expression expr =
    let rec vars_ acc = function
        | []                    -> List.sort_uniq compare acc
        | Var id :: tail        -> vars_ (id :: acc) tail
        | Not expr :: tail      -> vars_ acc (expr :: tail)
        | And(e1, e2) :: tail   -> vars_ acc (e1 :: e2 :: tail)
        | Or(e1, e2) :: tail    -> vars_ acc (e1 :: e2 :: tail)
    in vars_ [] [expr]
;;

variables_of_expression (Var "x");;
variables_of_expression (Or(And(Var "x", Not(Var "y")), Var "z"));;

module Mapping = Map.Make(String);;

let gen_map = List.fold_left2 (fun mapping var val_ -> Mapping.add var val_ mapping) Mapping.empty;;

let rec eval mapping = function
    | Var id        -> Mapping.find id mapping
    | Not expr      -> not (eval mapping expr)
    | And(e1, e2)   -> (eval mapping e1) && (eval mapping e2)
    | Or(e1, e2)    -> (eval mapping e1) || (eval mapping e2)
;;

let e1 = Or(And(Var "x", Not(Var "y")), Var "z");;
let e2 = Or(Not(And(Var "x", Not(Var "y"))), Var "z");;
let e3 = Or(Var "x", Not(Var "x"));;
let vars = variables_of_expression e1;;
let vals = List.map (fun _ -> true) vars;;
eval (gen_map vars vals) e1;;
eval (gen_map vars vals) e2;;

let rec mov = function
    | []            -> []
    | false :: xs   -> true :: xs
    | true :: xs    -> false :: mov xs
;;

mov vals;;

let brute_tautology expr =
    let rec brute vars vals expr =
        if eval (gen_map vars vals) expr = false then (false, List.combine vars vals)
        else if List.for_all (fun x -> x = true) vals then (true, [])
        else brute vars (mov vals) expr
    in let vars = variables_of_expression expr
    in brute vars (List.map (fun _ -> false) vars) expr
;;

brute_tautology e1;;
brute_tautology e2;;
brute_tautology e3;;

let neg_norm expr =
    let rec neg_norm expr neg = match expr with
        | Var id        -> if neg then Not(Var id) else Var id
        | Not expr      -> neg_norm expr (not neg)
        | And(e1, e2)   -> if neg then Or(neg_norm e1 true, neg_norm e2 true) else And(neg_norm e1 false, neg_norm e2 false)
        | Or(e1, e2)    -> if neg then And(neg_norm e1 true, neg_norm e2 true) else Or(neg_norm e1 false, neg_norm e2 false)
    in neg_norm expr false
;;

neg_norm e1;;
neg_norm e2;;
neg_norm e3;;

let conj_norm expr =
    let rec gen_acc = function
        | Var id        -> [Var id]
        | Not(Var id)   -> [Not(Var id)]
        | Or(e1, e2)    -> [Or(e1, e2)]
        | And(e1, e2)   -> (gen_acc e1) @ (gen_acc e2)
    in let join base = function
        | []        -> base
        | x :: xs   -> List.fold_left (fun a x -> And(a, Or(base, x))) (Or(base, x)) xs
    in let rec conj_norm acc = function
        | Var id                -> join (Var id) acc
        | Not var               -> join (Not var) acc
        | Or(e1, e2)            -> (
            let ce1 = conj_norm acc e1
            and ce2 = conj_norm acc e2
            in let acc = gen_acc ce2
            in match (ce1, ce2) with
                | (And(_, _), And(_, _))    -> conj_norm acc ce1
                | (And(_, _), _)            -> conj_norm acc ce1
                | (_, And(_, _))            -> conj_norm acc ce1
                | (_, _)                    -> Or(ce1, ce2))
        | And(e1, e2)           -> (
            let ce1 = conj_norm acc e1
            and ce2 = conj_norm acc e2
            in And(ce1, ce2))
    in conj_norm [] (neg_norm expr)
;;

conj_norm e1;;
neg_norm e2;;
conj_norm e2;;
conj_norm e3;;

let e4 = Or(And(Var "a", Var "b"), And(Var "c", Var "d"));;
let e5 = And(Var "x", Not(Var "x"));;
conj_norm e4;;
conj_norm e5;;

let positive_variables expr =
    let expr = neg_norm expr
    in let rec vars_ acc = function
        | []                    -> List.sort_uniq compare acc
        | Var id :: tail        -> vars_ (id :: acc) tail
        | Not var :: tail       -> vars_ acc tail
        | And(e1, e2) :: tail   -> vars_ acc (e1 :: e2 :: tail)
        | Or(e1, e2) :: tail    -> vars_ acc (e1 :: e2 :: tail)
    in vars_ [] [expr]
;;

let negative_variables expr =
    let expr = neg_norm expr
    in let rec vars_ acc = function
        | []                    -> List.sort_uniq compare acc
        | Var id :: tail        -> vars_ acc tail
        | (Not(Var id)) :: tail -> vars_ (id :: acc) tail
        | And(e1, e2) :: tail   -> vars_ acc (e1 :: e2 :: tail)
        | Or(e1, e2) :: tail    -> vars_ acc (e1 :: e2 :: tail)
    in vars_ [] [expr]
;;

let rec one_in xs = function
    | []        -> false
    | y :: ys   -> if List.exists (fun x -> x = y) xs then true else one_in xs ys
;;

one_in [1;2;3] [1];;

let conj_tautology expr =
    let conj = conj_norm expr
    in let rec test_conj = function
        | Or(e1, e2)    -> one_in (positive_variables (Or(e1, e2))) (negative_variables (Or(e1, e2)))
        | And(e1, e2)   -> test_conj e1 && test_conj e2
        | _             -> false
    in test_conj conj
;;

conj_tautology e1;;
conj_tautology e2;;
conj_tautology e3;;
conj_tautology e4;;
conj_tautology e5;;

let disj_norm expr =
    let rec gen_acc = function
        | Var id        -> [Var id]
        | Not(Var id)   -> [Not(Var id)]
        | Or(e1, e2)    -> [Or(e1, e2)]
        | And(e1, e2)   -> (gen_acc e1) @ (gen_acc e2)
    in let join base = function
        | []        -> base
        | x :: xs   -> List.fold_left (fun a x -> Or(a, And(base, x))) (And(base, x)) xs
    in let rec disj_norm acc = function
        | Var id                -> join (Var id) acc
        | Not var               -> join (Not var) acc
        | And(e1, e2)           -> (
            let ce1 = disj_norm acc e1
            and ce2 = disj_norm acc e2
            in let acc = gen_acc ce2
            in match (ce1, ce2) with
                | (Or(_, _), Or(_, _))  -> disj_norm acc ce1
                | (Or(_, _), _)         -> disj_norm acc ce1
                | (_, Or(_, _))         -> disj_norm acc ce1
                | (_, _)                -> And(ce1, ce2))
        | Or(e1, e2)            -> (
            let ce1 = disj_norm acc e1
            and ce2 = disj_norm acc e2
            in Or(ce1, ce2))
    in disj_norm [] (neg_norm expr)
;;

disj_norm e1;;
neg_norm e2;;
disj_norm e2;;
disj_norm e3;;

let disj_contradiction expr =
    let disj = disj_norm expr
    in let rec test_disj = function
        | And(e1, e2)   -> one_in (positive_variables (Or(e1, e2))) (negative_variables (Or(e1, e2)))
        | Or(e1, e2)    -> test_disj e1 && test_disj e2
        | _             -> false
    in test_disj disj
;;

disj_contradiction e1;;
disj_contradiction e2;;
disj_contradiction e3;;
disj_contradiction e4;;
disj_contradiction e5;;
