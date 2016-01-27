#use "prolog.ml";;

let run2 g p = eval_goal g p (fun sc -> 1 + (sc ())) (fun () -> 0)

let c1_1 = run2 g1 p1
let c1_2 = run2 g1 p2
let c1_3 = run2 g1 p3

type regexp = Atom of char | And of regexp * regexp | Or of regexp * regexp | Star of regexp;;
let rec match_regexp regex chars success failure = match regex with
    | Atom x            -> (match chars with
        | []            -> failure ()
        | chr :: chars  ->
            if chr = x then success chars failure
            else failure ())
    | And (left, right) ->
        match_regexp left chars (fun chars failure -> match_regexp right chars success failure) failure
    | Or (left, right)  ->
        match_regexp left chars success (fun () -> match_regexp right chars success failure)
    | Star x            ->
        let rec success_ chars failure = match_regexp x chars success_ (fun () -> success chars failure)
        in match_regexp x chars success_ (fun () -> success chars failure);;

let run regex chars = match_regexp regex chars (fun chars failure -> if chars = [] then true else failure ()) (fun () -> false);;

let r1 = Atom 'a';;                                                             (* a *)
let r2 = Star (Atom 'a');;                                                      (* a* *)
let r3 = And (Atom 'a', Atom 'b');;                                             (* ab *)
let r4 = Or (Atom 'a', Atom 'b');;                                              (* a|b *)
let r5 = And (Or (Atom 'a', Star (Atom 'b')), Star (Or (Atom 'a', Atom 'b')));; (* (a|b* )(a|b)* *)

run r1 ['a'];;
run r1 ['b'];;
run r1 ['a'; 'a'];;
run r2 [];;
run r2 ['a'];;
run r2 ['a'; 'a'];;
run r2 ['a'; 'a'; 'b'; 'a'];;
run r3 ['a'; 'b'];;
run r3 ['a'; 'b'; 'a'];;
run r3 ['c'];;
run r4 ['a'];;
run r4 ['b'];;
run r4 ['c'];;
run r5 ['b'; 'a'; 'a'; 'b'];;
