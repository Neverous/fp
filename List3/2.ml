let polynomial as_ x =
    let rec poly as_ x x_n result = match as_ with
        | []            -> result
        | a_n :: as_    -> poly as_ x (x_n *. x) (result +. a_n *. x_n)
    in poly as_ x 1. 0.
;;

let polynomial2 as_ x =
    List.fold_right (fun a_n result -> x *. result +. a_n) as_ 0.;;

Printf.printf "%.2f\n" (polynomial [2.; -1.; 0.; 1.] 0.);;
Printf.printf "%.2f\n" (polynomial [2.; -1.; 0.; 1.] 1.);;
Printf.printf "%.2f\n" (polynomial [2.; -1.; 0.; 1.] 2.);;
Printf.printf "%.2f\n" (polynomial [2.; -1.; 0.; 1.] 3.);;
Printf.printf "%.2f\n" (polynomial [2.; -1.; 0.; 1.] 4.);;
Printf.printf "%.2f\n" (polynomial [2.; -1.; 0.; 1.] 5.);;

Printf.printf "%.2f\n" (polynomial2 [2.; -1.; 0.; 1.] 0.);;
Printf.printf "%.2f\n" (polynomial2 [2.; -1.; 0.; 1.] 1.);;
Printf.printf "%.2f\n" (polynomial2 [2.; -1.; 0.; 1.] 2.);;
Printf.printf "%.2f\n" (polynomial2 [2.; -1.; 0.; 1.] 3.);;
Printf.printf "%.2f\n" (polynomial2 [2.; -1.; 0.; 1.] 4.);;
Printf.printf "%.2f\n" (polynomial2 [2.; -1.; 0.; 1.] 5.);;
