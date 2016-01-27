let polynomial as_ x =
    let rec poly as_ x result = match as_ with
        | []            -> result
        | a_n :: as_    -> poly as_ x (x *. result +. a_n)
    in poly as_ x 0.
;;

let polynomial2 as_ x =
    List.fold_left (fun result a_n -> x *. result +. a_n) 0. as_;;

Printf.printf "%.2f\n" (polynomial [1.; 0.; -1.; 2.] 0.);;
Printf.printf "%.2f\n" (polynomial [1.; 0.; -1.; 2.] 1.);;
Printf.printf "%.2f\n" (polynomial [1.; 0.; -1.; 2.] 2.);;
Printf.printf "%.2f\n" (polynomial [1.; 0.; -1.; 2.] 3.);;
Printf.printf "%.2f\n" (polynomial [1.; 0.; -1.; 2.] 4.);;
Printf.printf "%.2f\n" (polynomial [1.; 0.; -1.; 2.] 5.);;

Printf.printf "%.2f\n" (polynomial2 [1.; 0.; -1.; 2.] 0.);;
Printf.printf "%.2f\n" (polynomial2 [1.; 0.; -1.; 2.] 1.);;
Printf.printf "%.2f\n" (polynomial2 [1.; 0.; -1.; 2.] 2.);;
Printf.printf "%.2f\n" (polynomial2 [1.; 0.; -1.; 2.] 3.);;
Printf.printf "%.2f\n" (polynomial2 [1.; 0.; -1.; 2.] 4.);;
Printf.printf "%.2f\n" (polynomial2 [1.; 0.; -1.; 2.] 5.);;
