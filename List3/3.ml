let derivative = function
    | []            -> []
    | [_]           -> []
    | (_ :: as_)    ->
        let rec der as_ pos das = match as_ with
            | []        -> List.rev das
            | a :: as_  -> der as_ (pos +. 1.) ((pos *. a) :: das)
        in der as_ 1. []
;;

let derivative2 as_ = List.tl (List.mapi (fun pos a -> float_of_int pos *. a) as_)
;;

derivative [1.; 0.; -1.; 2.];;
derivative [1.];;
derivative [0.; 0.; 1.];;
derivative [1.; 1.];;
derivative [1.; 1.; 1.];;
derivative [1.; 1.; 1.; 1.];;
derivative [1.; 1.; 1.; 1.; 1.];;

derivative2 [1.; 0.; -1.; 2.];;
derivative2 [1.];;
derivative2 [0.; 0.; 1.];;
derivative2 [1.; 1.];;
derivative2 [1.; 1.; 1.];;
derivative2 [1.; 1.; 1.; 1.];;
derivative2 [1.; 1.; 1.; 1.; 1.];;
