let is_matrix repr =
    let size = List.length repr
    in List.for_all (fun row -> List.length row = size) repr
;;

is_matrix [];;
is_matrix [[1]];;
is_matrix [[1;1]];;
is_matrix [[1;1];[1]];;
is_matrix [[1;1];[1;1]];;

let nth_column matrix n = List.map (fun row -> List.nth row n) matrix;;

nth_column [] 0;;
nth_column [[1;2];[3;4]] 0;;
nth_column [[1;2];[3;4]] 1;;
nth_column [[1;2;3];[4;5;6];[7;8;9]] 0;;
nth_column [[1;2;3];[4;5;6];[7;8;9]] 1;;
nth_column [[1;2;3];[4;5;6];[7;8;9]] 2;;

let transpose matrix = List.mapi (fun pos _ -> nth_column matrix pos) matrix;;

transpose [[1;2;3];[4;5;6];[7;8;9]];;

let zip xs ys = List.map2 (fun x y -> (x, y)) xs ys;;

zip [1;2;3] ["a";"b";"c"];;

let zipf f xs ys = List.map (fun (x, y) -> f x y) (zip xs ys);;
let zipf2 f xs ys = List.map2 f xs ys;;

zipf (+.) [1.;2.;3.] [4.;5.;6.];;
zipf2 (+.) [1.;2.;3.] [4.;5.;6.];;

let mult_vec vector matrix =
    let sum = List.fold_left (+.) 0.
    and mult_row v = List.map (fun c -> v *. c)
    in List.map sum (transpose (zipf mult_row vector matrix));;

mult_vec [1.;2.] [[2.;0.];[4.;5.]];;

let mult_mat matrix1 matrix2 = List.map (fun row -> mult_vec row matrix2) matrix1;;

mult_mat [[1.;0.];[0.;1.]] [[1.;2.];[3.;4.]];;
