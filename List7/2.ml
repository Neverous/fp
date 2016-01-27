type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref;;

let rec concat_copy first second = match first with
    | LMcons (value, next)  ->
        if !next = LMnil then LMcons (value, second)
        else LMcons (value, ref (concat_copy !next second))
    | LMnil -> failwith "concat_copy";;

concat_copy (LMcons (1, ref LMnil)) (ref (LMcons(2, ref LMnil)));;

let rec concat_share first second = match !first with
    | LMcons (value, next)  ->
        if !next = LMnil then let () = next := !second in first
        else let _ = concat_share next second in first
    | LMnil                 -> second;;

let ex = ref (LMcons (1, ref LMnil));;
let ex2 = ref (LMcons (2, ref LMnil));;
concat_share ex ex2;;
ex;;
ex2;;
ex2 := LMcons (7, ref LMnil);;
ex;;
