let lit text cont = (fun txt -> cont (txt ^ text));;
let eol cont = (fun txt -> cont (txt ^ "\n"));;
let inr cont = (fun txt num -> cont (txt ^ (string_of_int num)));;
let flt cont = (fun txt num -> cont (txt ^ (string_of_float num)));;
let str cont = (fun txt str -> cont (txt ^ str));;
let (++) a b =  (fun txt -> a (b txt));;

let sprintf pattern = pattern (fun res -> res) "";;

lit "Hello World\n";;
sprintf (lit "Hello World" ++ eol);;
sprintf (inr ++ eol) 42;;
sprintf (lit "Ala ma " ++ inr) 7;;
sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".") 7 "ow";;
