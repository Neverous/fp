let rec fix func x = func (fix func) x;;

let sil = fix (fun self n -> if n = 0 then 1 else n * (self (n - 1)));;

sil 10;;

let sil2 =
    let self = ref (fun (n:int) -> 0)
    in let sil n = if n = 0 then 1 else n * (!self (n - 1))
    in let () = self := sil
    in sil;;

sil2 10;;

let fix2 =
    let t x = failwith "a"
    in let self = ref (fun f x -> f t x)
    in let fix f x = f (!self f) x
    in let () = self := fix
    in fix;;

let sil3 = fix2 (fun f n -> if n = 0 then 1 else n * (f (n - 1)));;

sil3 10;;
