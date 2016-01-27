type 'a flist = FNil | FCons of 'a * (unit -> 'a flist);;
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let fhd = function
    | FNil          -> failwith "fhd"
    | FCons (x, _)  -> x
;;

let lhd = function
    | LNil          -> failwith "lhd"
    | LCons (x, _)  -> x
;;

let ftl = function
    | FNil          -> failwith "ftl"
    | FCons (_, fl) -> fl ()
;;

let ltl = function
    | LNil                  -> failwith "ltl"
    | LCons (_, lazy ll)    -> ll
;;

let rec ftake = function
    | (0, _)                -> []
    | (_, FNil)             -> []
    | (n, FCons (x, xs))    -> x :: ftake (n - 1, xs ())
;;

let rec ltake = function
    | (0, _)                    -> []
    | (_, LNil)                 -> []
    | (n, LCons (x, lazy xs))   -> x :: ltake (n - 1, xs)
;;

let rec toLazyFList = function
    | []        -> FNil
    | x :: xs   -> FCons (x, function () -> toLazyFList xs)
;;

let rec toLazyLList = function
    | []        -> LNil
    | x :: xs   -> LCons (x, lazy (toLazyLList xs))
;;

let rec (@$) fl1 fl2 = match fl1 with
    | FNil          -> fl2
    | FCons (x, xs) -> FCons (x, function () -> (xs ()) @$ fl2)
;;

let rec fmap f = function
    | FNil          -> FNil
    | FCons (x, xs) -> FCons (f x, function () -> fmap f (xs ()))
;;

let rec lmap f = function
    | LNil                  -> LNil
    | LCons (x, lazy xs)    -> LCons (f x, lazy (lmap f xs))
;;

let fPI =
    let rec fPI_ acc i =
        let t = 1.0 /. float_of_int (2 * i + 1)
        in let acc2 = if i mod 2 == 0 then acc +. t else acc -. t
        in FCons (4.0 *. acc2, function () -> fPI_ acc2 (i + 1))
    in fPI_ 0.0 0
;;

let lPI =
    let rec lPI_ acc i =
        let t = 1.0 /. float_of_int (2 * i + 1)
        in let acc2 = if i mod 2 == 0 then acc +. t else acc -. t
        in LCons (4.0 *. acc2, lazy (lPI_ acc2 (i + 1)))
    in lPI_ 0.0 0
;;

let rec fmap3 f = function
    | FNil          -> FNil
    | FCons(a, xs)  ->
        let next = xs ()
        in let [b; c] = ftake (2, next)
        in FCons (f a b c, function () -> fmap3 f next)
;;

let rec lmap3 f = function
    | LNil              -> LNil
    | LCons(a, lazy xs) ->
        let [b; c] = ltake (2, xs)
        in LCons (f a b c, lazy (lmap3 f xs))
;;

let euler x y z = z -. (y -. z) *. (y -. z) /. (x -. 2. *. y +. z);;
let fPI_fast = fmap3 euler fPI;;
let lPI_fast = lmap3 euler lPI;;

ftake (10, fPI);;
ltake (10, lPI);;
ftake (10, fPI_fast);;
ltake (10, lPI_fast);;
