let hd stream = stream 0;;
let tl stream i = stream (i + 1);;
let add const stream i = const + stream i;;
let map func stream i = func (stream i);;
let map2 func stream1 stream2 i = func (stream1 i) (stream2 i);;
let replace n a stream i =
    if i mod n = 0 then a
    else stream i;;

let take n stream i = stream (i * n)
let rec fold func base stream = function
    | 0 -> func base (stream 0)
    | i -> func (fold func base stream (i - 1)) (stream i);;

let rec tabulate stream ?(from=0) _to =
    if from = _to then [ stream from ]
    else (tabulate stream ~from:from (_to - 1))@[stream _to];;

let odd x = 2 * x + 1;;
let even x = 2 * x;;

hd even;;
even 7;;
tl even 7;;
add 7 even 7;;
map ((+) 1) even 7;;
map2 (+) even odd 7;;
replace 1 0 even 7;;
take 2 even 4;;
fold (+) 0 even 7;;
tabulate even 6;;
tabulate even ~from:6 6;;
