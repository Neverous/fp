let is_palindrome xs =
    let rec is_palindrome_ p1 p2 rev = match (p1, p2) with
        | (p1, [])                  -> p1 = rev
        | (p :: p1, [_])            -> p1 = rev
        | (p :: p1, _ :: _ :: p2)   -> is_palindrome_ p1 p2 (p :: rev)
    in is_palindrome_ xs xs []
;;

is_palindrome [];;
is_palindrome [1];;
is_palindrome [1;1];;
is_palindrome [1;2;1];;
is_palindrome [1;3;3;1];;
is_palindrome [1;2;1;2];;
is_palindrome [1;2];;
