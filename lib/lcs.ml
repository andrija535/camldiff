let lazy_bind (x: 'a Lazy.t) (f: 'a -> 'b Lazy.t): 'b Lazy.t =
  let x = Lazy.force x in
  f x

let find_lcs (l1: 'a list) (l2: 'a list): 'a list =
  let (let$) = lazy_bind in
  let open Lazy in
  let rec go = function
    | [],[] | _,[] | [],_ -> lazy []
    | x::xs, y::ys ->
       if x = y then
         let$ rest = go (xs,ys) in
         lazy (x::rest)
       else
         let$ prefix1 = go (x::xs, ys) in
         let$ prefix2 = go (xs, y::ys) in
         if List.length prefix1 > List.length prefix2
         then lazy prefix1
         else lazy prefix2
  in
  force @@ go (l1,l2)

let%test "two_empty" = find_lcs [] [] = []
let%test "first_empty" = find_lcs [] [1;2;3] = []
let%test "second_empty" = find_lcs [1;2;3] [] = []
let%test "same_list" = find_lcs [1;2;3] [1;2;3] = [1;2;3]
let%test "no_lcs" = find_lcs [1;2;3] [4;5;6] = []
let%test "non_empty_lcs" = find_lcs [1;2;3;4;5;6] [1;3;6;8;9] = [1;3;6]
