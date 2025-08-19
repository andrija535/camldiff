let find_lcs (type a) (l1: a list) (l2: a list) (module M: Map.OrderedType with type t = a list * a list): a list =
  let module Memoizer = Map.Make(M) in
  let rec get_lcs_for_prefix (xs,ys) context =
    if Memoizer.mem (xs,ys) context then
      let v = Memoizer.find (xs,ys) context in
      v,context
    else
       let v = go context (xs,ys) in
       let context = Memoizer.add (xs,ys) v context in
       v,context
  and go context = function
    | [],[] | [],_ | _,[] -> []
    | x::xs, y::ys when x=y ->
       let rest,_ = get_lcs_for_prefix (xs,ys) context in
       x::rest
    | x::xs, y::ys ->
       let rest1,context = get_lcs_for_prefix (xs,y::ys) context in
       let rest2,_ = get_lcs_for_prefix (x::xs,ys) context in
       if List.length rest1 > List.length rest2 then rest1 else rest2
  in
  go Memoizer.empty (l1,l2)

let%test_module "lcs test" = (module struct
                                module M : Map.OrderedType with type t = int list * int list = struct
                                  type t = int list * int list

                                  let compare (x,y) (x',y') =
                                    if List.equal Int.equal x x' && List.equal Int.equal y y'
                                    then 0
                                    else -1
                                end
                                
                                let%test "two_empty" = find_lcs [] [] (module M) = []
                                let%test "first_empty" = find_lcs [] [1;2;3] (module M) = []
                                let%test "second_empty" = find_lcs [1;2;3] [] (module M) = []
                                let%test "same_list" = find_lcs [1;2;3] [1;2;3] (module M) = [1;2;3]
                                let%test "no_lcs" = find_lcs [1;2;3] [4;5;6] (module M) = []
                                let%test "non_empty_lcs" = find_lcs [1;2;3;4;5;6] [1;3;6;8;9] (module M) = [1;3;6]
                              end)
