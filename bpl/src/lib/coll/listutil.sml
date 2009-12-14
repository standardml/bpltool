(* Utility functions for lists. *)
structure ListUtil :> LISTUTIL =
struct

fun foldri f init xs =
  let
    val n = length xs
  in
    #2 (foldr (fn (x, (i,  acc)) => (i - 1, f (i, x, acc)))
              (n - 1, init) xs)
  end

fun mapi f xs = foldri (fn (i, x, acc) => f (i, x) :: acc) [] xs

fun cartesian_product l1 l2 =
  foldr
    (fn (x, prod) => foldr (fn (y, prod) => (x, y) :: prod) prod l2)
    [] l1

end
