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

fun split n l =
  let
    fun split' (0, l1, l2)    = (rev l1, l2)
      | split' (_, l1, [])    = (rev l1, [])
      | split' (n, l1, l::l2) = split' (n-1,l::l1,l2)
  in
    if n >= 0 then
      split' (n, [], l)
    else
      ([], [])
  end
end
