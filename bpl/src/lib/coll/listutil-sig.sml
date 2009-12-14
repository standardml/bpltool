(* Utility functions for lists *)
signature LISTUTIL =
sig

  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b

  val mapi : (int * 'a -> 'b) -> 'a list -> 'b list

  val cartesian_product : 'a list -> 'b list -> ('a * 'b) list

end
