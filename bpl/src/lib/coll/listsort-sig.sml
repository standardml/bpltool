(* Listsort *)
(* Taken from the Moscow ML library. FIXME license? *)

signature LISTSORT =
sig
  (** sorts the list xs in nondecreasing order, using the
   * given ordering.  Uses Richard O'Keefe's smooth applicative merge
   * sort.
   *
   * @params ordr xs
   * @param ordr  an ordering
   * @param xs    the list to sort
   *)
  val sort   : ('a * 'a -> order) -> 'a list -> 'a list
  (** checks that the list xs is sorted in nondecreasing
   * order, in the given ordering.
   *
   * @params ordr xs
   * @param ordr  an ordering.
   * @param xs    the list to sort.
   *)
  val sorted : ('a * 'a -> order) -> 'a list -> bool
end
