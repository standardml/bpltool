(* Modified version of the MONO_SET signature given in the Edinburgh
   library. *)
(* TCD: Modified to make insert (and derived functions) signal insertion of 
 *      duplicates.
 * April 2006: Arne John Glenstrup: Added foldUntil function.
 *) 

signature MONO_SET =
  sig
    type elt
    type Set

    val empty : Set
    val singleton : elt -> Set

    val size : Set -> int
    val isEmpty : Set -> bool
    val member : elt -> Set -> bool
    val eq : Set -> Set -> bool

    exception DuplicatesRemoved

    val list : Set -> elt list
      (** fromList l : Constructs a set containing the elements in l.
       * Raises DuplicatesRemoved if a an element of the list l appears
       * twice.
       *)
    val fromList : elt list -> Set 
(*
    val addList1 : elt list -> elt list -> Set -> Set * elt list
*)
      (** addList l s : Add elements in list l to s. 
       * Raises DuplicatesRemoved if a an element of the list l is already
       * in the set s.
       *)
    val addList : elt list -> Set -> Set

      (** insert a s : Add the element a to s.
       * Raises DuplicatesRemoved if a is already in the set.
       *)
    val insert : elt -> Set -> Set
      (** insert' a s : Add the element a to s
       * (does not raise DuplicatesRemoved).
       *)
    val insert' : elt -> Set -> Set
    val remove : elt -> Set -> Set
    val difference : Set -> Set -> Set
    val intersect : Set -> Set -> Set
      (** union (s,t) : returns s \cup t
       * raises DuplicatesRemoved if s \cap t \ne \empty
       *)
    val union : Set -> Set -> Set
      (** union (s,t) : returns s \cup t
       * (but will not raise DuplicatesRemoved)
       *)
    val union' : Set -> Set -> Set

    val partition : (elt -> bool) -> Set -> Set * Set

      (** subst (a,b) s : Substitute element b in s with element a. *)
    val subst : elt * elt -> Set -> Set

      (** fold f base s; folds using f over the base element. *)
    val fold : (elt -> 'b -> 'b) -> 'b -> Set -> 'b

      (** foldUntil f base s; folds using f over the base element,
       * returning r first time f returns (true, r), or if all elements
       * have been folded resulting in (..., r).
       *)
    val foldUntil : (elt -> 'b -> bool * 'b) -> 'b -> Set -> 'b

      (** map f s; builds a new set by applying f to each element in s *)
    val map : (elt -> elt) -> Set -> Set

      (** apply f s; applies f to each element of s (in order) *)
    val apply : (elt -> unit) -> Set -> unit

    type StringTree

    val layoutSet : {start: string, sep: string, finish: string} ->
      (elt -> StringTree) -> Set -> StringTree
(*
    val pu : elt Pickle.pu -> Set Pickle.pu 
*)				 
    
  end
