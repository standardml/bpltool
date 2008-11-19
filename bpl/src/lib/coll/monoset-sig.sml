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
    exception Empty

    val list : Set -> elt list
      (** Constructs a set containing the elements in l.
       * @params l
       * @exception DuplicatesRemoved if an element of the list l appears
       * twice.
       *)
    val fromList : elt list -> Set 
      (** Constructs a set containing the elements in l; duplicate
       * elements are allowed
       * @params l
       *)
    val fromList' : elt list -> Set 
(*
    val addList1 : elt list -> elt list -> Set -> Set * elt list
*)
      (** Add elements in list l to s. 
       * @params l s
       * @exception DuplicatesRemoved if a an element of the list l is already
       * in the set s.
       *)
    val addList : elt list -> Set -> Set
      (** Add elements in list l to s; no exceptions are raised.
       * @params l s
       *)
    val addList' : elt list -> Set -> Set

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
    val disjoint : Set -> Set -> bool
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

      (** all f s : applies f to each element x of the set s, until f x
       * evaluates to false; it returns false if such an x exists and
       * true otherwise.
       *)
    val all : (elt -> bool) -> Set -> bool

      (** map f s; builds a new set by applying f to each element in s *)
    val map : (elt -> elt) -> Set -> Set

      (** apply f s; applies f to each element of s (in order) *)
    val apply : (elt -> unit) -> Set -> unit

    val compare : Set * Set -> order

    type StringTree

    val layoutSet : {start: string, sep: string, finish: string} ->
      (elt -> StringTree) -> Set -> StringTree
(*
    val pu : elt Pickle.pu -> Set Pickle.pu 
*)				 

      (** Return some element from the given set.
       * @params set
       * @param set  the set to select an element from
       * @exception Empty if the set is empty
       *)
    val someElement : Set -> elt
  end
