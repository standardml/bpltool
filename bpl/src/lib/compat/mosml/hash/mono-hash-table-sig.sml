(** mono-hash-table-sig.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 *
 * The result signature of the hash table functor (see hash-table.sml).
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

signature MONO_HASH_TABLE =
sig
  (** Item key type. *)
  structure Key : HASH_KEY
  (** The hash table type, parameterised over the item value type. *)
  type 'a hash_table

	(** Create a new table.
	 * @params size findexn
	 * @param size     A size hint.
	 * @param findexn  The exception to be raised by find.
	 *)
  val mkTable : (int * exn) -> 'a hash_table

 	(** remove all elements from the table *)
  val clear : 'a hash_table -> unit

	(** Insert an item.  If the key already has an item associated with it,
	 * then the old item is discarded.
	 *)
  val insert : 'a hash_table -> (Key.hash_key * 'a) -> unit

(* not implemented in all versions of  Standard ML of New Jersey 
	(* return true, if the key is in the domain of the table *)
  val inDomain : 'a hash_table -> Key.hash_key -> bool *)

	(** Find an item; the table's exception is raised if the item
	 * doesn't exist.
	 * @params ht key
	 * @param ht    The hash table.
	 * @param key   The item key.
	 * @return      The item value.
	 *)
  val lookup : 'a hash_table -> Key.hash_key -> 'a

	(** Look for an item; return NONE if the item doesn't exist.
	 * @params ht key
	 * @param ht    The hash table.
	 * @param key   The item key.
	 * @return      The item value.
	 *)
  val find : 'a hash_table -> Key.hash_key -> 'a option

	(** Remove an item, returning the item; the table's exception is raised if
	 * the item doesn't exist.
	 * @params ht key
	 * @param ht    The hash table.
	 * @param key   The item key.
	 * @return      The item value.
	 *)
  val remove : 'a hash_table -> Key.hash_key -> 'a

	(** Return the number of items in the table. *)
  val numItems : 'a hash_table ->  int

	(** Return a list of the item value in the table. *)
  val listItems  : 'a hash_table -> 'a list
	(** Return a list of the item (key, value) pairs in the table. *)
  val listItemsi : 'a hash_table -> (Key.hash_key * 'a) list

	(** Apply a function to the item values of the table. *)
  val app  : ('a -> unit) -> 'a hash_table -> unit
	(** Apply a function to the entries of the table. *)
  val appi : ((Key.hash_key * 'a) -> unit) -> 'a hash_table -> unit

	(** Map a table to a new table that has the same keys. *)
  val map  : ('a -> 'b) -> 'a hash_table -> 'b hash_table
	(** Map a table to a new table that has the same keys. *)
  val mapi : ((Key.hash_key * 'a) -> 'b) -> 'a hash_table -> 'b hash_table

  (** Fold a function over a base value and the table items.
   * @params f u ht
   * @param f     The function to fold.
   * @param u     The base value.
   * @param ht    The hash table.
   *)
  val fold  : (('a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b
  (** Fold a function over a base value and the table items.
   * @params f u ht
   * @param f     The function to fold.
   * @param u     The base value.
   * @param ht    The hash table.
   *)
  val foldi : ((Key.hash_key * 'a * 'b) -> 'b) -> 'b -> 'a hash_table -> 'b

	(** Modify the hash-table items in place. *)
  val modify  : ('a -> 'a) -> 'a hash_table -> unit
	(** Modify the hash-table items in place. *)
  val modifyi : ((Key.hash_key * 'a) -> 'a) -> 'a hash_table -> unit

(* Also mapPartial?? *)

	(** Remove any hash table items that do not satisfy the given
	 * predicate.
	 *)
  val filter  : ('a -> bool) -> 'a hash_table -> unit
	(** Remove any hash table items that do not satisfy the given
	 * predicate.
	 *)
  val filteri : ((Key.hash_key * 'a) -> bool) -> 'a hash_table -> unit

	(** Create a copy of a hash table. *)
  val copy : 'a hash_table -> 'a hash_table

	(** Returns a list of the sizes of the various buckets.  This is to
	 * allow users to gauge the quality of their hashing function.
	 *)
  val bucketSizes : 'a hash_table -> int list

  end (* MONO_HASH_TABLE *)
