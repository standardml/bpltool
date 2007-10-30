(* Copyright (c) 2006  The BPL Group at the IT University of Copenhagen
 *
 * This file is part of BPL.
 *
 * BPL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BPL is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BPL; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

(** Lazy list datatype.  Note that this interface does not specify
 * whether an element in a lazylist is evaluated more than once if
 * the list is inspected several times.
 * @version $LastChangedRevision$
 *)
 
 signature LAZYLIST =
 sig
  (** A lazy list type. *)
  type 'a lazylist
  (** Lazy list cell data type. *)
  datatype 'a lazycell = Nil | Cons of 'a * 'a lazylist
  (** Lazy list nil constructor. *)
  val lzNil : 'a lazylist
  (** Lazy list cons constructor. *)
  val lzCons : (unit -> 'a * 'a lazylist) -> 'a lazylist
  (** Signal that a lazylist was unexpectedly empty. *)
  exception EmptyList
  (** Test whether the lazy list is empty. *)
  val lznull : 'a lazylist -> bool
  (** Return the first element of a lazy list. 
   * @exception EmptyList if the list is empty.
   *)
  val lzhd : 'a lazylist -> 'a
  (** Return the tail of a lazy list. 
   * @exception EmptyList if the list is empty.
   *)
  val lztl : 'a lazylist -> 'a lazylist
  (** Return a lazy list, given a list producer. *)
  val lzmake : (unit -> 'a lazycell) -> 'a lazylist
  (** Return head and tail of a lazy list, or Nil if it is empty. *)
  val lzunmk : 'a lazylist -> 'a lazycell
  (** Map a function on all the elements of a lazy list. *)
  val lzmap : ('a -> 'b) -> 'a lazylist -> 'b lazylist
  (** Fold a function over all the elements of a lazy list.
   * The result of <br />
   * lzfoldr op init [x1, x2, ...] is <br />
   * x1 op' (x2 op' (... op' init)), where x op g = x op' (g ()).
   * @params f init xs
   * @param f     function with which to fold.
   * @param init  result when folding over an empty list.
   * @param xs    the lazy list over which to fold.
   *)
  val lzfoldr : ('a * (unit -> 'b) -> 'b) -> 'b -> 'a lazylist -> 'b
  (** Append two lazy lists. *)
  val lzappend : 'a lazylist -> 'a lazylist -> 'a lazylist
	(** Concatenate a lazy list of lazy lists. *)
	val lzconcat : 'a lazylist lazylist -> 'a lazylist
	(** Concatenate a list of lazy lists. *)
	val lzconcatlists : 'a lazylist list -> 'a lazylist
  (** Return the nth element of a lazy list.
   * @params xs n
   * @exception Subscript if n >= length xs or n < 0.
   *)
  val lznth : 'a lazylist -> int -> 'a
  (** Return a lazy list of the first n elements of a lazy list.
   * @params xs n
   * @exception Subscript if n > length xs or n < 0.
   *)
  val lztake : 'a lazylist -> int -> 'a lazylist
  (** Return a lazy list excluding the first n elements of a lazy list.
   * @params xs n
   * @exception Subscript if n > length xs or n < 0.
   *)
  val lzdrop : 'a lazylist -> int -> 'a lazylist
  (** Given a list of k lazy lists, return a lazy list of lists
   * of length k, containing all combinations of picking one
   * element from each lazy list.
   * @params xss
   * @param xss  [[x_00, x_01, ..., x_0n_0], [x_10, x_11, ..., x_1n_1],
   *              ..., [x_k0, x_k1, ..., x_kn_k]]
   * @return [[x_00, x_10, ..., x_k0], [x_01, x_10, ..., x_k0],
   *          ..., [x_0n_0, x_1n_1, ..., x_kn_k]]
   *)
  val lzcombine : 'a lazylist list -> 'a list lazylist
  (** Given a list of lazy lists, return a lazy list merging elements
   * from alternating lists.  The lists need not have equal lengths,
   * in which case a nonexistant x_ij is omitted from the returned list.
   * @params xss
   * @param xss  [[x_00, ..., x_0n_0], ..., [x_k0, ..., x_kn_k]]
   * @return [x_00, x_10, ..., x_k0, x_01, x_11, ..., x_k1, ...,
   *          x_0n_max, x_1n_max, ..., x_kn_max].
   *)
  val lzmerge : 'a lazylist list -> 'a lazylist
  (** Compute all the elements of a lazy list. *)
  val lztolist : 'a lazylist -> 'a list
  (** Print a lazy list lazily. *)
  val lzprint : ('a -> string) -> 'a lazylist -> unit
  (** Print a lazy list lazily, each element on its own line. *)
  val lzprintln : ('a -> string) -> 'a lazylist -> unit
end
