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

(** Abstract data type for modelling bigraph permutations.
 * @version $LastChangedRevision$
 *)
signature PERMUTATION =
sig
  type nameset
  type nameconstraints
  type interface

  (** The mutable kind of permutation. *)
  type Mutable
  (** The immutable kind of permutation. *)
  type Immutable
  (** The permutation data type. *)
  type 'kind permutation
  (** An ordered permutation is a permutation with additional data that
   * supports incremental generation of all permutations of a given width. *)
  type 'kind ordered_permutation
  (** Signal a logical error, i.e. an error which "cannot happen" ;-).*)
  exception LogicalError of string
  (** Signal incorrect permutation data. *)
  exception NotPermutation of (int * nameset) list
  (** Construct a permutation.
   * @params Xs
   * @param Xs  list of n pairs, where each of the numbers 0..n-1 must
   *            be present exactly once in the list.  An entry (j, X)
   *            at position 0 <= k < n in Xs means that the
   *            permutation maps inner site k to outer root j, with Xs
   *            being the set of local names of this site.
   * @exception NotPermutation  raised if each number is not present
   *            exactly once.
   *)
  val make : (int * nameset) list -> 'kind permutation
  (** Deconstruct a permutation. *)
  val unmk : 'kind permutation -> (int * nameset) list

  (** Test two permutations for equality.
   * @params p1 p2
   * @param p1  the first permutation.
   * @param p2  the second permutation.
   *)
  val eq : 'kind1 permutation -> 'kind2 permutation -> bool
  (** Test two permutations for equality up to a bijection between the inner
   * names satisfying the constraints given by C.
   * 
   * If the permutations are equal, a set of constraints on a bijection between
   * the outer names is returned.
   *
   * @params C p1 p2
   * @param C   constraints on the bijection between the inner names of the two
   *            ions.
   * @param p1  the first ion.
   * @param p2  the second ion.
   *)
  val eq' : nameconstraints -> 'kind1 permutation -> 'kind2 permutation -> nameconstraints option

  (** Return the width of a permutation. *)
  val width : 'kind permutation -> int
  (** Return the inner face of a permutation *)
  val innerface : 'kind permutation -> interface
  (** Return the outer face of a permutation *)
  val outerface : 'kind permutation -> interface

  (** Return an identity permutation. 
   * The width of the permutation is given by the length of Xs.
   * @params Xs
   * @param Xs  list of local name sets.
   *)
  val id : nameset list -> Immutable permutation
  (** Return a nameless identity permutation.
   * @params n
   * @param m  width of the permutation.
   *)
  val id_n : int -> Immutable permutation
  (** Return the empty identity permutation. *)
  val id_0 : Immutable permutation

  (** Signal that two permutations cannot be composed. *)
  exception Uncomposable
    of Mutable permutation * Mutable permutation * string

  (** Return the composition of two permutations. 
   * @exception Uncomposable raised if the interfaces do not match.
   *)
  val o : ('kinda permutation * 'kindb permutation) -> 'kindc permutation

  (** Return the tensor product of two permutations. *)
  val * : ('kinda permutation * 'kindb permutation) -> 'kindc permutation

  (** Return the tensor product of a list of permutations. *)
  val ** : 'kinda permutation list -> 'kindb permutation

  (** Return the inverse of a permutation.
   * @params pi
   * @param pi  permutation to invert.
   * @return    the inverse of pi.
   *)
  val invert
      : 'kind permutation -> Immutable permutation
  (** Permute the list of values as described by the permutation.<br />
    * @params pi Xs
    * @param pi  the permutation.
    * @param Xs  the list of values [x_0, ..., x_{n-1}] to permute.
    * @return    a list [x_{pi^-1(0)}, ..., x_{pi^-1(n-1)}].
    *)
  val permute
      : 'kind permutation -> 'a list -> 'a list
  (** Push a permutation through a product of primes.
   * DEPRECATED! - Use prod instead.
   * @params pi Xss
   * @param pi   permutation to push through n primes.
   * @param Xss  list of local inner name lists.  The jth (0 <= j < n)
   *             local inner name list is a list of name sets
   *             describing the inner names of the sites of the jth prime.
   * @return     a permutation pi_Xss such that pi o (P_0 x ... x P_n-1)
   *             = (P_pi(0) x ... x P_pi(n-1)) pi_Xss.
   * @see bigraph literature on the Pushthrough Lemma.
   *)
  val pushthru
      : 'kinda permutation -> nameset list list -> 'kindb permutation

  (** Signal that a list of nameset lists is incompatible with a permutation.
   * 
   * @params pi Xss
   * @param pi   the permutation.
   * @param Xss  list of local inner name lists.
   *)
  exception IncompatibleNamesetListList
  of Mutable permutation * nameset list list
  (** Signal that a permutation is not regularizable relative to a list
   *  of local inner name lists.
   *)
  exception NotRegularisable of Mutable permutation * nameset list list
  (** Split a permutation into one major and a number of minor
   * permutations.
   * @params pi Xss
   * @param pi   the permutation to split
   * @param Xss  list of local inner name lists.
   * @exception IncompatibleNamesetListList  if the total number of elements
   *                                         in Xss is different than the
   *                                         width of pi.
   * @exception NotRegularisable  if the permutation cannot be
   *                              regularized.
   *)
  val split
      : 'kinda permutation -> nameset list list
        -> {major : 'kindb permutation, minors : 'kindc permutation list}
  (** Split the permutation into two parts: (1) one that groups the
   * sites according to Xss without changing the order within the group,
   * and (2) a permutation for each group that permutes the sites of a
   * group.
   *
   * @params pi Xss
   * @param pi   the permutation to split
   * @param Xss  list of local inner name lists.
   * @exception IncompatibleNamesetListList  if the total number of elements
   *                                         in Xss is different than the
   *                                         width of pi.
   *)
  val general_split
      : 'kinda permutation -> nameset list list
        -> {group : 'kindb permutation, minors : 'kindc permutation list}
  (** Compute the product of a list of local inner names lists and a
   * permutation as defined in the implementation article Definition 3.5.
   * Let the lengths of the inner names lists be [n_0, ..., n<sub>k-1</sub>}];
   * then the product Xss * pi is defined by<p>
   *
   * (Xss * pi)(i + &sum;<sub>i' < l</sub> n<sub>pi(i')</sub>) =
   * i + &sum;<sub>j' < pi(l)</sub> n_j',
   * where i < n<sub>pi(l)</sub> and l < k.
   *
   * @params Xss pi
   * @param Xss  List of local inner names lists.
   * @param pi   The permutation.
   *)
  val prod : nameset list list -> 'kinda permutation -> 'kindb permutation

  (** Signal that two lists have different lengths.  *)
  exception UnequalLengths 
	    of nameset list list * nameset list list * string
  (** Compute a permutation for unzipping tensor products.
   * @return a permutation ~pi such that 
   * merge ((X_{i< n} Ai) x X_{i< n} Mi) ~pi = merge X_{i< n} Ai x Mi,
   * where Ai : < li, Uis> -> Ji and Mi : < li', U'is> -> J'i.
   * @params Uiss U'iss
   * @param Uiss   list of Uis.
   * @param U'iss  list of U'is.
   * @exception UnequalLengths  if the lists have different lengths.
   *)
  val unzip 
      : nameset list list -> nameset list list -> 'kind permutation
  (** Swap _destructively_ to what pi maps index i and j.
   * The inner face is preserved, the outer face will change
   * if the namesets of i and j differ (and i <> j).
   * @params pi i j
   * @param pi  The permutation to update.
   * @return    The updated pi.
   *)
  val swap : Mutable permutation -> int * int -> 'kind permutation
  (** Return a shallow copy of the permutation. *)
  val copy : 'kind permutation -> Mutable permutation
  (** Return the permutation. *)
  val unchanged : 'kind permutation -> Immutable permutation
  (** Determine whether some permutation is the identity. *)
  val is_id : 'kind permutation -> bool
  (** Determine whether some permutation is the identity of width 0. *)
  val is_id0 : 'kind permutation -> bool

  (** Signals that there are no more new permutations. *)
  exception NoMorePerms
  (** Return the first permutation in the ordering.
   *
   * @params Xs
   * @param Xs  The local innerface of the permutation.
   *)
  val firstperm   : nameset list -> Mutable ordered_permutation
  (** Return the first permutation in the ordering.
   *
   * @params n
   * @param n  The width of the permutation.
   *)
  val firstperm_n : int -> Mutable ordered_permutation
  (** Update _destructively_ permutation p and return it as the next
   * permutation in the ordering.  The inner face of the permutation
   * is preserved (cf. swap).
   *
   * @params p
   * @param  The permutation to update.
   *)
  val nextperm : Mutable ordered_permutation -> Mutable ordered_permutation
  (** Return the current permutation of an ordered permutation.
   *
   * @params p
   * @param  The ordered permutation.
   *)
  val toperm : 'kind ordered_permutation -> Immutable permutation

  (** Prettyprint a permutation.
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param pi      The permutation to output.
   *)
  val pp : int -> PrettyPrint.ppstream -> 'kind permutation -> unit
  val oldpp : int -> PrettyPrint.ppstream -> 'kind permutation -> unit
  (** Return a prettyprinted string representation of a permutation. *)
  val toString : 'kind permutation -> string
end
