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
 * @version $Revision: 1.6 $
 *)
signature PERMUTATION =
sig
  type nameset
  type interface
  type ppstream

  (** The permutation data type. *)
  type permutation
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
  val make : (int * nameset) list -> permutation
  (** Deconstruct a permutation. *)
  val unmk : permutation -> (int * nameset) list

  (** Return the width of a permutation. *)
  val width : permutation -> int
  (** Return the inner face of a permutation *)
  val innerface : permutation -> interface
  (** Return the outer face of a permutation *)
  val outerface : permutation -> interface

  (** Return an identity permutation. 
   * The width of the permutation is given by the length of Xs.
   * @params Xs
   * @param Xs  list of local name sets.
   *)
  val id : nameset list -> permutation
  (** Return a nameless identity permutation.
   * @params n
   * @param m  width of the permutation.
   *)
  val id_n : int -> permutation
  (** Return the empty identity permutation. *)
  val id_0 : permutation

  (** Signal that two permutations cannot be composed. *)
  exception Uncomposable of string * permutation * permutation * string

  (** Return the composition of two permutations. 
   * @exception Uncomposable raised if the interfaces do not match.
   *)
  val o : (permutation * permutation) -> permutation

  (** Return the tensor product of two permutations. *)
  val * : (permutation * permutation) -> permutation

  (** Return the tensor product of a list of permutations. *)
  val ** : permutation list -> permutation

  (** Return the inverse of a permutation.
   * @params pi
   * @param pi  permutation to invert.
   * @return    the inverse of pi.
   *)
  val invert
      : permutation -> permutation
  (** Permute the list of values as described by the permutation.
    * @params pi Xs
    * @param pi  the permutation.
    * @param Xs  the list of values to permute.
    *)
  val permute
      : permutation -> 'a list -> 'a list
  (** Push a permutation through a product of primes.
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
      : permutation -> nameset list list -> permutation

  (** Signal that a permutation is not regularizable relative to a list
   *  of local inner name lists.
   *)
  exception NotRegularisable of string * permutation * nameset list list
  (** Split a permutation into one major and a number of minor
   * permutations.
   * @params pi Xss
   * @param Xss  list of local inner name lists.
   * @exception NotRegularisable  if the permutation cannot be
   *                              regularized.
   *)
  val split
      : permutation -> nameset list list
        -> {major : permutation, minors : permutation list}
  (** Compute a permutation for unzipping tensor products.
   * @return a permutation ~pi such that 
   * merge ((X_{i< n} Ai) x X_{i< n} Mi) ~pi = merge X_{i< n} Ai x Mi,
   * where Ai : < li, Uis> -> Ji and Mi : < li', U'is> -> J'i.
   * @params Uiss U'iss
   * @param Uiss   list of Uis.
   * @param U'iss  list of U'is.
   *)
  val unzip 
      : nameset list list -> nameset list list -> permutation
  (** Determine whether some permutation is the identity. *)
  val is_id : permutation -> bool
  (** Determine whether some permutation is the identity of width 0. *)
  val is_id0 : permutation -> bool
  (** Prettyprint a permutation.
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param pi      The permutation to output.
   *)
  val pp : int -> ppstream -> permutation -> unit
end
