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

(** Abstract data type for bigraph terms.
 * The terms are not necessarily well-formed:
 * scope rule may be violated in abstractions, names may clash in
 * interfaces of tensor product operands, bigraph widths may be
 * incompatible in compositions, etc.
 * <p>
 * Each constructor takes an info argument that can contain contextual
 * information (e.g., source file location for the term).
 * @version $LastChangedRevision$
 *)
signature BGTERM =
sig
  type info
  type nameset
  type control
  type ion
  type Immutable
  type 'kind permutation
  type wiring

  (** The bgterm data type. *)
  datatype bgterm = 
	 (** merge_n = one root containing n sites. *)
	   Mer of int * info
	 (** "X" = a concretion of a set of names. *)
	 | Con of nameset * info
	 (** w = a general wiring. *)
	 | Wir of wiring * info
	 (** K_yX = an ion. *)
	 | Ion of ion * info
	 (** pi = a permutation. *)
	 | Per of Immutable permutation * info
	 (** (X)P = an abstraction. *)
	 | Abs of nameset * bgterm * info
	 (** b_0 x...x b_n-1 = a tensor product of n bigraphs. *)
	 | Ten of bgterm list * info
	 (** b_0 |...| b_n-1 = a prime product of n bigraphs. *)
	 | Pri of bgterm list * info
	 (** b_0 ||...|| b_n-1 = a parallel product of n bigraphs. *)
	 | Par of bgterm list * info
	 (** b_1 b_2 = a composition of a pair of bigraphs. *)
	 | Com of bgterm * bgterm * info
  (** Construct a wide local substitution.  The width of the
   * substitution is determined by the length of the list of wirings. 
   *)
  val WLS : info -> wiring list -> bgterm
  (** Return the contextual information of a given bgterm. *)
  val info : bgterm -> info
  (** Test two bigraph terms for equality (same structure and same names).
   * @params b1 b2
   * @param b1  the first bigraph term.
   * @param b2  the second bigraph term.
   *)
  val eq : bgterm -> bgterm -> bool
  (** Indicate that an unlisted control was encountered. *)
  exception UnknownControl of bgterm
  (** Indicate that a control was applied to a wrong number of free
   * names or bound name sets.
   *)
  exception WrongArity of bgterm
  (** Replace controls.  Each ion control is looked up by name in
   * the control table and replaced by that entry.
   * @exception  UnknownControl if the term contains a control not listed.
   *)
  val replacecontrols : control list -> bgterm -> bgterm
  (** Prettyprint a bgterm without parentheses around it.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgterm to print.
   *)
  val pp : int -> PrettyPrint.ppstream -> bgterm -> unit
  val oldpp : int -> PrettyPrint.ppstream -> bgterm -> unit
  
  val size : bgterm -> int

end
