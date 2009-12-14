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

(** Generate subsets of a set. *)
signature SUBSET =
sig
  (** A set type *)
  type set
  (** A subset generator type. *)
  type subsetgen

  (** Signal that there are no more subsets. *)
  exception NoSubsets

  (** Create a subset generator which generates all subsets
   * of a set.
   *
   * @params set
   * @param set  the set.
   * @return a subset generator.
   *)
  val make : set -> subsetgen

  (** Get the next subset from a subset generator.
   * @params subset_gen
   * @param subset_gen  the subset generator.
   * @return a subset of the set given to make.
   * @exception NoSubsets  if no more subsets are available.
   *)
  val next : subsetgen -> set
  (** Get the next subset and its complement from a subset generator.
   * @params subset_gen
   * @param subset_gen  the subset generator.
   * @return a subset and its complement of the set given to make.
   * @exception NoSubsets  if no more subsets are available.
   *)
  val next' : subsetgen -> set * set

  (** Get the next subset from a subset generator which has
   * m elements.
   *
   * @params subset_gen m
   * @param subset_gen  the subset generator.
   * @param m           the number of elements.
   * @return a subset of the set given to make with m elements.
   * @exception NoSubsets  if no more subsets are available.
   *)
  val next_eq : subsetgen -> int -> set
  (** Get the next subset from a subset generator which has
   * m elements. The complement of the subset is given as well.
   *
   * @params subset_gen m
   * @param subset_gen  the subset generator.
   * @param m           the number of elements.
   * @return a subset of the set given to make with m elements.
   *         The complement of the subset is given as well.
   * @exception NoSubsets  if no more subsets are available.
   *)
  val next_eq' : subsetgen -> int -> set * set

  (** Get the next subset from a subset generator which has
   * at least m elements.
   *
   * @params subset_gen m
   * @param subset_gen  the subset generator.
   * @param m           the minimum number of elements.
   * @return a subset of the set given to make with at least m elements.
   * @exception NoSubsets  if no more subsets are available.
   *)
  val next_geq : subsetgen -> int -> set
  (** Get the next subset from a subset generator which has
   * at least m elements. The complement of the subset is given as well.
   *
   * @params subset_gen m
   * @param subset_gen  the subset generator.
   * @param m           the minimum number of elements.
   * @return a subset of the set given to make with at least m elements.
   *         The complement of the subset is given as well.
   * @exception NoSubsets  if no more subsets are available.
   *)
  val next_geq' : subsetgen -> int -> set * set
end
