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

(** Structure generating set-injections.
 * @version $LastChangedRevision: 442 $
 *)
functor SetInjection (
  structure SSet : MONO_SET
  structure RSet : MONO_SET
  structure RSubset : SUBSET
  structure Map : MONO_FINMAP
  sharing type SSet.elt =
               Map.dom
  sharing type RSet.Set =
               RSubset.set
) : SETINJECTION
  where type S   = SSet.Set
    and type R   = RSet.Set
    and type inj = RSet.elt Map.map =
struct

type S   = SSet.Set
type R   = RSet.Set
type inj = RSet.elt Map.map

(* Generate all injections of set S into set R.
 * Algorithm:
 * 1. choose an ordering [s_1,...,s_n] for S
 * 2. for each subset R' of R with |R'| = n
 *    a. choose an ordering [r'_1,...,r'_n]
 *    b. for each permutation pi : n -> n
 *       i. add the injection [s_1 |-> pi(r'_1),...,s_n |-> pi(r'_n)]
 *
 * Each injection is paired with the complement R'C of the subset R'.
 *)
fun injections S R =
  let
    val ss = SSet.list S
    val n  = SSet.size S
    val R'_gen = RSubset.make R

    fun next_perm R'C NONE acc = acc
      | next_perm R'C (SOME pi_gen) acc =
      let
        val pi_r's = OrderedPermutation.list pi_gen
        val inj    = ListPair.foldr Map.add Map.empty (ss, pi_r's)
      in
        next_perm R'C (OrderedPermutation.nextperm pi_gen)
                  ((inj, R'C)::acc)
      end

    fun next_subset R'_gen acc =
      let
        val (R', R'C) = RSubset.next_eq' R'_gen n
        val r's       = RSet.list R'
        val pi_gen    = OrderedPermutation.firstperm r's
      in
        next_subset R'_gen (next_perm R'C (SOME pi_gen) acc)
      end
      handle RSubset.NoSubsets => acc
  in
    next_subset R'_gen []
  end

end
