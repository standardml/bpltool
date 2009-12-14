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

(** Structure for generating ordered permutations of lists.
 * @version $LastChangedRevision: 2717 $
 *)
structure OrderedPermutation : ORDERED_PERMUTATION =
struct
  (* An ordered permutation is a permutation with additional data that
   * supports incremental generation of all permutations of this width.
   *)
  type 'a permutation = (int * 'a) array
  datatype dir = Left | Right
  type 'a ordered_permutation = int * 'a permutation * (int * dir) list

  val array = Array.array
  val sub = Array.sub
  infix 8 sub
  val update = Array.update
  fun appi f array 
    = (Array.foldl (fn (x, i) => (f (i, x); i + 1)) 0 array; ())
  val afoldr = Array.foldr
  val acopy = Array.copy

  (* Return the first permutation in the ordering. *)
  fun firstperm xs =
    let
      fun poslist 0 = []
        | poslist n = (n - 1, Left) :: poslist (n - 1)
      val pi = Array.fromList (ListUtil.mapi (fn x => x) xs)
      val n = Array.length pi
    in
      (n, pi, poslist n)
    end

  fun swap (pi : 'a permutation) (i, j) =
    let
      val iimg = pi sub i
      val jimg = pi sub j
    in
      (  update (pi, i, jimg)
       ; update (pi, j, iimg)
       ; pi)
    end

  (* Update _destructively_ permutation p and return it as the next
   * permutation in the ordering.  The inner face of the permutation
   * is preserved (cf. swap).
   * This implementation uses the Johnson-Trotter algorithm.
   *)
  exception NoMorePerms
  fun nextperm (n, pi, poss) =
    let
      fun np offset i []  = raise NoMorePerms
        | np offset i [_] = raise NoMorePerms
        | np offset i ((p, Left) :: ps) =
          if (p <= 0) then
            (p, Right) :: np (offset + 1) (i - 1) ps
          else
            (  swap pi (offset + p - 1, offset + p)
             ; (p - 1, Left) :: ps)
        | np offset i ((p, Right) :: ps) =
          if (p >= i - 1) then
            (p, Left) :: np offset (i - 1) ps
          else
            (swap pi (offset + p, offset + p + 1);
             (p + 1, Right) :: ps)
      val newposs = np 0 n poss
    in
      SOME (n, pi, newposs)
    end
    handle NoMorePerms => NONE

  fun list (_, pi, _) = afoldr (fn ((_, x), acc) => x :: acc) [] pi
end
