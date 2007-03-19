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

(** Naïve implementation based on the Partition module.
 *)
functor OrderedPartition
  (structure LazyList : LAZYLIST
   structure Partition : PARTITION)
  :> ORDERED_PARTITION =
struct
  open LazyList

  type 'a opartitiongen = 'a list list lazylist ref

  exception NoPartitions

  (* generate all permutations of a list of lists
   * excluding the ones where only empty sublists change places.
   * FIXME inefficient! *)
  fun all_perms []   = lzCons (fn () => ([], lzNil))
    | all_perms list =
      let
        (* We consecutively place each element of the list in
         * front and permute the remaining elements recursively.
         * We allow at most one empty list to be placed in the
         * front: notempty = true means that we have already
         * created the permutations with an empty list in front. *)
        fun next_permutation _ _ []                 = lzNil
          | next_permutation true pre ([]::post)    =
            next_permutation true ([]::pre) post
          | next_permutation notempty pre (e::post) = lzmake (fn () =>
            lzunmk
              (lzappend
                 (lzmap
                    (fn l => e::l)
                    (all_perms (foldl (op ::) post pre)))
                 (next_permutation
                    (notempty orelse null e)
                    (e::pre)
                    post)))
      in
        next_permutation false [] list
      end

  fun make list m =
      let
        (* use a partition generator to
         * generate the 'basis' partitions *)
        val pg = Partition.make list m
                 handle Partition.NoPartitions => raise NoPartitions

        (* generate all permutations for each partition *)
        fun next () = lzmake (fn () =>
            lzunmk (lzappend (all_perms (Partition.next pg)) (next ()))
            handle Partition.NoPartitions => Nil)
      in
        ref (next ())
      end

  fun next opg =
      case lzunmk (!opg) of
         Nil         => raise NoPartitions
       | Cons (r, g) => (opg := g; r)

  (* FIXME inefficient! *)
  fun next' opg =
      let 
        val part = next opg
      in
        if List.all (fn l => length l > 0) part then
          part
        else
          next' opg
      end
end

