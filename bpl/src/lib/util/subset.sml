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

functor Subset
  (structure LazyList : LAZYLIST
   structure Set      : MONO_SET)
  :> SUBSET where type set = Set.Set =
struct
  open LazyList

  type set = Set.Set
  type subsetgen = set lazylist ref

  exception NoSubsets

  fun make X =
      let
        fun all_subsets []      = lzCons (fn () => (Set.empty, lzNil))
          | all_subsets (e::es) = lzmake (fn () =>
            lzunmk
              (lzfoldr (fn (X', rest) => 
                           lzCons (fn () => (Set.insert e X',
                           lzCons (fn () => (X', rest ())))))
                       lzNil (all_subsets es)))
      in
        ref (all_subsets (Set.list X))
      end
  
  fun next sg =
      case lzunmk (!sg) of
         Nil         => raise NoSubsets
       | Cons (r, g) => (sg := g; r)

  (* FIXME inefficient *)
  fun next' sg m =
      let
        val s = next sg
      in
        if Set.size s >= m then
          s
        else
          next' sg m
      end
end

