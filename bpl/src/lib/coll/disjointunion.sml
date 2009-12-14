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
functor DisjointUnion (
  structure SSet : MONO_SET
  structure RSet : MONO_SET
  structure USet : MONO_SET
  val inl : SSet.elt -> USet.elt
  val inr : RSet.elt -> USet.elt
  val cas : (SSet.elt -> 'a) * (RSet.elt -> 'a) -> USet.elt -> 'a
) : DISJOINT_UNION
  where type S = SSet.Set
    and type R = RSet.Set =
struct

type S = SSet.Set
type R = RSet.Set
type U = USet.Set

local
  fun insert inx e U = USet.insert (inx e) U
in
  fun disjoint_union S R =
      SSet.fold (insert inl) (RSet.fold (insert inr) USet.empty R) S
    
  fun split U =
      USet.fold (fn t => fn (S, R) =>
                    cas (fn s => (SSet.insert s S, R),
                         fn r => (S, RSet.insert r R)) t)
                (SSet.empty, RSet.empty) U
end

end
