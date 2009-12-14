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

(** Structure generating maps between sets.
 * @version $LastChangedRevision: 442 $
 *)
functor SetMap (
  structure SSet : MONO_SET
  structure RSet : MONO_SET
  structure Map : MONO_FINMAP
  sharing type SSet.elt =
               Map.dom
) : SETMAP
  where type S   = SSet.Set
    and type R   = RSet.Set
    and type map = RSet.elt Map.map =
struct

type S   = SSet.Set
type R   = RSet.Set
type map = RSet.elt Map.map

fun maps S R =
  let
    fun maps_s2r s map_list r acc =
      foldr
        (fn (map, acc) => Map.add (s, r, map) :: acc)
        acc map_list

    fun maps_s s map_list =
      RSet.fold (maps_s2r s map_list) [] R
in
  SSet.fold maps_s [Map.empty] S
end

end
