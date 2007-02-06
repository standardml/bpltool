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

(** Structure providing comparison of sets.
 * @version $LastChangedRevision$
 *)
functor SetCompare (structure Set : MONO_SET
		    structure EltOrder : ORDERING
		    sharing type Set.elt = EltOrder.T) : SETCOMPARE =
struct
  type T = Set.Set
  fun List_collate R xs ys =
      let
	fun collate [] [] = EQUAL
	  | collate (x :: xs) [] = GREATER
	  | collate [] (y :: ys) = LESS
	  | collate (x :: xs) (y :: ys) =
	    case R (x, y) of
	      EQUAL => collate xs ys
	    | result => result
      in
	collate xs ys
      end
  fun lt set1 set2 =
      let
	val sz1 = Set.size set1
	val sz2 = Set.size set2
      in
	if sz1 < sz2 then
	  true
	else if sz2 < sz1 then
	  false
	else
	  let
	    val x1s = Set.list set1
	    val x2s = Set.list set2
	  in
	    (List_collate (fn (x1, x2) => 
			      if EltOrder.lt x1 x2 then
				LESS
			      else if EltOrder.lt x2 x1 then
				GREATER
			      else
				EQUAL)
			  x1s 
			  x2s)
	    = LESS
	  end
      end
end
