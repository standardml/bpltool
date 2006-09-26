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

(** Array structure matching the ARRAY signature from
 * the new SML Basis Library, but based on the old
 * SML Basis Library Array structure.
 * @version $LastChangedRevision$
 *)

structure Array :> ARRAY =
struct
  open Array
  type 'a vector = 'a Vector.vector
  val appi = fn f => fn array => appi f (array, 0, NONE)
  val copy = fn {src, dst, di} 
		=> copy {src = src, dst = dst, di = di,
			 si = 0, len = NONE}
  val copyVec = fn {src, dst, di} 
		=> copyVec {src = src, dst = dst, di = di,
			 si = 0, len = NONE}
  val vector = fn array => extract (array, 0, NONE) 
  val modifyi = fn f => fn array => modifyi f (array, 0, NONE)
  val foldli = fn f => fn base => fn array
		=> foldli f base (array, 0, NONE)
  val foldri = fn f => fn base => fn array
		=> foldri f base (array, 0, NONE)
  fun findi f array = 
      let
	fun findi' i = 
	    if i < length array then
	      let
		val elt = sub (array, i)
	      in
		if f (i, elt) then 
		  SOME (i, elt)
		else 
		  findi' (i + 1)
	      end
	    else
	      NONE
      in
	findi' 0
      end
  fun find f array = case findi (fn (_, elt) => f elt) array of
		       SOME (_, elt) => SOME elt
		     | NONE => NONE
  fun exists f array = case find f array of SOME _ => true
					  | NONE => false
  fun all f = not o exists (not o f)
  fun collate R (array1, array2) =
      let
	val l1 = length array1
	val l2 = length array2
	fun collate' i = if i >= l1 then
			   if i >= l2 then EQUAL else LESS
			 else
			   if i >= l2 then
			     GREATER
			   else
			     case R (sub (array1, i), sub (array2, i)) of
			       EQUAL => collate' (i + 1)
			     | result => result
      in
	collate' 0
      end
end
