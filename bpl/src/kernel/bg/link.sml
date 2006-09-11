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

(** Abstract data type for modelling individual links.
 * @version $Revision: 1.4 $
 *)
functor Link (structure Name : NAME
	      structure NameSet : MONO_SET
	      structure NameSetCompare : SETCOMPARE
	      sharing type NameSet.elt = Name.name
	      sharing type NameSetCompare.T = NameSet.Set) :> LINK 
              where type name = Name.name 
	        and type nameset = NameSet.Set =
struct
  type name = Name.name
  structure NameSet = NameSet
  type nameset = NameSet.Set

  type link = {outer : name option, inner : nameset}
  fun make link = link
  fun unmk link = link
  val innernames : link -> nameset = #inner

  structure Order : ORDERING =
  struct
  type T = link
  fun lt {outer = outer1, inner = inner1}
	 {outer = outer2, inner = inner2} = 
      case (outer1, outer2) of
	(NONE, SOME _) => true
      | (SOME _, NONE) => false
      | (NONE, NONE) => NameSetCompare.lt inner1 inner2
      | (SOME x1, SOME x2) => 
	if x1 = x2 then 
	  NameSetCompare.lt inner1 inner2
	else
	  Name.unmk x1 < Name.unmk x2
  end
end
