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

(** Abstract data type for modelling ions.
 * @version $LastChangedRevision$
 *)

functor Ion'(structure Control : CONTROL
	     structure Name : NAME
	     structure NameSet : MONO_SET
	     structure NameSetPP : COLLECTIONPRETTYPRINT
               where type ppstream = PrettyPrint.ppstream
	     sharing type Name.name = NameSet.elt
	     sharing type NameSet.Set = NameSetPP.collection) : ION 
	     where type control  = Control.control
	       and type name     = Name.name
 	       and type nameset  = NameSet.Set =
struct
  type control  = Control.control
  type name     = Name.name
  type nameset  = NameSet.Set

  type ion = {ctrl : control, free : name list, bound : nameset list}
  fun make x = x
  fun unmk x = x
  fun innernames {ctrl, free, bound} =
      foldl (fn (Xs, X) => NameSet.union X Xs) NameSet.empty bound
  fun outernames ({ctrl, free, bound}) = NameSet.fromList free

  fun pp indent pps ({ctrl, free, bound} : ion) =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun <<< () = begin_block pps CONSISTENT indent
	fun >>> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun brk0 () = add_break pps (0, 0)
	fun pplist pp_y ys =
	    (<<(); show "<";
	     foldl (fn (y, notfirst) => 
		       (if notfirst then (show ","; brk()) else ();
			pp_y y;
			true)) false ys;
	     show ">"; >>())
      in
	<<<();
	show (#1 (Control.unmk ctrl));
	case bound of
	  [] =>
	  (case free of
	     [] => ()
	   | (y :: ys) => pplist (show o Name.unmk) free)
	| (X :: Xs) =>
	  (brk0();
	   pplist (show o Name.unmk) free;
	   brk0();
	   pplist (NameSetPP.pp indent pps) bound);
	>>>()
      end
end


functor Ion (structure Control : CONTROL
	     structure Name : NAME
	     structure NameSet : MONO_SET
	     structure NameSetPP : COLLECTIONPRETTYPRINT
               where type ppstream = PrettyPrint.ppstream
	     sharing type Name.name = NameSet.elt
	     sharing type NameSet.Set = NameSetPP.collection) :> ION 
	     where type control  = Control.control
	       and type name     = Name.name
 	       and type nameset  = NameSet.Set =
struct
  structure Ion = Ion'(structure Control = Control
		       structure Name = Name
		       structure NameSet = NameSet
		       structure NameSetPP = NameSetPP)
  open Ion
end