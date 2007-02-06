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

(** Prettyprinter for mono- and polymorphic lists
 * @version $LastChangedRevision$
 *)
functor ListPrettyPrint 
	  (type elt
	   structure PrettyPrint : PRETTYPRINT
	   val pp_elt : int -> PrettyPrint.ppstream -> elt 
			-> unit) :> COLLECTIONPRETTYPRINT
  where type ppstream = PrettyPrint.ppstream 
    and type collection = elt list =
struct
  type collection = elt list
  type ppstream = PrettyPrint.ppstream
  (** Prettyprint a given list.
   * @params indent xs
   * @param indent  Indentation to use at each block level.
   * @param xs       The list to print.
   *)
  fun ppbr indent leftb rightb pps xs =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun pp_e (e, notfirst) =
	    (if notfirst then (show ","; brk()) else ();
	     pp_elt indent pps e;
	     true)
      in
	<<(); show leftb; foldl pp_e false xs; show rightb; >>()
      end
  fun pp indent = ppbr indent "[" "]"
end

functor PolyListPrettyPrint 
	  (structure PrettyPrint : PRETTYPRINT) :> POLYCOLLECTIONPRETTYPRINT
  where type ppstream = PrettyPrint.ppstream 
    and type 'a collection = 'a list =
struct
  type 'a collection = 'a list
  type ppstream = PrettyPrint.ppstream
  (** Prettyprint a given list.
   * @params indent xs
   * @param indent  Indentation to use at each block level.
   * @param xs       The list to print.
   *)
  fun ppbr pp_elt indent leftb rightb pps xs =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun pp_e (e, notfirst) =
	    (if notfirst then (show ","; brk()) else ();
	     pp_elt indent pps e;
	     true)
      in
	<<(); show leftb; foldl pp_e false xs; show rightb; >>()
      end
  fun pp pp_elt indent = ppbr pp_elt indent "[" "]"
end

