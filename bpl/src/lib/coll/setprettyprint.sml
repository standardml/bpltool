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

(** Prettyprinter for monomorphic sets
 * @version $Revision: 1.4 $
 *)
functor SetPrettyPrint 
	  (structure Set : MONO_SET
	   structure PrettyPrint : PRETTYPRINT
	   val pp_elt : int -> PrettyPrint.ppstream -> Set.elt 
			-> unit) :> COLLECTIONPRETTYPRINT
  where type ppstream = PrettyPrint.ppstream 
    and type collection = Set.Set =
struct
  type collection = Set.Set
  type ppstream = PrettyPrint.ppstream
  (** Prettyprint a given set.
   * @params indent S
   * @param indent  Indentation to use at each block level.
   * @param S       The set to print.
   *)
  fun ppbr indent leftb rightb pps S =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun pp_e e notfirst =
	    (if notfirst then (show ","; brk()) else ();
	     pp_elt indent pps e;
	     true)
      in
	<<(); show leftb; Set.fold pp_e false S; show rightb; >>()
      end
  fun pp indent = ppbr indent "{" "}"
end
