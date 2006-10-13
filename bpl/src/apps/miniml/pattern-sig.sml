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

(** (Full) patterns used during parsing - later compiled away by
 * match compilation.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/05/31 15:01:26 $ by: $Author: hniss $
 *)

signature PATTERN = sig

    datatype con = 
         TupleCon of int
       | ConstCon of { name : string, arity : int, span : int }
    datatype pat = PVar of string | PCon of con * pat list

    structure ConSet : MONO_SET
			   where type elt = con

    val arity : con -> int
    val span  : con -> int

    val ppCon : con Pretty.pp
    val ppPat : pat Pretty.pp

end (* signature PATTERN *)
