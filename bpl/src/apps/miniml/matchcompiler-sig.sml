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

(** Match compiler for MiniML.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/05/31 15:01:25 $ by: $Author: hniss $
 *)

signature MATCHCOMPILER = sig

    type pat = Pattern.pat
    type pos = int * int
    exception NonExhaustiveMatch of pos
    val compile : ('info1 -> pos) -> 'info1 -> ('info1 -> ''info2)
		  -> ('info1,pat) MiniML.prog -> (''info2,string*string) MiniML.prog

end (* signature MATCHCOMPILE *)
