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

(** Type inference algorithm.
 * @version $Revision: 1.1 $
 * Modified: $Date: 2006/05/31 15:01:26 $ by: $Author: hniss $
 *)

signature TYPEINFERENCE = sig

    type pos = int * int
    exception TypeError of pos * string list

    type typeexp = TypeExp.typeexp

    val inference : ('info1 -> pos) -> (pos -> typeexp -> 'info2)
		    -> ('info1, Pattern.pat) MiniML.prog
		    -> ('info2, Pattern.pat) MiniML.prog
end (* signature TYPEEXP *)
