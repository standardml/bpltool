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

signature UTIL = sig
    exception ShouldntHappen of int
    val abort : int -> 'a

    val stringSep : string -> string -> string (* start, finish, sep *)
		      -> ('a -> string) -> 'a list -> string

    val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c

    val pairCmp : ('x * 'x -> order) * ('y * 'y -> order) -> ('x * 'y) * ('x * 'y) -> order 
    val listCmp : ('a * 'a -> order) -> ('a list * 'a list) -> order
    val vectorCmp : ('a * 'a -> order) -> ('a vector * 'a vector) -> order

    structure StringMap : MONO_FINMAP where type dom = string

    val ppSet : 'a Pretty.pp -> 'a Rbset.set Pretty.pp
end
