(* Copyright (c) 2007  Henning Niss, IT University of Copenhagen
 *
 * BAM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BAM is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BAM; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

structure Stack :> STACK = struct

    type 'a t = 'a list
    exception Empty
    val empty = []
    val isEmpty = List.null
    fun push x S = x::S
    fun pop [] = raise Empty
      | pop (x::S) = (x, S)

    val compare = Util.listCmp

    fun take n s =
	let val n' = if n > List.length s then List.length s else n
	in  List.take(s, n')
	end

    local
	open Pretty
    in
    fun pp sep pp0 s = 
	(clist (sep^"#") pp0 s +^ sep) +^ "·"
    end

end (* structure Stack *)
