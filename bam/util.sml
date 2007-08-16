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

structure Util :> UTIL =
struct

    exception ShouldntHappen of int
    fun abort code = raise ShouldntHappen code

    local
	fun str _ nil _ _ = ""
	  | str p (h::t) sep needSep =
	    let val s = p h ^ (str p t sep true)
	    in  if needSep then sep ^ s else s
	    end
    in
	fun stringSep start finish sep p l = 
	    start ^ (str p l sep false) ^ finish
    end (* local *)

    fun pairCmp (xcmp,ycmp) ((x1,y1), (x2,y2)) =
	case xcmp (x1, x2) of
	    EQUAL => ycmp(y1, y2)
	  | order => order

    fun listCmp cmp (xs, ys) =
	let fun loop [] [] = EQUAL
              | loop [] (y::ys) = LESS
              | loop (x::xs) [] = GREATER
	      | loop (x::xs) (y::ys) = 
		let val ord = cmp (x,y)
		in  if ord = EQUAL then loop xs ys else ord
		end
	in  loop xs ys
	end

    fun vectorCmp cmp (xs, ys) =
	let val cxs = Vector.length xs
	    val cys = Vector.length ys
	    val len = if cxs > cys then cys else cxs
	    fun loop n =
		if n < len then let val ord = cmp (Vector.sub(xs,n),Vector.sub(ys,n))
				in  if ord = EQUAL then loop (n+1) else ord
				end
		else Int.compare(cxs, cys)
	in  loop 0
	end

    fun curry f x y = f(x,y)

    structure StringMap = OrderFinMap(type T = string
                                      val lt = curry String.<)

    fun ppSet pp s = 
	Pretty.bracket "{#}" (Pretty.clist "#, " pp (Rbset.listItems s))

end (* structure Util *)
