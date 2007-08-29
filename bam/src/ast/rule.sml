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

structure Rule :> RULE = struct
    exception NotWellFormed of string
    type 'ctrlinfo t = 'ctrlinfo Process.t * 'ctrlinfo Process.t

    fun rule (lhs, rhs) =
	let fun unbound i = 
		raise NotWellFormed("unknown hole "^ Int.toString i ^" in RHS")
	    val (map,lhs') = Process.renumber (fn i => ()) Util.IntMap.empty lhs
	    val (_, rhs')  = Process.renumber unbound map rhs
	in  (lhs',rhs')
	end

    val LHS : 'ctrlinfo t -> 'ctrlinfo Process.t = #1
    val RHS : 'ctrlinfo t -> 'ctrlinfo Process.t = #2
    fun holeIndices r = Process.holeIndices (LHS r)
    fun maxHoleIndex r = Process.maxHoleIndex (LHS r)
    fun map f (lhs, rhs) = (Process.map f lhs, Process.map f rhs)
    val compare = Util.pairCmp (Process.compare, Process.compare)
    fun pp (lhs, rhs) = Pretty.ppBinary(Process.pp lhs, "-->", Process.pp rhs)
    val toString = Pretty.ppToString o pp
end (* structure Rule *)
