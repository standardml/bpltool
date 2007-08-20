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

structure Control :> CONTROL = struct
    datatype activity = ACTIVE | PASSIVE
    type 'ctrlinfo t = string * 'ctrlinfo * activity

    fun ctrl x = x
    val name     : 'info t -> string   = #1
    val info     : 'info t -> 'info    = #2
    val activity : 'info t -> activity = #3

    fun isActive  C = activity C = ACTIVE
    fun isPassive C = activity C = PASSIVE

    fun predi p C = p C
    fun pred  p C = predi (fn (name,info,act) => p info) C

    fun mapi f C = f C
    fun map  f C = mapi (fn (name,info,act) => (name,f info, act)) C

    open Pretty
    fun pp' ppInfo C = 
	let val infotree = ppInfo (info C)
	    val ctree = ppString (name C)
	in  if isPrinting infotree then ctree ++ (bracket "{#}" infotree)
	    else ctree
	end
    val pp = pp' (fn _ => empty)
end 
