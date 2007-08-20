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

signature CONTROL = sig
    type 'ctrlinfo t

    datatype activity = ACTIVE | PASSIVE

    val ctrl : string * 'ctrlinfo * activity -> 'ctrlinfo t

    val name : 'ctrlinfo t -> string
    val info : 'ctrlinfo t -> 'ctrlinfo
    val activity : 'ctrlinfo t -> activity
    val isActive : 'ctrlinfo t -> bool
    val isPassive : 'ctrlinfo t -> bool

    val map : ('ctrlinfo -> 'newctrlinfo) -> 'ctrlinfo t -> 'newctrlinfo t
    val pred : ('ctrlinfo -> bool) -> 'ctrlinfo t -> bool

    val mapi : (string * 'ctrlinfo * activity -> string * 'newctrlinfo * activity) 
	       -> 'ctrlinfo t -> 'newctrlinfo t
    val predi : (string * 'ctrlinfo * activity -> bool) -> 'ctrlinfo t -> bool

    val pp : 'ctrlinfo t Pretty.pp
    val pp' : 'ctrlinfo Pretty.pp -> 'ctrlinfo t Pretty.pp
end 
