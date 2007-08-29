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

signature RULE = sig
    exception NotWellFormed of string

    type 'ctrlinfo t

    (* Rules are renumbered so that hole indices are consecutive
       and starts from 0. Furthermore, it is checked that all
       holes in the RHS occurs in the LHS.
    *)
    val rule : 'ctrlinfo Process.t * 'ctrlinfo Process.t -> 'ctrlinfo t
    val LHS : 'ctrlinfo t -> 'ctrlinfo Process.t
    val RHS : 'ctrlinfo t -> 'ctrlinfo Process.t
    val holeIndices : 'ctrlinfo t -> int Rbset.set
    val maxHoleIndex : 'ctrlinfo t -> int
    val map : ('ctrlinfo -> 'newctrlinfo) -> 'ctrlinfo t -> 'newctrlinfo t
    val compare : 'ctrlinfo t * 'ctrlinfo t -> order
    val pp : 'ctrlinfo t Pretty.pp
    val toString : 'ctrlinfo t -> string
end 
