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
    type 'ctrlinfo t
    val rule : 'ctrlinfo Term.t * 'ctrlinfo Term.t -> 'ctrlinfo t
    val LHS : 'ctrlinfo t -> 'ctrlinfo Term.t
    val RHS : 'ctrlinfo t -> 'ctrlinfo Term.t
    val map : ('ctrlinfo -> 'newctrlinfo) -> 'ctrlinfo t -> 'newctrlinfo t
    val compare : 'ctrlinfo t * 'ctrlinfo t -> order
    val pp : 'ctrlinfo t Pretty.pp
end 
