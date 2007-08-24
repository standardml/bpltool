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

signature TERM = sig
    type 'cinfo t

    val Par : 'cinfo t * 'cinfo t -> 'cinfo t
    val ParList : 'cinfo t list -> 'cinfo t
    val Prefix : 'cinfo Control.t * 'cinfo t -> 'cinfo t
    val Nil : 'cinfo t
    val Hole : int -> 'cinfo t

    val map : ('cinfo -> 'newcinfo) -> 'cinfo t -> 'newcinfo t

    val toplevels : 'cinfo t -> 'cinfo t list
    val exists : ('cinfo -> bool) -> 'cinfo t -> bool
    val compare : 'cinfo t * 'cinfo t -> order
    val equal : 'cinfo t * 'cinfo t -> bool

    type holemap = int Util.IntMap.map
    val renumber : (int -> unit) (* applied if a the hole number is not
			            present in the map *)
                   -> holemap -> 'cinfo t -> holemap * 'cinfo t

    val holeIndices : 'cinfo t -> int Rbset.set
    val maxHoleIndex : 'cinfo t -> int

    val plug : 'cinfo t vector -> 'cinfo t -> 'cinfo t
    val plug1 : int * 'cinfo t -> 'cinfo t -> 'cinfo t

    val pp : 'cinfo t Pretty.pp
    val pp' : 'cinfo Control.t Pretty.pp -> 'cinfo t Pretty.pp
    val toString : 'cinfo t -> string

    datatype 'cinfo view =
	     VPar of 'cinfo t * 'cinfo t
           | VPrefix of 'cinfo Control.t * 'cinfo t
           | VNil
	   | VHole of int

    val view : 'cinfo t -> 'cinfo view

    datatype 'cinfo pattern =
	     PSuccess
	   | PVar of string
	   | PPar of 'cinfo pattern * 'cinfo pattern
	   | PPrefix of 'cinfo Control.t * 'cinfo pattern
	   | PPrefixed of string * 'cinfo pattern
	   | PHole of int
	   | PHoled of string
	   | PNil

    type 'cinfo match

    val match : 'cinfo pattern -> 'cinfo t -> 'cinfo match option
    val lookup : 'cinfo match -> string -> 'cinfo t option
    val lookupCtrl : 'cinfo match -> string -> 'cinfo Control.t option
    val lookupHole : 'cinfo match -> string -> int option

end
