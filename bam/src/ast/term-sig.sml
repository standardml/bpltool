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
    type 'a t

    type 'a ctrl_id = string * 'a
		   
    val Par : 'a t * 'a t -> 'a t
    val Prefix : 'a ctrl_id * 'a t -> 'a t
    val Nil : 'a t
    val Hole : int -> 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t

    val toplevels : 'a t -> 'a t list
    val exists : ('a -> bool) -> 'a t -> bool
    val compare : 'a t * 'a t -> order

    val plug : 'a t vector -> 'a t -> 'a t
    val plug1 : int * 'a t -> 'a t -> 'a t

    val pp : 'a t Pretty.pp
    val pp' : 'a ctrl_id Pretty.pp -> 'a t Pretty.pp

    datatype 'a view =
	     VPar of 'a t * 'a t
           | VPrefix of 'a ctrl_id * 'a t
           | VNil
	   | VHole of int

    val view : 'a t -> 'a view

    datatype 'a pattern =
	     PSuccess
	   | PVar of string
	   | PPar of 'a pattern * 'a pattern
	   | PPrefix of 'a ctrl_id * 'a pattern
	   | PPrefixed of string * 'a pattern
	   | PHole of int
	   | PHoled of string
	   | PNil

    type 'a match

    val match : 'a pattern -> 'a t -> 'a match option
    val lookup : 'a match -> string -> 'a t option
    val lookupCtrl : 'a match -> string -> 'a ctrl_id option
    val lookupHole : 'a match -> string -> int option

end
