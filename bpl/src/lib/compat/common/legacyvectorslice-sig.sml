(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** VectorSlice signature from the new SML Basis Library
 * @version $LastChangedRevision: 442 $
 *)

signature VECTOR_SLICE =
sig
    type 'a slice
    val length : 'a slice -> int
    val sub : 'a slice * int -> 'a
    val full : 'a Vector.vector -> 'a slice
    val slice : 'a Vector.vector * int * int option -> 'a slice
    val subslice : 'a slice * int * int option -> 'a slice
    val base : 'a slice -> 'a Vector.vector * int * int
    val vector : 'a slice -> 'a Vector.vector
    val concat : 'a slice list -> 'a Vector.vector
    val isEmpty : 'a slice -> bool
    val getItem : 'a slice -> ('a * 'a slice) option
    val appi : (int * 'a -> unit) -> 'a slice -> unit
    val app  : ('a -> unit) -> 'a slice -> unit
    val mapi : (int * 'a -> 'b) -> 'a slice -> 'b Vector.vector
    val map  : ('a -> 'b) -> 'a slice -> 'b Vector.vector
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
    val findi : (int * 'a -> bool)
                  -> 'a slice -> (int * 'a) option
    val find  : ('a -> bool) -> 'a slice -> 'a option
    val exists : ('a -> bool) -> 'a slice -> bool
    val all : ('a -> bool) -> 'a slice -> bool
    val collate : ('a * 'a -> order)
                    -> 'a slice * 'a slice -> order 
end