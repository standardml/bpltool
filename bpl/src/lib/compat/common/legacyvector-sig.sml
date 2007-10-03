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

(** VECTOR signature from the new SML Basis Library
 * @version $LastChangedRevision: 442 $
 *)

signature VECTOR =
sig
    eqtype 'a vector
    val maxLen : int
    val fromList : 'a list -> 'a vector
    val tabulate : int * (int -> 'a) -> 'a vector
    val length : 'a vector -> int
    val sub : 'a vector * int -> 'a
    val update : 'a vector * int * 'a -> 'a vector
    val concat : 'a vector list -> 'a vector
    val appi : (int * 'a -> unit) -> 'a vector -> unit
    val app  : ('a -> unit) -> 'a vector -> unit
    val mapi : (int * 'a -> 'b) -> 'a vector -> 'b vector
    val map  : ('a -> 'b) -> 'a vector -> 'b vector
    val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldl  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val foldr  : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
    val findi : (int * 'a -> bool)
                  -> 'a vector -> (int * 'a) option
    val find  : ('a -> bool) -> 'a vector -> 'a option
    val exists : ('a -> bool) -> 'a vector -> bool
    val all : ('a -> bool) -> 'a vector -> bool
    val collate : ('a * 'a -> order)
                    -> 'a vector * 'a vector -> order 
end