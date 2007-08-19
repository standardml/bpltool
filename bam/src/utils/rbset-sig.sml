(* Copyright (c) 2001-2007 Ken Friis Larsen, Peter Sestoft
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

(* Rbset -- ordered sets implemented by red-black trees *)
(* Intention: should resemble SML/NJs ORD_SET signature *)

signature Rbset = sig
type 'item set

exception NotFound
exception NonMonotonic

val empty        : ('item * 'item -> order) -> 'item set
val singleton    : ('item * 'item -> order) -> 'item -> 'item set
val add          : 'item set * 'item -> 'item set
val add'         : 'item * 'item set -> 'item set
val addList      : 'item set * 'item list -> 'item set
val isEmpty      : 'item set -> bool
val isSubset     : 'item set * 'item set -> bool
val member       : 'item set * 'item -> bool
val delete       : 'item set * 'item -> 'item set
val numItems     : 'item set ->  int
val getOrder     : 'item set -> ('item * 'item -> order)
val union        : 'item set * 'item set -> 'item set
val intersection : 'item set * 'item set -> 'item set
val difference   : 'item set * 'item set -> 'item set
val listItems    : 'item set -> 'item list
val app          : ('item -> unit) -> 'item set -> unit
val revapp       : ('item -> unit) -> 'item set -> unit
val foldr        : ('item * 'b -> 'b) -> 'b -> 'item set -> 'b
val foldl        : ('item * 'b -> 'b) -> 'b -> 'item set -> 'b
val map          : ('item -> 'newitem) * ('newitem * 'newitem -> order) 
                   -> 'item set -> 'newitem set
val mapPartial   : ('item -> 'newitem option) * ('newitem * 'newitem -> order)
		   -> 'item set -> 'newitem set
val mapMono      : ('item -> 'newitem) * ('newitem * 'newitem -> order) 
                   -> 'item set -> 'newitem set
val find         : ('item -> bool) -> 'item set -> 'item option
val min          : 'item set -> 'item option
val max          : 'item set -> 'item option
val hash         : ('item -> word) -> 'item set -> word
val equal        : 'item set * 'item set -> bool
val compare      : 'item set * 'item set -> order 

val depth        : 'item set -> int

datatype 'item intv = 
    All
  | From of 'item
  | To   of 'item
  | FromTo of 'item * 'item

val subset  : 'item set * 'item intv -> 'item set
val sublist : 'item set * 'item intv -> 'item list

end
