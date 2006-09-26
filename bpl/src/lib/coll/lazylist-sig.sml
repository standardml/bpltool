(* Copyright (c) 2006  The BPL Group at the IT University of Copenhagen
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

(** Lazy list datatype.
 * @version $LastChangedRevision: 93 $
 *)
 
 signature LAZYLIST =
 sig
  (** A lazy list type. *)
  type 'a lazylist
  (** Lazy list cell data type. *)
  datatype 'a lazycell = Nil | Cons of 'a * 'a lazylist
  (** Lazy list nil constructor. *)
  val lzNil : 'a lazylist
  (** Lazy list cons constructor. *)
  val lzCons : (unit -> 'a * 'a lazylist) -> 'a lazylist
  (** Signal that a lazylist was unexpectedly empty. *)
  exception EmptyList
  (** Return the first element of a lazy list. 
   * @exception EmptyList if the list is empty.
   *)
  val lzhd : 'a lazylist -> 'a
  (** Return the tail of a lazy list. 
   * @exception EmptyList if the list is empty.
   *)
  val lztl : 'a lazylist -> 'a lazylist
  (** Return head and tail of a lazy list, or Nil if it is empty. *)
  val lzunmk : 'a lazylist -> 'a lazycell
  (** Map a function on all the elements of a lazy list. *)
  val lzmap : ('a -> 'b) -> 'a lazylist -> 'b lazylist
  (** Compute all the elements of a lazy list. *)
  val lztolist : 'a lazylist -> 'a list
  (** Compute a lazy list of all matches of a redex in an agent. *)
end
