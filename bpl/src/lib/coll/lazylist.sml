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

structure LazyList :> LAZYLIST =
struct
  exception EmptyList

  datatype 'a lazycell = Nil | Cons of 'a * 'a lazylist
  and 'a lazylist = Thunk of unit -> 'a lazycell

  val lzNil = Thunk (fn () => Nil)
  
  fun lzCons f = Thunk (fn () => Cons (f ()))

  fun lzhd (Thunk t)
    = case t () of
        Nil => raise EmptyList
      | Cons (head, _) => head
      
  fun lztl (Thunk t)
    = case t () of
        Nil => raise EmptyList
      | Cons (_, tail) => tail

  fun lzunmk (Thunk t) = t ()
        
  fun lzmap f (Thunk t)
    = Thunk (fn () =>
             case t () of
               Nil => Nil
             | Cons (elt, tail)
                => Cons (f elt, lzmap f tail))
  
  fun lztolist (Thunk t)
    = case t () of
        Nil => []
      | Cons (elt, tail) => elt :: lztolist tail
end