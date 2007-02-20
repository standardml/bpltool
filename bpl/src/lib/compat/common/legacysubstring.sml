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

(** Substring structure matching the SUBSTRING signature from
 * the new SML Basis Library, but based on the old
 * SML Basis Library Substring structure.
 * @version $LastChangedRevision$
 *)

structure Substring :> SUBSTRING 
  where type string = String.string
    and type char = Char.char
= struct

    open Substring

    type char = Char.char
    type string = String.string

    val full = all

    fun concatWith s subs = raise Fail "Substring.concatWith not implemented"
    fun isSubstring "" sub = true
      | isSubstring s sub = not (isEmpty (#2 (position s sub)))
    fun isSuffix "" sub    = true
      | isSuffix s sub = s = string (#2 (position s sub))

end (* structure Substring *)
