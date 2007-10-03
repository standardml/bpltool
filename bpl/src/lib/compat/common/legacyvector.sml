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

(** Vector structure matching the VECTOR signature from
 * the new SML Basis Library, but based on the old
 * SML Basis Library Vector structure.
 * @version $LastChangedRevision: 442 $
 *)

structure Vector1996 = Vector

structure Vector :> VECTOR where type 'a vector = 'a Vector1996.vector =
struct
  open Vector
  fun update (v, i, x) =
    tabulate (length v, fn j => if i = j then x else sub (v, j))
  local
    fun mapi' f v = mapi f (v, 0, NONE)
    fun appi' f v = appi f (v, 0, NONE)
    fun foldli' f init v = foldli f init (v, 0, NONE)
    fun foldri' f init v = foldri f init (v, 0, NONE)
  in
    val mapi = mapi'
    val appi = appi'
    val foldli = foldli'
    val foldri = foldri'
  end
  fun findi f v =
    let
      fun findi' i 0 = NONE
        | findi' i n =
          let
            val ie = (i, sub (v, i))
          in
            if f ie then SOME ie else findi' (i + 1) (n - 1)
          end
    in
      findi' 0 (length v) 
    end
  fun find f v =
    case findi (f o #2) v of NONE => NONE | SOME (_, e) => SOME e
  fun exists f v = case find f v of NONE => false | SOME _ => true
  fun all f v = not (exists (not o f) v)
  fun collate f (v1, v2) =
    let
      fun c _ 0 0 = EQUAL
        | c _ 0 _ = LESS
        | c _ _ 0 = GREATER
        | c i n1 n2 =
          case f (sub (v1, i), sub (v2, i)) of
            EQUAL => c (i + 1) (n1 - 1) (n2 - 1)
          | order => order
    in
      c 0 (length v1) (length v2)
    end
end