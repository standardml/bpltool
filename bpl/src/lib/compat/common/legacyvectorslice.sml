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

 (** VectorSlice structure matching the VECTOR_SLICE signature from
 * the new SML Basis Library, but based on the old
 * SML Basis Library Vector structure.
 * @version $LastChangedRevision: 442 $
 *)
 
 structure VectorSlice :> VECTOR_SLICE =
 struct
   type 'a slice = 'a vector * int * int
   fun length (_, _, n) = n
   fun sub ((v, i, n), j) =
     if j < 0 orelse n <= j then
       raise Subscript
     else
       Vector.sub (v, i + j) 
   fun full v = (v, 0, Vector.length v)
   fun slice (v, i, sz) =
     if i < 0 then
       raise Subscript
     else
       let
         val l = Vector.length v
       in
         case sz of
           NONE => if l < i then raise Subscript else (v, i, l - i)
         | SOME j =>
           if j < 0 orelse l < i + j then raise Subscript else (v, i, j)
       end
   fun subslice ((v, i1, n), i2, sz) =
     if i2 < 0 then
       raise Subscript
     else
       case sz of
         NONE => if n < i2 then raise Subscript else (v, i1 + i2, n - i2)
       | SOME j =>
         if j < 0 orelse n < i2 + j then raise Subscript else (v, i1 + i2, j) 
   fun base sl = sl
   fun vector (v, i, n) = Vector1996.extract (v, i, SOME n)
   fun concat l = Vector.concat (map vector l)
   fun isEmpty (_, _, n) = n = 0
   fun getItem (v, i, 0) = NONE
     | getItem (v, i, n) = SOME (Vector.sub (v, i), (v, i + 1, n - 1))
   fun appi f (v, i, n) = Vector1996.appi f (v, i, SOME n)
   fun app f = appi (f o #2)
   fun mapi f (v, i, n) = Vector1996.mapi f (v, i, SOME n)
   fun map f = mapi (f o #2)
   fun foldli f init (v, i, n) = Vector1996.foldli f init (v, i, SOME n) 
   fun foldri f init (v, i, n) = Vector1996.foldri f init (v, i, SOME n) 
   fun foldl f = foldli (fn (_, a, b) => f (a, b)) 
   fun foldr f = foldri (fn (_, a, b) => f (a, b)) 
   fun findi f (v, i, n) =
     let
       fun findi' j 0 = NONE
         | findi' j n =
           let
             val e = Vector.sub (v, j)
           in
             if f (j - i, e) then SOME (j - i, e) else findi' (j + 1) (n - 1)
           end
     in
       findi' i n
     end
   fun find f sl = case findi (f o #2) sl of NONE => NONE | SOME (_, e) => SOME e
   fun exists f sl = case find f sl of NONE => false | SOME _ => true
   fun all f sl = not (exists (not o f) sl)
   fun collate f ((v1, i1, 0), (v2, i2, 0)) = EQUAL
     | collate f ((v1, i1, 0), (v2, i2, _)) = LESS
     | collate f ((v1, i1, _), (v2, i2, 0)) = GREATER
     | collate f ((v1, i1, n1), (v2, i2, n2)) =
       case f (Vector.sub (v1, i1), Vector.sub (v2, i2)) of
         EQUAL => collate f ((v1, i1 + 1, n1 - 1), (v2, i2 + 1, n2 - 1))
       | order => order
end