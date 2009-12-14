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

(** Structure providing comparison of maps.
 * @version $LastChangedRevision: 442 $
 *)
functor MapCompare (
  structure Map : MONO_FINMAP
  structure DomOrder : ORDERING
  sharing type Map.dom = DomOrder.T) : MAPCOMPARE =
struct
  type 'a T = 'a Map.map

  fun List_collate R xs ys =
    let
      fun collate [] [] = EQUAL
        | collate (x :: xs) [] = GREATER
        | collate [] (y :: ys) = LESS
        | collate (x :: xs) (y :: ys) =
          case R (x, y) of
            EQUAL => collate xs ys
          | result => result
    in
      collate xs ys
    end
    
  fun compare rng_lt map1 map2 =
    let
      val x1s = Map.list map1
      val x2s = Map.list map2
      val size1 = length x1s
      val size2 = length x2s
    in
      if size1 < size2 then
        LESS
      else if size1 > size2 then
        GREATER
      else
        List_collate (fn ((d1, r1), (d2, r2)) => 
                         if DomOrder.lt d1 d2 then
                           LESS
                         else if DomOrder.lt d2 d1 then
                           GREATER
                         else if rng_lt r1 r2 then
                           LESS
                         else if rng_lt r2 r1 then
                           GREATER
                         else
                           EQUAL)
                     x1s
                     x2s
    end
    
  fun lt rng_lt map1 map2 = (compare rng_lt map1 map2) = LESS
end
