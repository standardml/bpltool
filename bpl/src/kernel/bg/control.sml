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

(** Abstract data type for modelling controls.
 * @version $LastChangedRevision$
 *)

structure Control' : CONTROL =
struct
  datatype kind = Active | Passive | Atomic
  type control = string * kind * int * int
  fun make c = c
  fun unmk c = c
  fun name  (n, _, _, _) = n  
  fun kind  (_, k, _, _) = k  
  fun bound (_, _, b, _) = b  
  fun free  (_, _, _, f) = f  
  fun strEq s1 s2 = (case String.compare (s1, s2) of
                       EQUAL => true
                     | _     => false)
  fun kindEq Active  Active  = true
    | kindEq Passive Passive = true
    | kindEq Atomic  Atomic  = true
    | kindEq _       _       = false
  fun eq (n1, k1, b1, f1)  (n2, k2, b2, f2) =
    strEq n1 n2 andalso kindEq k1 k2 andalso
    b1 = b2 andalso f1 = f2
  fun compare (c1, c2) =
    let
      val (name1, kind1, bound1, free1) = unmk c1
      val (name2, kind2, bound2, free2) = unmk c2
    in
      case String.compare (name1, name2) of
        EQUAL =>
      (case Int.compare (bound1, bound2) of
        EQUAL =>
      (case Int.compare (free1, free2) of
        EQUAL =>
      (case (kind1, kind2) of
        (Active,  Passive) => LESS
      | (Passive, Active)  => GREATER
      | (Active,  Atomic)  => LESS
      | (Atomic,  Active)  => GREATER
      | (Passive, Atomic)  => LESS
      | (Atomic,  Passive) => GREATER
      | _ => EQUAL)
      | x => x)
      | x => x)
      | x => x
    end

  fun kind2String Active  = "Active"
    | kind2String Passive = "Passive"
    | kind2String Atomic  = "Atomic"
  fun toString (n, k, b, f) = n ^ " : " ^ kind2String k ^ "(" ^
    (if b > 0 then Int.toString b ^ " -> " else "") ^
    Int.toString f ^ ")"
end

structure Control :> CONTROL =
struct
  open Control'
end
