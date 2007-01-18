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
  type control = string * kind
  fun make c = c
  fun unmk c = c
  fun kind (_,k) = k  
  fun strEq s1 s2 = (case String.compare (s1, s2) of
                       EQUAL => true
                     | _     => false)
  fun eq (s1, Active)  (s2, Active)  = strEq s1 s2
    | eq (s1, Passice) (s2, Passive) = strEq s1 s2
    | eq (s1, Atomic)  (s2, Atomic)  = strEq s1 s2
    | eq _            _              = false
end

structure Control :> CONTROL =
struct
  open Control'
end
