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

signature CONTROL =
sig
  (** The control data type. *)
  eqtype control
  (** The kind datatype. *)
  datatype kind = Active | Passive | Atomic
  (** Construct a control.
   * @params (name, kind, bound, free)
   *)
  val make : (string * kind * int * int) -> control
  (** Deconstruct a control.
   * @return (name, kind, bound, free)
   *)
  val unmk : control -> (string * kind * int * int)
  (** Return the name of a control. *)
  val name : control -> string
  (** Return the kind of a control. *)
  val kind : control -> kind
  (** Return the bound arity of a control. *)
  val bound : control -> int
  (** Return the free arity of a control. *)
  val free : control -> int
  (** Test two controls for equality.
   * @params c1 c2
   * @param c1  the first control.
   * @param c2  the second control.
   *)
  val eq : control -> control -> bool
  val kind2String : kind -> string
  val toString : control -> string
end
