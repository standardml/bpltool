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

(** Datatype for representing instantiations.
 * @version $LastChangedRevision: 397 $
 *)

signature INSTANTIATION =
sig
  type bgval
  type name
  type 'a bgbdnf
  type DR
  (** Instantiation type. *)
  type inst
  (** Construct an instantitation.   For instance,
     [1&[x1,x2] |--> 0&[y1,y2], ...]
   * make' [(1,0,[(x1,y1),(x2,y2)])] will let root 1 of the instance
   * be a copy of root 0 of the original, where name y1 is renamed to x1,
   * name y2 renamed to x2, and all other variables and roots will be
   * copies of the corresponding entities of the original.
   *)
  val make' : (int * int * (name * name) list) list -> inst
  (** The identity instantiation. *)
  val id : inst
  (** Use an instantiation to instantiate a bgval. *)
  val instantiate : inst -> DR bgbdnf -> bgval
  (** Prettyprint an instantiation.
   * @params indent pps inst
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param inst    The instantiation to print.
   *)
  val pp : int -> PrettyPrint.ppstream -> inst -> unit
  (** Return a prettyprinted string representation of a instantiation. *)
  val toString : inst -> string
  
end