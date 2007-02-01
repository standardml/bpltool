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
  type 'a bgbdnf
  type DR
  (** Instantiation type. *)
  type inst
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