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

(** Abstract data type for modelling ions.
 * @version $LastChangedRevision$
 *)

signature ION =
sig
  eqtype control
  type name
  type nameset

  (** The ion data type. *)
  type ion
  (** Construct an ion.
   * @params {ctrl, free, bound}
   * @param ctrl   the control of the ion.
   * @param free   outer names to which the ion's free ports are
   *               linked.
   * @param bound  sets of inner names to which each of the ion's bound
   *               ports are linked.
   *)
  val make : {ctrl : control, free : name list, bound : nameset list}
	     -> ion
  (** Deconstruct an ion.
   * @see make.
   *)
  val unmk : ion -> {ctrl : control, free : name list, bound : nameset list}
  (** Test two ions for equality.
   * @params i1 i2
   * @param i1  the first ion.
   * @param i2  the second ion.
   *)
  val eq : ion -> ion -> bool
  (** Return the inner names of the ion. *)
  val innernames : ion -> nameset
  (** Return the outer names of the ion. *)
  val outernames : ion -> nameset
  (** Prettyprint an ion.
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param KyX     The ion to output.
   *)
  val pp : int -> PrettyPrint.ppstream -> ion -> unit
  val oldpp : int -> PrettyPrint.ppstream -> ion -> unit
end
