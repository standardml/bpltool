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

(** Abstract data type for modelling wirings.
 * @version $Revision: 1.6 $
 *)
signature WIRING =
sig
  type link
  type linkset
  type nameset
  type ppstream

  (** The wiring data type. *)
  type wiring
  (** Construct a wiring from a link set.  Inner names must be
   * disjoint, outer names need not be. *)
  val make : linkset -> wiring
  (** Construct a wiring from a list of links.  Inner names must be
   * disjoint, outer names need not be.
   *)
  val make' : link list -> wiring
  (** Deconstruct a wiring. *)
  val unmk : wiring -> linkset

  (** Return the set of inner names of the wiring. *)
  val innernames : wiring -> nameset
  (** Return the set of outer names of the wiring. *)
  val outernames : wiring -> nameset

  (** Compose two wirings.
   * The inner face of w1 need not match the outer face of w2, in
   * which case inner names not mapped by w2 to the domain
   * of w1 will be closed by w, and names mapped by w1 that are not
   * present in the image of w2 will be names introduced by w.
   * @params w1 w2
   *)
  val o : (wiring * wiring) -> wiring
  (** Make the tensor product of two wirings.  Note that disjointness
   * of interfaces is NOT checked; the result in this case will be
   * undefined. 
   *)
  val * : (wiring * wiring) -> wiring
  (** Make the tensor product of a list of wirings.  Note that
   * disjointness of interfaces is NOT checked; the result in this case
   * will be undefined. 
   *)
  val ** : wiring list -> wiring
  (** Make the parallel product of two wirings.  Note that disjointness
   * of inner faces is NOT checked; the result in this case will be
   * undefined. 
   *)
  val || : (wiring * wiring) -> wiring

  (** Compute the set of names to which the wiring maps a given set. *)
  val app : wiring -> nameset -> nameset
  (** Compute the set of names which the wiring maps to a given set. *)
  val app_inverse : wiring -> nameset -> nameset
  (** Restrict a wiring to only map a given set of names.
   * The outer face is trimmed to include only names to which some
   * inner name maps.  Each name in the set not mapped by the wiring
   * will result in a single, closed link.
   *)
  val restrict : wiring -> nameset -> wiring
  (** Restrict a wiring to only map to a given set of names.
   * The inner face is trimmed to include only names which maps to an
   * outer name.
   *)
  val restrict_outer : wiring -> nameset -> wiring
  (** Determine whether some wiring is an identity. *)
  val is_id : wiring -> bool
  (** Determine whether some wiring is a zero identity. *)
  val is_id0 : wiring -> bool
  (** Construct an identity wiring on a set of names. *)
  val id_X : nameset -> wiring
  (** Construct an empty identity wiring. *)
  val id_0 : wiring
  (** Construct a wiring that introduces names. *)
  val introduce : nameset -> wiring
  (** Construct a wiring that closes names without linking them together. *)
  val close : nameset -> wiring
  (** Prettyprint a wiring.
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param w       The wiring to output.
   *)
  val pp : int -> ppstream -> wiring -> unit
end
