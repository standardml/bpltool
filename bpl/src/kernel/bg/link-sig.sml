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

(** Abstract data type for modelling individual links.
 * @version $LastChangedRevision$
 *)
signature LINK =
sig
  type name
  type nameset

  (** The link data type. *)
  type link
  (** Construct a link.
   * @params {outer = y, inner = X}
   * @param y  outer name to which inner names are linked.  If not
   *           present, the link is closed.
   * @param X  inner names, all linked to the outer name.  If no outer
   *           name is present, each inner name is closed individually
   *           (i.e., not linked to a common edge).
   *)
  val make : {outer : name option, inner : nameset} -> link
  (** Deconstruct a link. *)
  val unmk : link -> {outer : name option, inner : nameset}
  (** Return the inner names of a link. *)
  val innernames : link -> nameset
  (** Return the outer name of a link (if it has one). *)
  val outername : link -> name option

  structure Order : ORDERING where type T = link
end
