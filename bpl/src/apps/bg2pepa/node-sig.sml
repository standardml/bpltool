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

(** Abstract data type for modelling nodes.
 * @version $LastChangedRevision: 2717 $
 *)
signature NODE =
sig
  type node

  (** Construct a node. *)
  val make : string -> node

  (** Deconstruct a node. *)
  val unmk : node -> string

  (** Get the original string used to create a name. *)
  val ekam : node -> string

  (** Deconstruct a node. *)
  val fresh : node option -> node

  (** Test for node equality.  Any module implementing this operator
   * must satisfy <code>make {x,K} == make {x,K}</code> for any valid
   * x and K.
   *)
  val == : node * node -> bool
  (** Compare nodes in a total order. *)
  val < : node * node -> bool
  (** Compare nodes in a total order. *)
  val compare : node * node -> order

  structure Order : ORDERING where type T = node
  structure NodeSet : MONO_SET where type elt = node
end
