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

(** Abstract data type for modelling names.
 * @version $LastChangedRevision$
 *)
signature NAME =
sig
  (** The link data type. *)
  eqtype name
  (** Construct a name. *)
  val make : string -> name
  (** Get the original string used to create a name. *)
  val ekam : name -> string
  (** Construct a fresh name possibly based on another name. *)
  val fresh : name option -> name
  (** Deconstruct a name. *)
  val unmk : name -> string
  (** Calculate a hash for the given name. *)
  val hash : name -> word
 
  (** Test for name equality.  Any module implementing this operator
   * must satisfy <code>make x == make x</code> for any valid x.
   *)
  val == : name * name -> bool
  (** Compare names in a total order. *)
  val < : name * name -> bool
  (** Compare names in a total order. *)
  val compare : name * name -> order

  structure Order : ORDERING where type T = name
  structure NameSet : MONO_SET where type elt = name
end
