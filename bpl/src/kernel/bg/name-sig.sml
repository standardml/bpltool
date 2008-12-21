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
  (** The name data type. *)
  type name
  (** Construct a name. *)
  val make : string -> name
  (** Get the original string used to create a name. *)
  val ekam : name -> string
  (** Construct a fresh name possibly based on another name. *)
  val fresh : name option -> name
  (** Deconstruct a name.  The returned string may include an internal id. *)
  val unmk : name -> string
  (** Calculate a hash for the given name. *)
  val hash : name -> word
  (** Reset the internal representation.  NOTE: Reusing names created
   * before a call to reset (or bigraphs containing these old names)
   * can produce incorrect results!
   *)
  val reset : unit -> unit
 
  (** Test for name equality.  Any module implementing this operator
   * must satisfy <code>make x == make x</code> for any valid x.
   *)
  val == : name * name -> bool
  (** Compare names in a total order. *)
  val < : name * name -> bool
  (** Compare names in a total order. *)
  val compare : name * name -> order

  (** Prettyprint a name. *)
  val pp : int -> PrettyPrint.ppstream -> name -> unit

  (** Return a prettyprinted string representation of a name. *)
  val toString : name -> string


  structure Order : ORDERING where type T = name
  structure NameSet : MONO_SET where type elt = name

  (** Signals that the two names will clash, if they are printed as
   * the string given to <code>make</code>.
   * @params n1 n2
   * @param  n1 the first name.
   * @param  n2 the second name.
   *)
  exception PPUnchangedNameClash of name * name

  (** Tell the name module to print the given sets of
   * names as the string given to <code>make</code>.
   * Normal pretty printing (i.e. no names are treated specially) is set
   * by calling <code>pp_unchanged NameSet.empty</code>.
   *
   * @params X
   * @param X   names
   * @exception PPUnchangedNameClash  if two names in X will clash if
   *                                  printed unchanged. In that case,
   *                                  the pretty printing will be as if
   *                                  <code>pp_unchanged NameSet.empty</code>
   *                                  had been called.
   *)
  val pp_unchanged : NameSet.Set -> unit
  (** Add a set  of names from the set of names that should be printed
   * unchanged. If a name x in X would clash with another name that is
   * already being printed unchanged, then x is silently ignored.
   *
   * @params X
   * @param X   names
   *)
  val pp_unchanged_add    : NameSet.Set -> unit
  (** Remove a set of names from the set of names that should be printed
   * unchanged. This doesn't affect the names given to the most recent
   * invocation of <code>pp_unchanged</code> - these are silently ignored.
   *
   * @params X
   * @param X   names
   *)
  val pp_unchanged_remove : NameSet.Set -> unit
end
