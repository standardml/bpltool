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

(** Abstract data type for modelling bigraph interfaces. 
 * @version $LastChangedRevision$
 *)
signature INTERFACE =
sig
  type nameset

  (** The interface data type. *)
  type interface
  (** Construct an interface.  Note that local names must not be
   * included in the glob argument, and that the width is given by the
   * length of the loc argument.
   * @params {width, loc, glob}
   * @param loc    list of sets of local names for each location.
   * @param glob   set of global names. 
   *)
  val make : {loc : nameset list, glob : nameset}
	     -> interface
  (** Construct an interface with no local names.
   * @params {width, glob}
   * @param width  interface width
   * @param glob   set of global names. 
   *)
  val make' : {width : int, glob : nameset} -> interface
  (** Deconstruct an interface.
   * @see make.
   *)
  val unmk
      : interface -> {width : int, loc : nameset list, glob : nameset}
  (** Return the width of an interface. *)
  val width : interface -> int
  (** Return the list of local name sets. *)
  val loc : interface -> nameset list
  (** Return the set of global names. *)
  val glob : interface -> nameset
  (** Return the set of names. *)
  val names : interface -> nameset

  (** Determine interface equality.  Two interfaces are considered
   * equal if they have identical width, their global name sets are
   * identical, and their local sets are pairwise identical.
   *)
  val eq : (interface * interface) -> bool

  (** Determine whether the interface is local.
   *)
  val is_local : interface -> bool

  (** Construct the tensor product of two interfaces. *)
  val * : (interface * interface) -> interface
  (** Empty interface. *)
  val zero : interface
  (** Nameless interface of width 1. *)
  val one : interface
  (** interface of width m, *)
  val m : int -> interface
  (** Global name interface of width 0. *)
  val X : nameset -> interface
  (** Prettyprint a given interface.
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param I       The interface to output.
   *)
  val pp : int -> PrettyPrint.ppstream -> interface -> unit
  (** Return a prettyprinted string representation of an interface. *)
  val toString : interface -> string

  (** Abstraction on interface 
   * Raises an error if abstraction is not welldefined.
   * 
   * @param X   Names to be abstracted.
   * @param I   Interface to be abstracted.
   * @return I' Interface I' = (X)I --- i.e. I with names X localized.
   *)
  val abs : nameset -> interface -> interface

  (** Signals an attempted abstraction of a nonprime interface.
   * @param interface      The nonprime interface.
   *)
  exception AbsOfNonPrimeIface of interface

  (** Signals an attempted abstraction an interface of names 
   * already local in that interface.
   * @param interface      An interface.
   * @param X              A set of names.
   *)
  exception AbsLocalNameClash of interface * nameset

  (** Signals an attempted abstraction an interface of names
   * not global in the interface.
   * @param interface      An interface.
   * @param X              A set of names.
   *)
   exception AbsMissingGlobalNames of interface * nameset

  (** Construct the parallel product of two interfaces. *)
  val || : (interface * interface) -> interface

  (** Construct the parallel product of a list of interfaces. *)
  val ||| : interface list -> interface
end
