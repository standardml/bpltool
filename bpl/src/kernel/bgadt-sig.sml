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

(** BG Abstract Data Types module.  This module contains just the abstract
 * data types of the BPL kernel, ML syntactical sugaring and error handling.
 * For parsing and I/O functionality, see the BG module.
 * @version $LastChangedRevision$
 *)
 
signature BG_ADT =
sig
  type info
  type ppstream

  (** Page width used by bdnfToString. *)
  val pageWidth : int ref
  (** Block indentation used by bdnfToString. *)
  val indent : int ref

  (** Bigraph terms (possibly not well-formed). *)
  type bgterm
  (** Bigraph values (well-formed, possibly not BDNF). *)
  type bgval
  (** M BDNF molecule class phantom type. *)
  type M 
  (** S BDNF singular top-level node class phantom type. *)
  type S
  (** G BDNF global discrete prime class phantom type. *)
  type G
  (** N BDNF name-discrete prime class phantom type. *)
  type N
  (** P BDNF discrete prime class phantom type. *)
  type P
  (** D BDNF discrete bigraph class phantom type. *)
  type D
  (** B BDNF general bigraph class phantom type. *)
  type B
  (** DR BDNF discrete, regular bigraph class phantom type. *)
  type DR
  (** BR BDNF general, regular bigraph class phantom type. *)
  type BR
  (** Bigraphs on {M,S,G,N,P,D,DR,B,BR} BDNF form. *)
  type 'class bgbdnf

  structure BgTerm : BGTERM
  structure BgVal  : BGVAL
  structure BgBDNF : BGBDNF
  structure Match  : MATCH

  structure Interface   : INTERFACE
  structure Ion         : ION
  structure Permutation : PERMUTATION
  structure Wiring      : WIRING

  structure Control : CONTROL
  structure NameSet : MONO_SET
  structure LinkSet : MONO_SET

  structure Name : NAME
  structure Link : LINK

  structure Sugar : SUGAR
  structure BGErrorHandler : BGERRORHANDLER

end
