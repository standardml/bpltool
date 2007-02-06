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
  (** Reaction rule *)
  type rule

  structure Info   : INFO
  structure BgTerm : BGTERM
  structure BgVal  : BGVAL
  structure BgBDNF : BGBDNF
  structure Match  : MATCH
  structure Instantiation : INSTANTIATION
  structure Rule          : RULE

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

  structure ErrorHandler : ERRORHANDLER


  sharing type BgBDNF.M =
               M

  sharing type BgBDNF.S =
               S

  sharing type BgBDNF.G =
               G

  sharing type BgBDNF.N =
               N

  sharing type BgBDNF.P =
               P

  sharing type BgBDNF.D =
               D

  sharing type BgBDNF.B =
               B

  sharing type BgBDNF.DR =
               DR

  sharing type BgBDNF.BR =
               BR =
               Match.BR

  sharing type BgBDNF.bgbdnf =
               bgbdnf =
               Match.bgbdnf

  sharing type Info.info =
               BgTerm.info =
               BgVal.info =
               BgBDNF.info =
               Match.info

  sharing type bgterm =
               BgTerm.bgterm =
               BgVal.bgterm

  sharing type bgval =
               BgVal.bgval =
               BgBDNF.bgval =
               Sugar.bgval

  sharing type rule =
               Rule.rule =
               Match.rule

  sharing type Interface.interface =
               BgVal.interface

  sharing type Control.control =
               Ion.control

  sharing type Ion.ion =
               BgTerm.ion =
               BgVal.ion

  sharing type Permutation.permutation =
               BgTerm.permutation =
               BgVal.permutation

  sharing type Permutation.Immutable =
               BgTerm.Immutable

  sharing type Wiring.wiring =
               BgVal.wiring

  sharing type Name.name =
               Link.name =
               Ion.name =
               NameSet.elt

  sharing type NameSet.Set =
               Name.NameSet.Set =
               Permutation.nameset =
               Ion.nameset =
               Interface.nameset =
               Link.nameset =
               Wiring.nameset =
               BgTerm.nameset =
               BgVal.nameset

  sharing type Wiring.wiring =
               BgTerm.wiring

  sharing type LinkSet.Set =
               Wiring.linkset

  sharing type Link.link =
               LinkSet.elt
  (** Maximum revision number for containing components.*)
  val revision : string
end
