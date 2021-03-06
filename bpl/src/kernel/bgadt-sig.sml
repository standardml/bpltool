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
  (** Reaction rule. *)
  type rule
  (** Control. *)
  type control

  structure Info          : INFO
  
  structure BgTerm        : BGTERM
  structure BgVal         : BGVAL
  structure BgBDNF        : BGBDNF
  structure BPLTerm       : BPLTERM
  structure BPL2BgVal     : BPL2BGVAL
  
  structure Match         : MATCH
  structure Instantiation : INSTANTIATION
  structure Rule          : RULE
  structure Reaction      : REACTION
  where type rulename = string

  structure Interface   : INTERFACE
  structure Ion         : ION
  structure Permutation : PERMUTATION
  structure Wiring      : WIRING

  structure Control : CONTROL
  structure NameSet : MONO_SET
  structure NameMap : MONO_FINMAP
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
               Match.B =
               B

  sharing type BgBDNF.DR =
               DR =
               Match.DR =
               Instantiation.DR

  sharing type BgBDNF.BR =
               BR =
               Match.BR =
               Rule.BR =
               Reaction.BR

  sharing type BgBDNF.bgbdnf =
               bgbdnf =
               Match.bgbdnf =
               Instantiation.bgbdnf  =
               Rule.bgbdnf =
               Reaction.bgbdnf 

  sharing type Info.info =
               BgTerm.info =
               BgVal.info =
               BgBDNF.info =
               Match.info =
               Rule.info =
               ErrorHandler.origin

  sharing type bgterm =
               BgTerm.bgterm =
               BgVal.bgterm

  sharing type bgval =
               BgVal.bgval =
               BgBDNF.bgval =
               BPL2BgVal.bgval =
               Instantiation.bgval =
               Sugar.bgval =
               Rule.bgval =
               Reaction.bgval

  sharing type BPLTerm.dec =
               BPL2BgVal.dec

  sharing type rule =
               Rule.rule =
               Match.rule =
               Reaction.rule =
               Sugar.rule =
               BPL2BgVal.rule

  sharing type Control.kind =
               Sugar.ctrlkind =
               BPLTerm.kind =
               BPL2BgVal.kind

  sharing type control =
               Control.control =
               BgTerm.control =
               BgVal.control =
               BgBDNF.control =
               Ion.control

  sharing type Match.match =
               Reaction.match

  sharing type Instantiation.inst =
               Rule.inst

  sharing type Interface.interface =
               BgVal.interface =
               BgBDNF.interface =
               Instantiation.interface =
               BPL2BgVal.interface

  sharing type Ion.ion =
               BgTerm.ion =
               BgVal.ion =
               BgBDNF.ion

  sharing type Permutation.permutation =
               BgTerm.permutation =
               BgVal.permutation =
               BgBDNF.permutation

  sharing type Permutation.Immutable =
               BgTerm.Immutable

  sharing type Name.name =
               Link.name =
               Ion.name =
               Instantiation.name =
               BgVal.name =
               Wiring.name =
               NameSet.elt =
               NameMap.dom

  sharing type NameSet.Set =
               Name.NameSet.Set =
               Permutation.nameset =
               Ion.nameset =
               Interface.nameset =
               Link.nameset =
               Wiring.nameset =
               BgTerm.nameset =
               BgVal.nameset

  sharing type NameMap.map =
               Wiring.namemap

  sharing type Wiring.wiring =
               BgTerm.wiring =
               BgVal.wiring =
               BgBDNF.wiring

  sharing type LinkSet.Set =
               Wiring.linkset

  sharing type Link.link =
               LinkSet.elt =
               Wiring.link

  (** Maximum revision number for containing components.*)
  val revision : string
end
