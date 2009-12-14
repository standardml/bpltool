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

(** Abstract data type for modelling Precondition-Change-Rules (PC-rules
 * or simply PCRs).
 * @version $LastChangedRevision: 2717 $
 *)
signature PCRULE =
sig
  type aspect
  type value
  type preconds
  type changes
  type 'a entitymap
  type 'a aspectmap
  type nodeset
  type edgeset
  type controlset
  type conbg
  type reaction_rule
  type translation
  type instantiation

  (** The Precondition-Change-Rule data type. *)
  type pcrule

  (** Construct a PC-rule. *)
  val make : {name : string, preconds : preconds, changes : changes}
             -> pcrule

  (** Deconstruct a PC-rule. *)
  val unmk : pcrule
             -> {name : string, preconds : preconds, changes : changes}

  (** Deconstruct a PC-rule. *)
  val unmk' : pcrule
              -> {name : string, redex : conbg, changes : changes}

  (** Deconstruct a PC-rule. *)
  val unmk'' : pcrule
               -> {name : string, table : (value * value) aspectmap}

  (** Return the name of a rule. *)
  val name : pcrule -> string

  (** Return the redex of a rule. *)
  val redex : pcrule -> conbg

  (** Convert a PC-rule to an ordinary bigraphical reaction rule. *)
  val reaction_rule : pcrule -> reaction_rule

  (** FIXME *)
  val support : pcrule -> {nodes : nodeset, edges : edgeset}

  (** FIXME *)
  val width : pcrule -> int

  (** FIXME *)
  val controls : pcrule -> controlset

  (** Translate the support according the given translation. *)
  val translate : translation -> pcrule -> pcrule

  (** Instantiate according to a translation of its support and
   * a mapping of its roots and names to roots/nodes and names/edges
   * respectively.
   * NB: this is _not_ the same as instantiation of parameters. *)
  val instantiate : instantiation -> pcrule
                    -> (value * value) aspectmap * int entitymap
end
