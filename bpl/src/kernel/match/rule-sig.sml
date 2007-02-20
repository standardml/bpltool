(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** Abstract data type for modelling rules.
 * @version $LastChangedRevision$
 *)
signature RULE =
sig
  type rule
  type bgval
  type 'a bgbdnf
  type BR
  type inst
  (** Construct a rule.  The instantiation must be compatible
   * with redex and reactum inner faces, i.e., instantiate the
   * inner face of reactum from the inner face of redex.
   * @params {redex, react, inst}
   * @param redex  Redex bigraph
   * @param react  Reactum bigraph
   * @param inst   Instantiation
   *)
  val make : {name : string, redex : BR bgbdnf, react : bgval, inst : inst} -> rule
  (** Construct a rule.
   * The instantiation will be inferred from redex and reactum.
   * @params {redex, react}
   * @param redex  Redex bigraph
   * @param react  Reactum bigraph
   *)
  val make' : {name : string, redex : BR bgbdnf, react : bgval} -> rule
  (** Deconstruct a rule. @see make. *)
  val unmk : rule -> {name : string, redex : BR bgbdnf, react : bgval, inst : inst}
  (** Prettyprint a rule.
   * @params indent pps r
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param r       The rule to output.
   *)
  val pp : int -> PrettyPrint.ppstream -> rule -> unit
  (** Return a prettyprinted string representation of a rule. *)
  val toString : rule -> string
end