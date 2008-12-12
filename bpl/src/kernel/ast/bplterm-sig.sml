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

(** 
 * Abstract data type for an alternative variant of bigraph terms.
 * The aim is to be able to express location- and context models in BPL
 * and then programatically get them transformed into input for the BPL
 * tool -- e.g. a BRS. Work in progess.
 *
 * <p>
 * The data type does not prevent the representation of non-bigraphs,
 * e.g. prime products of bigraphs with local inner names, products
 * of bigraphs with inner name clashes, compositions of bigraphs with
 * incompatible interfaces, etc.
 * </p>
 *
 * TODO: UPDATE THIS OUT-OF-DATE-DOCUMENTATION:
 *
 * The terms are not necessarily well-formed:
 * scope rule may be violated in abstractions, names may clash in
 * interfaces of tensor product operands, bigraph widths may be
 * incompatible in compositions, etc.
 * <p>
 * Each constructor takes an info argument that can contain contextual
 * information (e.g., source file location for the term).
 *
 * @author Henning Niss, Ebbe Elsborg et al.
 * @version $LastChangedRevision$
 *)
signature BPLTERM = sig
	(** General identifier type. *)
	type id = string
  (** Site identifier type.  A site is either indexed by a natural
   * number, or identified by name.
   *)
  datatype siteid = SiteNum of int | SiteName of id

  (** Wire type. *)
  datatype wire
    (** Global renaming y/x renaming inner name x to outer name y. *)
  = GRen of id * id
    (** Global name introduction y. *)
  | GInt of id
		(** Local renaming (y)/(x) renaming local inner name x to 
		 * local outer name y.
		 *)
	| LRen of id * id
		(** Local name introduction (y)(y * merge(0)). *)
	| LInt of id

	(** The BPL bigraph term data type. *)	 
  datatype bigraph
  (** A wiring. *)  
  = Wir of wire list
  (** A parallel product b1 || b2 of bigraphs. *)
  | Par of bigraph * bigraph
  (** A prime product b1 | b2 of bigraphs. *)
  | Pri of bigraph * bigraph
  (** A composition b1 o b2 of bigraphs. *)
  | Com of bigraph * bigraph
  (** A tensor product b1 * b2 of bigraphs. *)
  | Ten of bigraph * bigraph
  (** An ion K[y1,...,ym][[x1], ..., [xn]].
   * @params K ys xs
   *)
  | Ion of id * id list * id list
  (** A closure /X o b of a bigraph. *)
  | Clo of id list * bigraph
  (** An abstraction (X)b of some of the outer names of a bigraph. *)
  | Abs of id list * bigraph
  (** A concretion [X]b of some of the outer names of a bigraph. *)
  | Con of id list * bigraph 
  (** A site (X)[id]<x1, ..., xn>, is an identity bigraphs
   * where id is an integer index or an identifying name,
   * and X is the set of local names x1, ..., xn.
   *)
  | Sit of siteid * id list
  (** Lookup a bigraph identified by name. *)
  | Ref of id
  (** A barren root. Note that the empty bigraph id_0 can be
   * constructed using Wir[]. *)
  | Bar

  (** Control kind type. *)
  eqtype kind
  val Active  : kind
  val Passive : kind
  val Atomic  : kind

  (** Signature type. For each control, we record
   * (name, kind, bound arity, free arity).
   *)
  type sign = (id * kind * int * int) list
  (** Named signature type. *)
  type sigdef = id * sign

  (** Declaration type. *)
  datatype dec
  (** Named reaction rule definition:  name ::: redex ----|> reactum. *)
  = Rul of id * (bigraph * bigraph)
  (** Named bigraph definition. *)
	| Val of id * bigraph
  
  (** Bigraphical program type.  A program is a list of named
   * signatures, an identifier specifying which to use, and
   * a list of declarations.
   *)
  type prog = sigdef list * id * dec list

  (** Prettyprint a bigraph. *)
  val ppBigraph : bigraph Pretty.pp
  (** Prettyprint a program. *)
  val pp        :    prog Pretty.pp
end (* signature BPL_TERM *)
