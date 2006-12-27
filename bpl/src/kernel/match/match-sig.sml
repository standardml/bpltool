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

(** Match datatype and inference.
 * @version $LastChangedRevision$
 *)
 
signature MATCH =
sig
  type info
  type 'class bgbdnf
  type B
  type BR
  type DR
  type nameset
  type 'a lazylist
  
  (** A match type. *)
  type match

  (** Deconstruct a match. *)
  val unmk : match -> {context : B bgbdnf,
                       redex : BR bgbdnf,
                       parameter : DR bgbdnf}
                       
  (** Compute a lazy list of matches of a redex in an agent. *)
  val matches : {agent : BR bgbdnf, redex : BR bgbdnf} -> match lazylist

  (** Infer a match of a redex in an agent.
   * @params {agent, redex}
   * @param agent      the bigraph agent in which a match is searched.
   * @param redex      the redex to match.
   * @return SOME match if a match is found, NONE otherwise.
   *)
  val amatch : {agent : BR bgbdnf, redex : BR bgbdnf} -> match option

  (** Infer all matches of a redex in an agent.
   * @params {agent, redex}
   * @param agent   the bigraph agent in which a match is searched.
   * @param redex   the redex to match.
   * @return a list of matches, empty if redex does not match.
   *)
  val allmatches : {agent : BR bgbdnf, redex : BR bgbdnf} -> match list

  (** Prettyprint a match.
   * @params indent pps m
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param m       The match to print.
   *)
  val pp : int -> PrettyPrint.ppstream -> match -> unit

  (** Prettyprint a match using a specific prettyprinter for bgbdnf's.
   * @params ppBDNF indent pps m
   * @param ppBDNF  Prettyprinter to use on bgbdnf's.
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param m       The match to print.
   *)
  val pp' : (int -> PrettyPrint.ppstream -> B bgbdnf -> unit)
          ->(int -> PrettyPrint.ppstream -> DR bgbdnf -> unit)
          -> int -> PrettyPrint.ppstream -> match -> unit

  (** Prettyprint a match, including derivation tree.
   * @params indent pps m
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param m       The match to print.
   *)
  val ppWithTree : int -> PrettyPrint.ppstream -> match -> unit

  (** Return a prettyprinted string representation of a match. *)
  val toString : match -> string

  (** Return a prettyprinted string representation of a match,
   * using a specific prettyprinter for bgbdnf's.
   *)
  val toString' : (int -> PrettyPrint.ppstream -> B bgbdnf -> unit)
                ->(int -> PrettyPrint.ppstream -> DR bgbdnf -> unit)
                -> match -> string

  (** Return a prettyprinted string representation of a match,
   * including derivation tree.
   *)
  val toStringWithTree : match -> string

  (** Return a prettyprinted string representation of a match,
   * including derivation tree,
   * using a specific prettyprinter for bgbdnf's.
   *)
  val toStringWithTree' : (int -> PrettyPrint.ppstream -> B bgbdnf -> unit)
                ->(int -> PrettyPrint.ppstream -> DR bgbdnf -> unit)
                -> match -> string
  (** Revision number.*)
  val revision : string
end
