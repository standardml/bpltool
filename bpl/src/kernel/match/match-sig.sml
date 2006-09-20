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
  type 'class bgrbdnf
  type B
  type D
  type ppstream
  
  
  (** A match type. *)
  type match
  (** An inference tree type. *)
  type inference
  (** Deconstruct a match. *)
  val unmk : match -> {context : B bgrbdnf,
                       redex : B bgrbdnf,
                       parameter : D bgrbdnf}
  (** Inference value indicating no inference (yet). *)
  val noinference : inference
  (** Infer the next match of a redex in an agent, following a given inference.
   * @params {agent, redex} lastinf
   * @param agent      the bigraph agent in which a match is searched.
   * @param redex      the redex to match.
   * @param lastinf    the last inference tree found.
   * @return SOME {match, inf} if a new match is found, and
   *         its inference tree.
   *)
  val nextmatch : {agent : B bgrbdnf, redex : B bgrbdnf}
                -> inference
                -> {match : match, inf : inference} option
  (** Infer a match of a redex in an agent.
   * @params {agent, redex}
   * @param agent      the bigraph agent in which a match is searched.
   * @param redex      the redex to match.
   * @return SOME match if a match is found, NONE otherwise.
   *)
  val amatch : {agent : B bgrbdnf, redex : B bgrbdnf} -> match option
  (** Infer all matches of a redex in an agent.
   * @params {agent, redex}
   * @param agent   the bigraph agent in which a match is searched.
   * @param redex   the redex to match.
   * @return a list of matches, empty if redex does not match.
   *)
  val allmatches : {agent : B bgrbdnf, redex : B bgrbdnf} -> match list
  (** Prettyprint a match
   * @params indent pps m
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param m       The match to print.
   *)
  val pp : int -> ppstream -> match -> unit
  (** Prettyprint an inference tree
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The inference tree to print.
   *)
  val pp_inference : int -> ppstream -> inference -> unit
end
