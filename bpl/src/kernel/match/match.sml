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
 
functor Match (
  structure Permutation : PERMUTATION
  structure BgBDNF : BGBDNF
  structure PrettyPrint : PRETTYPRINT
  sharing type BgBDNF.ppstream = PrettyPrint.ppstream
                               = Permutation.ppstream) :> MATCH 
  where type 'class bgrbdnf  = 'class BgBDNF.bgrbdnf
    and type B        = BgBDNF.B
    and type D        = BgBDNF.D
    and type ppstream = PrettyPrint.ppstream =
struct
  type 'class bgrbdnf  = 'class BgBDNF.bgrbdnf
  type B        = BgBDNF.B
  type D        = BgBDNF.D
  type ppstream = PrettyPrint.ppstream
  type permutation = Permutation.permutation
  
  type match = {context : B bgrbdnf,
                redex : B bgrbdnf,
                parameter : D bgrbdnf}

	(* Inference trees include nondeterministic choices of
	 * parenthesisation and permutation.
	 * Arguments parls and merls are lists of non-negative integers
	 * representing the parenthisation of tensor products in PAR and
	 * MER rules.  The numbers indicate the number of factors in each
	 * parenthesised factor.
	 * pi is the permutation introduced by the MER rule.
	 *)
  datatype Dg
    = SWX of D'g       (* Switch rule *)
    | PAX              (* Prime axiom *)
    | MER of Ds list * {parls : int list, merls : int list, pi : permutation}
                       (* Merge rule *)
  and D'g
    = PAX'             (* Prime axiom *)
    | MER' of D's list * {parls : int list, merls : int list, pi : permutation}
                       (* Merge rule *)
  and Ds
    = SPM of Dg        (* Switch, Prime axiom or Merge *)
  	| ION of Dg        (* Ion rule *)
  and D's
    = PAM' of D'g      (* Prime axiom or Merge *)
    | ION' of D'g      (* Ion rule *)
  type inference = (Dg list * int list) option  (* DNF rule *)

  val noinference = NONE

  fun unmk m = m

  fun nextmatch {agent, redex} inf = NONE (* = not yet implemented! *)

    (* MAIN MATCHING ALGORITHM HERE! *)


  fun amatch agentredex = nextmatch agentredex noinference

  fun allmatches agentredex = 
	  let
	    val nextmatch_aR = nextmatch agentredex
		  fun iterateall i = case nextmatch_aR i of
  	  									   SOME {match = m, inf = i} => m :: iterateall i
    		                 | NONE => []
    in
			iterateall noinference
		end
  
  fun pp indent pps m = ()
  
  fun pp_inference indent pps t = ()
end
