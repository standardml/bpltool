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

(** Abstract data type for bigraph binding discrete normal forms (BDNF).
 * @version $LastChangedRevision$
 *)
signature BGBDNF =
sig
  type info
  type nameset
  type interface
  type wiring
  type 'kind permutation
  type bgval
  type bgmatch
  type bgterm
  type ppstream


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
  (** The bgbdnf data type.  'class must be M, S, G, N, P, D, or B. *)
  type 'class bgbdnf
  (** Signal that a BDNF does not represent a regular bigraph.
   * @params file i b errtxt
   * @param file    the file name in which the exception was raised.
   * @param b       the bigraph
   * @param errtxt  explanatory error text.
   *)
  exception IrregularBDNF of string * info * bgval * string
  (** Construct a B bgbdnf from a bgval. *)
  val make : bgval -> B bgbdnf
  (** Deconstruct a bgbdnf. *)
  val unmk : 'class bgbdnf -> bgval
  (** Regularize a B bgbdnf.
   * @params b
   * @param  b the BDNF to reqularize
   * @exception IrregularBDNF  if b is irregular.
   *)
  val regularize : B bgbdnf -> BR bgbdnf

  (** Sum type for singular top-level nodes. *)
  datatype stlnode =
           SCon of bgval
         | SMol of M bgbdnf

  (** Construct a B bgbdnf from a wiring, nameset list and D bgbdnf. *)
  val make_B : wiring -> nameset list -> D bgbdnf -> B bgbdnf
  (** Construct a BR bgbdnf from a wiring, nameset list and DR bgbdnf. *)
  val make_BR : wiring -> nameset list -> DR bgbdnf -> BR bgbdnf
  (** Construct a D bgbdnf from a renaming, P bgbdnf list and a permutation.
	 * @params alpha Ps pi
   * @exception  NotRenaming if alpha is not a renaming.
   *)
  val make_D : wiring -> P bgbdnf list -> 'kind permutation -> D bgbdnf
  (** Construct a DR bgbdnf from a renaming and a P bgbdnf list.
	 * @params alpha Ps
   * @exception  NotRenaming if alpha is not a renaming.
   *)
  val make_DR : wiring -> P bgbdnf list -> DR bgbdnf
  (** Deconstruct a B bgbdnf. 
   * @return (wirxid, D) representing (a wiring x id_(Xs)) and a DBDNF.
   *)
  val unmkB : B bgbdnf -> {wirxid : bgval, D : D bgbdnf}
  (** Deconstruct a D bgbdnf. 
   * @return (ren, Ps, perm) representing a renaming, a tensor product
   * of PBDNF primes, and a permutation
   *)
  val unmkD
      : D bgbdnf -> {ren : bgval, Ps : P bgbdnf list, perm : bgval}
  (** Deconstruct a P bgbdnf. 
   * @return (idxlocsub, N) representing (id_Z x a local substitution)
   * and a NBDNF.
   *)
  val unmkP : P bgbdnf -> {idxlocsub : bgval, N : N bgbdnf}
  (** Deconstruct a N bgbdnf. 
   * @return (absnames, G) representing an abstraction and a GBDNF.
   *)
  val unmkN : N bgbdnf -> {absnames : nameset, G : G bgbdnf}
  (** Deconstruct a G bgbdnf. 
   * @return (idxmerge, Ss) representing a (id_Y x merge) and a tensor
   * product of singular top-level nodes.
   *)
  val unmkG : G bgbdnf -> {idxmerge : bgval, Ss : S bgbdnf list}
  (** Deconstruct a S bgbdnf. 
   * @return (stlnode) representing a renaming concretion or a MBDNF.
   *)
  val unmkS : S bgbdnf -> stlnode
  (** Deconstruct a M bgbdnf. 
   * @return {idxion, N} representing id_Z tensor an ion, and a
   * NBDNF.
   *)
  val unmkM : M bgbdnf -> {idxion : bgval, N : N bgbdnf}

  (** Deconstruct a BR bgbdnf. 
   * @return (wirxid, D) representing (a wiring x id_(Xs)) and a DRBDNF.
   *)
  val unmkBR : BR bgbdnf -> {wirxid : bgval, D : DR bgbdnf}
  (** Deconstruct a D bgbdnf. 
   * @return (ren, Ps) representing a renaming and a tensor product
   * of PBDNF primes.
   *)
  val unmkDR : DR bgbdnf -> {ren : bgval, Ps : P bgbdnf list}

  (** Return the contextual information of a bgbdnf. *)
  val info : 'class bgbdnf -> info
  
  (** Return the inner face of a bgbdnf. *)
  val innerface : 'class bgbdnf -> interface
  
  (** Return the outer face of a bgbdnf. *)
  val outerface : 'class bgbdnf -> interface
    
  (** Signal that some term was not expected not to be BDNF.
   * @params file i m errtxt
   * @param file    the file name in which the exception was raised.
   * @param m       the match that caused the error.
   * @param errtxt  explanatory error text.
   *)
  exception MalformedBDNF of string * info * bgmatch * string
  (** Signal that some term was not expected not to be RBDNF.
   * @params file i m errtxt
   * @param file    the file name in which the exception was raised.
   * @param m       the match that caused the error.
   * @param errtxt  explanatory error text.
   *)
  exception MalformedRBDNF of string * info * bgmatch * string
  (** Signal that two lists unexpectedly are of unequal length.
   * @params file l1 l2 errtxt
   * @param file    the file name in which the exception was raised.
   * @param l1      the first list.
   * @param l2      the second list.
   * @param errtxt  explanatory error text.
   *)
  exception UnequalLength of string * bgval list * bgval list * string
  (** Signal that two lists unexpectedly are of unequal length.
   * @params file l1 l2 errtxt
   * @param file    the file name in which the exception was raised.
   * @param l1      the first list.
   * @param l2      the second list.
   * @param errtxt  explanatory error text.
   *)
  exception UnequalLength2
    of string * bgval list * (int * nameset) list * string
  

  (** Prettyprint a bgbdnf without parentheses around it.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgbdnf to print.
   *)
  val pp : int -> ppstream -> 'class bgbdnf -> unit
end
