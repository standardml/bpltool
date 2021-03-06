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
  type control
  type ion
  type wiring
  type Immutable
  type 'kind permutation
  type bgval
  type bgmatch

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
  (** The bgbdnf data type.  'class must be M, S, G, N, P, D, DR, B or BR. *)
  type 'class bgbdnf
  (** Signal that a BDNF does not represent a regular bigraph.
   * @params i b errtxt
   * @param i       bigraph info
   * @param b       the bigraph
   * @param errtxt  explanatory error text.
   *)
  exception IrregularBDNF
   of info * bgval * Immutable permutation * nameset list list * string
  (** Construct a B bgbdnf from a bgval. *)
  val make : bgval -> B bgbdnf
  (** Deconstruct a bgbdnf. *)
  val unmk : 'class bgbdnf -> bgval
  (** Regularize a B bgbdnf.
   * @params b
   * @param  b the BDNF to regularize
   * @exception IrregularBDNF  if b is irregular.
   *)
  val regularize : B bgbdnf -> BR bgbdnf
  (** Add identity permutations to a BR bgbdnf thus putting it on B bgbdnf form.
   * (more efficient than <code>make o unmk</code>).
   * @params b
   * @param  b the RBDNF to unregularize
   *)
  val unregularize : BR bgbdnf -> B bgbdnf

  (** Test two BDNFs for equality modulo structural congruence
   *  and internal names.
   * @params b1 b2
   * @param b1  the first bigraph.
   * @param b2  the second bigraph.
   *)
  val eq : 'class bgbdnf -> 'class bgbdnf -> bool

  (** Test two BDNFs for equality modulo structural congruence,
   * internal names and internal representation of outer names.
   * If, say, b1 has outer name y internally represented as y_42,
   * and b2 has outer name y internally represented as y_17,
   * eq' will assume they are the same name. 
   * @params b1 b2
   * @param b1  the first bigraph.
   * @param b2  the second bigraph.
   *)
  val eq' : 'class bgbdnf -> 'class bgbdnf -> bool

  (** Replace ion controls by looking their names up in the list. *)
  val replacectrls : control list -> 'a bgbdnf -> 'a bgbdnf

  (** Sum type for singular top-level nodes. The renaming concretion is
      represented by just the renaming. *)
  datatype stlnode =
           SCon of info * wiring
         | SMol of M bgbdnf
  (** Sum type for singular top-level nodes. *)
  datatype stlnode' =
           SCon' of bgval
         | SMol' of M bgbdnf

  (** Construct a B bgbdnf from a wiring, nameset list and D bgbdnf. *)
  val makeB : wiring -> nameset list -> D bgbdnf -> B bgbdnf
  (** Construct a BR bgbdnf from a wiring, nameset list and DR bgbdnf. *)
  val makeBR : wiring -> nameset list -> DR bgbdnf -> BR bgbdnf
  (** Construct a D bgbdnf from a renaming, P bgbdnf list and a permutation.
   * NOTE: it is not checked whether alpha is a renaming!
	 * @params alpha Ps pi
   *)
  val makeD : wiring -> P bgbdnf list -> 'kind permutation -> D bgbdnf
  (** Construct a DR bgbdnf from a renaming and a P bgbdnf list.
   * NOTE: it is not checked whether alpha is a renaming!
	 * @params alpha Ps
   *)
  val makeDR : wiring -> P bgbdnf list -> DR bgbdnf
  (** Construct a P bgbdnf from a substitution and an N bgbdnf.
   * NOTE: it is not checked whether sigma is a substitution!
   * @params sigma N
   *)
  val makeP : wiring -> N bgbdnf -> P bgbdnf
  (** Construct an N bgbdnf from a name set and a G bgbdnf.
   * NOTE: it is not checked whether X is a subset of G's outer names!
   * @params X G
   *)
  val makeN : nameset -> G bgbdnf -> N bgbdnf
  (** Construct an G bgbdnf from a list of S bgbdnf's.
   * @params Y Ss
   *)
  val makeG : S bgbdnf list -> G bgbdnf
  (** Construct an S bgbdnf from an stlnode (renaming concretion or molecule).
   * NOTE: it is not checked whether a given wiring actually is a renaming!
   * @params stlnode
   *)
  val makeS : stlnode -> S bgbdnf
  (** Construct an M bgbdnf from an ion and an M bgbdnf.
   * @params Z KyX N
   *)
  val makeM : ion -> N bgbdnf -> M bgbdnf
  (** Deconstruct a B bgbdnf. 
   * @return (wirxid, D) representing (a wiring x id_(Xs)) and a DBDNF.
   *)
  val unmkB : B bgbdnf -> {wirxid : bgval, D : D bgbdnf}
  (** Deconstruct a B bgbdnf. 
   * @return (wir, D) representing a wiring, a local identity,
   * and a DBDNF.
   *)
  val unmkB' : B bgbdnf
    -> {wir : wiring, id_X : Immutable permutation, D : D bgbdnf}
  (** Deconstruct a D bgbdnf. 
   * @return (ren, Ps, perm) representing a renaming, a tensor product
   * of PBDNF primes, and a permutation
   *)
  val unmkD
      : D bgbdnf -> {ren : bgval, Ps : P bgbdnf list, perm : bgval}
  (** Deconstruct a D bgbdnf. 
   * @return (ren, Ps, perm) representing a renaming, a tensor product
   * of PBDNF primes, and a permutation
   *)
  val unmkD' : D bgbdnf 
    -> {ren : wiring, Ps : P bgbdnf list, perm : Immutable permutation}
  (** Deconstruct a P bgbdnf, yielding basic elements and a bgbdnf. 
   * @params P
   * @param P a prime on the form (id_Z * (Y)(s * id_1)"X") N
   *)
  val unmkP : P bgbdnf
    -> {id_Z : wiring, Y : nameset, s : wiring, X : nameset, N : N bgbdnf}
  (** Deconstruct a P bgbdnf, yielding a bgval and bgbdnf. 
   * @return (idxlocsub, N) representing (id_Z x a local substitution)
   * and a NBDNF.  The local substitution is on the form 
   * (Y)(w x id_1)"X", where w is a wiring and id_1 an identity
   * permutation.
   *)
  val unmkP' : P bgbdnf -> {idxlocsub : bgval, N : N bgbdnf}
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
  (** Deconstruct a S bgbdnf. 
   * @return (stlnode) representing a renaming concretion or a MBDNF.
   *)
  val unmkS' : S bgbdnf -> stlnode'
  (** Deconstruct a M bgbdnf. 
   * @return {idxion, N} representing id_Z tensor an ion, and a
   * NBDNF.
   *)
  val unmkM : M bgbdnf -> {id_Z : wiring, KyX : ion, N : N bgbdnf}
  (** Deconstruct a M bgbdnf. 
   * @return {idxion, N} representing id_Z tensor an ion, and a
   * NBDNF.
   *)
  val unmkM' : M bgbdnf -> {idxion : bgval, N : N bgbdnf}

  (** Deconstruct a BR bgbdnf. 
   * @return (wirxid, D) representing (a wiring x id_(Xs)) and a DRBDNF.
   *)
  val unmkBR : BR bgbdnf -> {wirxid : bgval, D : DR bgbdnf}
  (** Deconstruct a BR bgbdnf. 
   * @return (wir, Xs, D) representing a wiring, nameset list, and a DRBDNF.
   *)
  val unmkBR' : BR bgbdnf -> {wir : wiring, Xs : nameset list, D : DR bgbdnf}
  (** Deconstruct a DR bgbdnf. 
   * @return (ren, Ps) representing a renaming and a tensor product
   * of PBDNF primes.
   *)
  val unmkDR : DR bgbdnf -> {ren : bgval, Ps : P bgbdnf list}
  (** Deconstruct a DR bgbdnf. 
   * @return (wir, Ps) representing a wiring and a tensor product
   * of PBDNF primes.
   *)
  val unmkDR' : DR bgbdnf -> {wir : wiring, Ps : P bgbdnf list}

  (** Return the contextual information of a bgbdnf. *)
  val info : 'class bgbdnf -> info
  
  (** Return the inner face of a bgbdnf. *)
  val innerface : 'class bgbdnf -> interface
  
  (** Return the outer face of a bgbdnf. *)
  val outerface : 'class bgbdnf -> interface
    
  (** Signal that some term was not expected not to be BDNF.
   * @params i m errtxt
   * @param i       bigraph info
   * @param m       the match that caused the error.
   * @param errtxt  explanatory error text.
   *)
  exception MalformedBDNF of info * bgmatch * string
  (** Signal that some term was not expected not to be RBDNF.
   * @params i m errtxt
   * @param i       bigraph info
   * @param m       the match that caused the error.
   * @param errtxt  explanatory error text.
   *)
  exception MalformedRBDNF of info * bgmatch * string
  (** Signal that two lists unexpectedly are of unequal length.
   * @params l1 l2 errtxt
   * @param l1      the first list.
   * @param l2      the second list.
   * @param errtxt  explanatory error text.
   *)
  exception UnequalLength of bgval list * bgval list * string
  (** Signal a logical error, i.e. an error which "cannot happen" ;-).
   * @params errtxt
   * @param errtxt  explanatory error text.
   *)
  exception LogicalError of string
  

  (** Prettyprint a bgbdnf without parentheses around it. Will try to avoid
   * the long version of link names with internal numbers by using
   * Name.pp_unchanged, Name.pp_unchanged_add, and Name.pp_unchanged_remove.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgbdnf to print.
   *)
  val pp : int -> PrettyPrint.ppstream -> 'class bgbdnf -> unit
  val oldpp : int -> PrettyPrint.ppstream -> 'class bgbdnf -> unit
  (** Prettyprint a bgbdnf without parentheses around it. Will try to avoid
   * the long version of link names with internal numbers by using
   * Name.pp_unchanged_add and Name.pp_unchanged_remove but will not call
   * Name.pp_unchanged.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgbdnf to print.
   *)
  val pp' : int -> PrettyPrint.ppstream -> 'class bgbdnf -> unit


  (** Prettyprint a bgbdnf with interfaces, without parentheses around it.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgbdnf to print.
   *)
  val ppWithIface : int -> PrettyPrint.ppstream -> 'class bgbdnf -> unit

  (** Return a prettyprinted string representation of a bgbdnf. *)
  val toString : 'class bgbdnf -> string

  val size : 'class bgbdnf -> int

  (** Maximum revision number for containing components.*)
  val revision : string
end
