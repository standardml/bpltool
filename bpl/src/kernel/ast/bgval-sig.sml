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

(** Abstract data type for bigraph values, i.e., well-formed bigraph
 * terms with interfaces.
 * <p>
 * Matching a bgval is done by constructing a pattern, calling match,
 * and then performing SML match on the result.  For instance,
 * matching an abstraction of a molecule is done like this:
 * <pre>
 * fun analyse v 
 *    = case match (PAbs (   PCom (PIon, PVar  ))) v of
 *                  MAbs (X, MCom (MIon, MVal P))
 *                    => ...X...P...
 *                | _ => ...no match...
 * </pre>
 * Matching just the top level constructor is done like this:
 * <pre>
 * fun analyse v
 *     = case match PCns v of
 *	   MMer n => ...n...
 *	 | MCon X => ...X...
 *	 | MWir w => ...w...
 *	 | MIon KyX => ...KyX...
 *	 | MPer pi => ...pi...
 *	 | MAbs (X, MVal v) => ...X...v...
 *	 | MCom (MVal v1, MVal v2) => ...v1...v2...
 *	 | MTns vs => ...vs...
 * </pre>
 * match is guaranteed to only return these match constructors in
 * place of a PCns constructor.
 * 
 * @version $LastChangedRevision$
 *)
signature BGVAL =
sig
  type info
  type name
  type nameset
  type interface
  type wiring
  type control
  type ion
  type Immutable
  type 'kind permutation
  type bgterm

  (** The bgval data type. *)
  type bgval
  (** Test two bgvals for equality: same structure, same interfaces,
   * but internal names may differ.
   * @params b1 b2
   * @param b1  the first bigraph term.
   * @param b2  the second bigraph term.
   *)
  val eq : bgval -> bgval -> bool
  (** Construct merge_n = one root containing n sites. *)
  val Mer : info -> int -> bgval
  (** Construct "X" = a concretion of a set of names. *)
  val Con : info -> nameset -> bgval
  (** Construct w = a general wiring. *)
  val Wir : info -> wiring -> bgval
  (** Signal duplicate names. 
   * @params i namesets errtxt
   * @param i         Contextual information.
   * @param namesets  The offending namesets.
   * @param errtxt    Explanatory error text.
   *)
  exception DuplicateNames of info * name list list * string
  (** Construct K_yX = an ion.
   * @exception DuplicateNames  if outer or inner names are not
   * distinct.
   *)
  val Ion : info -> ion -> bgval
  (** Construct a bigraph equivalent to (w * idp(1)) o K_y'X where
   * y' is a vector of m distinct names, arity(K) = m -> n, and
   * w : {y'} -> {y} is a substitution satisfying y = w(y').
   * @params i K_yX
   * @param i     Contextual information.
   * @param K_yX  An ion which might have duplicate outer names.
   * @exception DuplicateNames  if inner names are not distinct.
   *)
  val Ion' : info -> ion -> bgval
  (** Construct pi = a permutation.
   * @exception DuplicateNames  if local names are not distinct.
   *)
  val Per : info -> 'kind permutation -> bgval
  (** Signal that a name is missing from the outer face of a prime
   * that is attempted abstracted. 
   * @params v errtxt
   * @param v       The bigraph value.
   * @param errtxt  text detailing the error.
   *)
  exception NameMissing of bgval * string
  (** Signal that a bigraph to be abstracted was not prime. 
   * @params v errtxt
   * @param v       The bigraph value.
   * @param errtxt  Explanatory error text.
   *)
  exception NotPrime of bgval * string
  (** Construct (X)P = an abstraction.
   * @exception NotPrime     i P is not prime.
   * @exception NameMissing  if X is not a subset of P's global outer
   *                          names.
   *)
  val Abs : info -> nameset * bgval -> bgval
  (** Construct a bigraph equivalent to (X)(P * Y * id) where Y are
   * the names of X which are not in the outer face of P and id is
   * either idp(1) if width(P) = 0 and idp(0) otherwise.
   *
   * @exception NotPrime i P is not prime (except when P : Z).
   *)
  val Abs' : info -> nameset * bgval -> bgval
  (** Signal that the inner or outer name sets of a list of bigraphs
   * were not disjoint. 
   * @params vs errtxt
   * @param vs      The list of values.
   * @param errtxt  Explanatory error text.
   *)
  exception NotTensorable of bgval list * string
  (** Construct b_1 x...x b_n-1 = a tensor product of n bigraphs.
   * @exception NotTensorable  if inner or outer names clash.
   *)
  val Ten : info -> bgval list -> bgval
  (** Signal that the interfaces of two bigraphs were not compatible. 
   * @params v1 v2 errtxt
   * @param v1      The first value.
   * @param v2      The second value.
   * @param errtxt  Explanatory error text.
   *)
  exception NotComposable of bgval * bgval * string
  (** Construct b_1 b_2 = a composition of a pair of bigraphs. 
   * @exception NotComposable  if the inner face of b_1 is different
   *                           from outer face of b_2.
   *)
  val Com : info -> bgval * bgval -> bgval
  (** Construct a bigraph equivalent to (b_1 || id) b_2 or
   * (b_1 || id) ((X) (b_2 * Y)), that is, a composition of a pair of
   * bigraphs, where id is an appropriate identity and, if b_2 is prime,
   * X and Y appropriate namesets allowing composition.  This will only
   * work when the inner width of b_1 matches the outer width of b_2, or
   * if b_1 is a wiring.
   * @exception NotComposable  if the inner face of b_1 is positive and
   *                           different
   *                           from outer face of b_2 (b_1 is allowed
   *                           to have only a subset of the global outer
   *                           names of b_2 as global inner names).
   *)
  val Com' : info -> bgval * bgval -> bgval

  (** Construct a bgval from a bgterm.  The bgterm is checked
   * internally for interface consistency.
   * @params t2i t
   * @param t2i  Function that, given a bgterm node, returns the
   *             contextual information for the corresponding bgval
   *             node.
   * @param t    The bgterm from which the corresponding bgval is
   *             constructed.
   *)
  val make : (bgterm -> info) -> bgterm -> bgval
  (** Deconstruct a bgterm.  Normally, this is not the function you
   * want for matching a bgval, because it deconstructs the value all
   * the way down the syntax tree.  Instead match on the result of a call
   * to match.
   * @params v2ti v
   * @param v        The bgval to deconstruct.
   * @return (t, inner, outer)  t is a bgterm with the given inner and
   *                            outer faces. 
   *)
  val unmk : bgval -> (bgterm * interface * interface)

  (** Replace internal names with fresh names. Replaces as many
   *  internal names as possible with globally fresh names.
   *  @params v
   *  @param v The bgval to rename internally.  *)
  val rename_internally : bgval -> bgval

  (** Pattern datatype for bgvals.
   * @see BGTERM.bgterm
   *)
  datatype bgpat =
	   PVar
	 | PCns
	 | PMer
	 | PCon
	 | PWir
	 | PIon
	 | PPer
	 | PAbs of bgpat
	 | PTen of bgpat list
	 | PTns
	 | PCom of bgpat * bgpat
  (** Matched pattern datatype for bgvals.
   * @see bgpat.
   *)
  datatype bgmatch =
	   MVal of bgval
	 | MMer of int	      
	 | MCon of nameset    
	 | MWir of wiring     
	 | MIon of ion	      
	 | MPer of Immutable permutation
	 | MAbs of nameset * bgmatch
	 | MTen of bgmatch list
	 | MTns of bgval list
	 | MCom of bgmatch * bgmatch

  (** Attempt to pattern match a bgval.
   * @params pat val
   * @return       A match structure isomorphic to pat, where PVar's
   *               have been replaced with MVal's, PCns' have been
   *               replaced with MMer, MCon, MWir, MIon, MPer,
   *               MAbs, MCom or MTns, and other Pxxx's have been
   *               replaced with Mxxx.
   *               If a node in the pattern tree
   *               does not match the corresponding node in the value
   *               tree, an MVal is returned at the corresponding node
   *               in the match tree. 
   * @param pat    The pattern to match.
   * @param val    The value to match.
   *)
  val match : bgpat -> bgval -> bgmatch
  (** Return the inner face of a bgval. *)
  val innerface : bgval -> interface
  (** Return the outer face of a bgval. *)
  val outerface : bgval -> interface
  (** Return the contextual information of a bgval. *)
  val info : bgval -> info
  (** Convert a bgmatch to a bgval. 
   * @params i m
   * @param i  Contextual information for matched nodes. 
   * @param m  Match to convert.
   *)
  val match2bgval : info -> bgmatch -> bgval
  (** Signal that name sets in interfaces are not disjoint. 
   * @params i I1 I2 errtxt
   * @param i       Contextual information.
   * @param I1      Interface that clashes with I2.
   * @param I2      Interface that clashes with I1.
   * @param errtxt  Explanatory error text.
   *)
  exception NameClash of info * nameset * nameset * string
  (** Signal that the outer local or inner name sets of a list of bigraphs
   * were not disjoint. 
   * @params vs errtxt
   * @param vs      The list of values.
   * @param errtxt  Explanatory error text.
   *)
  exception NotParallelisable of bgval list * string
  (** Signal that the outer local and outer global name sets of a list
   * of bigraphs were not disjoint. 
   * @params vs errtxt
   * @param vs      The list of values.
   * @param errtxt  Explanatory error text.
   *)
  exception NotPrimeable of bgval list * string
  (** Construct a local substitution.
   *)
  val LS : info -> wiring -> bgval
  (** Construct a wide local substitution.  The width of the
   * substitution is determined by the length of the list of wirings. 
   * @exception NameClash if wiring inner or outer names are not disjoint.
   *)
  val WLS : info -> wiring list -> bgval
  (** Construct a parallel product of a list of bgvals.
   * Given a list of bgvals                                       <br />
   * v_0, ..., v_n-1, if any outer names clash, the general result has
   * the form                                                     <br />
   * A ((w_0 * B_0) v_0 * ... * (w_n-1 * B_n-1) v_n-1), where     <br />
   * B_i = idp_[X^i_0, ..., X^i_{k_i-1}] (an identity permutation)<br />
   *   A = (w_inv * B_0 * ... * B_n-1)                            <br />
   * and (w_inv w_i) is an identity wiring.  Otherwise, a tensor
   * product is returned.  The actual result may
   * be smaller, with identity bigraphs optimised away.
   * @exception NotParallelisable  if outer local or inner names are
   * not disjoint. 
   *)
  val Par : info -> bgval list -> bgval
  (** Construct a prime product of a list of bgvals.
   * Given a list of bgvals                                           <br />
   * v_0, ..., v_n-1, the general result has the form                 <br />
   * (X)A (B_0 v_0 * ... * B_n-1 v_n-1), where                           <br />
   * v_i has outer width k_i                                          <br />
   * B_i = (w_i * idp_{k_i}) (idw_i * 'X^i_0' * ... * 'X^i_{k_i-1}')  <br />
   *   A = w_inv * merge_K                                            <br />
   *   X = X^0_0 u ... u X^{n-1}_{k_{n-1}-1}                          <br />
   *   K = k_0 + ... + k_n-1                                          <br />
   * and (w_inv w_i) is an identity wiring.  (idw and idp are identity
   * wirings and permutations, respectively.)  The actual result may
   * be smaller, with identity bigraphs optimised away.
   * @exception NotPrimeable  if an outer global name clashes with an
   *                          outer local name, or if the inner face
   *                          is not local.
   *)
  val Pri : info -> bgval list -> bgval
  (** Signal that some operation on a bgval is not implemented.
   * @params errtxt
   * @param errtxt  Explanatory error text.
   *)
  exception NotImplemented of string
  (** Determine whether a bgval is an identity.  NOTE: An
   * implementation is not required to implement this for bgvals
   * containing compositions. 
   * @exception NotImplemented  if the bgval passed contains compositions.
   *)
  val is_id : bgval -> bool
  (** If true is returned, the bgval is an identity.  NOTE: the
   * bgval might be an identity even if false is returned.
   *)
  val is_id' : bgval -> bool
  (** Determine whether a bgval is the zero identity.  NOTE: An
   * implementation is not required to implement this for bgvals
   * containing compositions. 
   * @exception NotImplemented  if the bgval passed contains compositions.
   *)
  val is_id0 : bgval -> bool
  (** If true is returned, the bgval is the zero identity.  NOTE:
   * the bgval might be the zero identity even if false is 
   * returned.
   *)
  val is_id0' : bgval -> bool
  (** Simplify a bgval by heuristically removing id_0, composition
   * with id, etc.
   *)
  val simplify : bgval -> bgval
  (** Replace ion controls by looking their names up in the list. *)
  val replacectrls : control list -> bgval -> bgval
  (** Prettyprint a bgval without parentheses around it. Will try to avoid
   * the long version of link names with internal numbers by using
   * Name.pp_unchanged, Name.pp_unchanged_add, and Name.pp_unchanged_remove.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgval to print.
   *)
  val pp : int -> PrettyPrint.ppstream -> bgval -> unit
  (** Prettyprint a bgval without parentheses around it. Will try to avoid
   * the long version of link names with internal numbers by using
   * Name.pp_unchanged_add and Name.pp_unchanged_remove but will not call
   * Name.pp_unchanged.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgval to print.
   *)
  val pp' : int -> PrettyPrint.ppstream -> bgval -> unit
  (** Prettyprint a bgval without parentheses around it,
   * using the old syntax.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgval to print.
   *)
  val oldpp : int -> PrettyPrint.ppstream -> bgval -> unit
  (** Prettyprint a bgval without parentheses around it, using the
   * long version of link names that include the internal number. *)
  val pp_unchanged : int -> PrettyPrint.ppstream -> bgval -> unit
  (** Prettyprint a bgval with interfaces, without parentheses around it.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgval to print.
   *)
  val ppWithIface : int -> PrettyPrint.ppstream -> bgval -> unit
  (** Prettyprint a bgmatch.
   * @params indent pps m
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param m       The bgmatch to print.
   *)
  val pp_match : int -> PrettyPrint.ppstream -> bgmatch -> unit

  (** Return a prettyprinted string representation of a bgval. *)
  val toString : bgval -> string

  (** Return a prettyprinted string representation of a bgval, where
   * link names are shown in their long version with internal numbers. *)
  val toString_unchanged : bgval -> string

  val size : bgval -> int

  (** Revision number.*)
  val revision : string

  (** tensor2parallel
   * Substitutes || for ** by removal of y//X's.
   * 
   * TODO: NOT FULLY TESTED.
   **)
  val t2p : bgval -> (bgterm * interface * interface)

end
