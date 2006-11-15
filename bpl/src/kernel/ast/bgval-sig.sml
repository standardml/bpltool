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
  type bgterminfo
  type name
  type nameset
  type interface
  type wiring
  type ion
  type Immutable
  type 'kind permutation
  type bgterm
  type ppstream

  (** The bgval data type. *)
  type bgval
  (** Construct merge_n = one root containing n sites. *)
  val Mer : info -> int -> bgval
  (** Construct "X" = a concretion of a set of names. *)
  val Con : info -> nameset -> bgval
  (** Construct w = a general wiring. *)
  val Wir : info -> wiring -> bgval
  (** Signal duplicate names. 
   * @params file i namesets errtxt
   * @param file      File name for the code that detected the problem.
   * @param i         Contextual information.
   * @param namesets  The offending namesets.
   * @param errtxt    Explanatory error text.
   *)
  exception DuplicateNames of string * info * name list list * string
  (** Construct K_yX = an ion.
   * @exception DuplicateNames  if outer or inner names are not
   * distinct.
   *)
  val Ion : info -> ion -> bgval
  (** Construct pi = a permutation.
   * @exception DuplicateNames  if local names are not distinct.
   *)
  val Per : info -> 'kind permutation -> bgval
  (** Signal that a name is missing from the outer face of a prime
   * that is attempted abstracted. 
   * @params file v errtxt
   * @param file    File name for the code that detected the problem.
   * @param v       The bigraph value.
   * @param errtxt  text detailing the error.
   *)
  exception NameMissing of string * bgval * string
  (** Signal that a bigraph to be abstracted was not prime. 
   * @params file v errtxt
   * @param file    File name for the code that detected the problem.
   * @param v       The bigraph value.
   * @param errtxt  Explanatory error text.
   *)
  exception NotPrime of string * bgval * string
  (** Construct (X)P = an abstraction.
   * @exception NotPrime     i P is not prime.
   * @exception NameMissing  if X is not a subset of P's global outer
   *                          names.
   *)
  val Abs : info -> nameset * bgval -> bgval
  (** Signal that the inner or outer name sets of a list of bigraphs
   * were not disjoint. 
   * @params i vs errtxt
   * @param i       Contextual information.
   * @param vs      The list of values.
   * @param errtxt  Explanatory error text.
   *)
  exception NotTensorable of string * bgval list * string
  (** Construct b_1 x...x b_n-1 = a tensor product of n bigraphs.
   * @exception NotTensorable  if inner or outer names clash.
   *)
  val Ten : info -> bgval list -> bgval
  (** Signal that the interfaces of two bigraphs were not compatible. 
   * @params i v1 v2 errtxt
   * @param i       Contextual information.
   * @param v1      The first value.
   * @param v2      The second value.
   * @param errtxt  Explanatory error text.
   *)
  exception NotComposable of string * bgval * bgval * string
  (** Construct b_1 b_2 = a composition of a pair of bigraphs. 
   * @exception NotComposable  if the inner face of b_1 is different
   *                           from outer face of b_2.
   *)
  val Com : info -> bgval * bgval -> bgval

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
   * @param v2ti     Function that, given a bgval, returns the
   *                 corresponding bgterm contextual information.
   * @param v        The bgval to deconstruct.
   * @return (t, inner, outer)  t is a bgterm with the given inner and
   *                            outer faces. 
   *)
  val unmk : (bgval -> bgterminfo) 
	     -> bgval -> (bgterm * interface * interface)

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
  (** The default empty contextual information. *)
  val noinfo : info
  (** Convert a bgmatch to a bgval. 
   * @params i m
   * @param i  Contextual information for matched nodes. 
   * @param m  Match to convert.
   *)
  val match2bgval : info -> bgmatch -> bgval
  (** Signal that name sets in interfaces are not disjoint. 
   * @params file i I1 I2 errtxt
   * @param file    File name for the code that detected the problem.
   * @param i       Contextual information.
   * @param I1      Interface that clashes with I2.
   * @param I2      Interface that clashes with I1.
   * @param errtxt  Explanatory error text.
   *)
  exception NameClash of string * info * nameset * nameset * string
  (** Signal that the outer local or inner name sets of a list of bigraphs
   * were not disjoint. 
   * @params i vs errtxt
   * @param i       Contextual information.
   * @param vs      The list of values.
   * @param errtxt  Explanatory error text.
   *)
  exception NotParallelisable of string * bgval list * string
  (** Signal that the outer local and outer global name sets of a list
   * of bigraphs were not disjoint. 
   * @params i vs errtxt
   * @param i       Contextual information.
   * @param vs      The list of values.
   * @param errtxt  Explanatory error text.
   *)
  exception NotPrimeable of string * bgval list * string
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
   * @params file v errtxt
   * @param file    File name for the code that detected the problem.
   * @param v       The bigraph value.
   * @param errtxt  Explanatory error text.
   *)
  exception NotImplemented of string * bgval * string
  (** Determine whether a bgval is an identity.  NOTE: An
   * implementation is not required to implement this for bgvals
   * containing compositions. 
   * @exception NotImplemented  if the bgval passed contains compositions.
   *)
  val is_id : bgval -> bool
  (** Determine whether a bgval is the zero identity.  NOTE: An
   * implementation is not required to implement this for bgvals
   * containing compositions. 
   * @exception NotImplemented  if the bgval passed contains compositions.
   *)
  val is_id0 : bgval -> bool
  (** Prettyprint a bgval without parentheses around it.
   * @params indent pps t
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param t       The bgval to print.
   *)
  val pp : int -> ppstream -> bgval -> unit
  (** Prettyprint a bgmatch.
   * @params indent pps m
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param m       The bgmatch to print.
   *)
  val pp_match : int -> ppstream -> bgmatch -> unit

  val size : bgval -> int

end
