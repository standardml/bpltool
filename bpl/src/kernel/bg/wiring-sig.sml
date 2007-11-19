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

(** Abstract data type for modelling wirings.
 * @version $LastChangedRevision$
 *)
signature WIRING =
sig
  type name
  type nameedge
  type nameset
  type nameconstraints
  type 'a namemap
  type link
  type linkset

  (** The wiring data type. *)
  type wiring
  (** Signal that a wiring is not a renaming.
   * @params wiring errtxt
   * @param wiring  The wiring.
   * @param errtxt  Explanatory error text.
   *)
  exception NotARenaming of wiring * string
  (** Signal that a wiring has multiple links whose names clash.
   * @params wiring errtxt
   * @param wiring  The wiring.
   * @param errtxt  Explanatory error text.
   *)
  exception NotAWiring of wiring * string
  (** Signal that a wiring has closed edges.
   * @params wiring errtxt
   * @param wiring  The wiring.
   * @param errtxt  Explanatory error text.
   *)
  exception NotASubstitution of wiring * string
  (** Construct a wiring from a link set.  Inner names must be
   * disjoint, outer names need not be. *)
  val make : linkset -> wiring
  (** Construct a wiring from a list of links.  Inner names must be
   * disjoint, outer names need not be.
   *)
  val make' : link list -> wiring
  (** Construct a renaming from a map from inner names to outer names.
   * The map must be injective.
   *)
  val make_ren : name namemap -> wiring
  (** Construct a name introduction from a set of outer names.
   *)
  val make_intro : nameset -> wiring
  (** Deconstruct a wiring. *)
  val unmk : wiring -> linkset
  (** Deconstruct a substitution into a map from inner to outer names.
   * @exception NotASubstitution if the wiring is not a substitution.
   *)
  val unmk_sub : wiring -> name namemap
  (** Deconstruct a renaming.
   * @exception NotARenaming if the wiring is not a renaming.
   *)
  val unmk_ren : wiring -> name namemap

  (** Test two wirings for equality.
   * @params w1 w2
   * @param w1  the first wiring.
   * @param w2  the second wiring.
   *)
  val eq : wiring -> wiring -> bool
  (** Test two wirings for equality up to a bijection between the inner
   * names satisfying the constraints given by C.
   * 
   * If the wirings are equal, a set of constraints on a bijection between the
   * outer names is returned.
   *
   * @params C w1 w2
   * @param C   constraints on the bijection between the inner names of the two
   *            wirings.
   * @param w1  the first wiring.
   * @param w2  the second wiring.
   *)
  val eq' : nameconstraints -> wiring -> wiring -> nameconstraints option
  (** Signal that two wirings cannot be extended due to a link
   * clash involving an inner name, and an outer name of the second wiring.
   *)
  exception CannotExtend of wiring * wiring * name* nameedge

  (** Return the set of inner names of the wiring. *)
  val innernames : wiring -> nameset
  (** Return the set of outer names of the wiring. *)
  val outernames : wiring -> nameset

  (** Return the set of names introduced, i.e., that have no points. *)
  val introductions : wiring -> nameset

  (** Compose two wirings.
   * The inner face of w1 need not match the outer face of w2, in
   * which case inner names not mapped by w2 to the domain
   * of w1 will be closed by w, and names mapped by w1 that are not
   * present in the image of w2 will be names introduced by w.
   * @params w1 w2
   *)
  val o : (wiring * wiring) -> wiring
  (** Make the tensor product of two wirings.  Note that disjointness
   * of interfaces is NOT checked; the result in this case will be
   * undefined. 
   *)
  val * : (wiring * wiring) -> wiring
  (** Make the tensor product of a list of wirings.  Note that
   * disjointness of interfaces is NOT checked; the result in this case
   * will be undefined. 
   *)
  val ** : wiring list -> wiring
  (** Make the parallel product of two wirings.  Note that disjointness
   * of inner faces is NOT checked; the result in this case will be
   * undefined. 
   *)
  val || : (wiring * wiring) -> wiring
  (** Make the parallel product of a list of wirings.  Note that disjointness
   * of inner faces is NOT checked; the result in this case will be
   * undefined. 
   *)
  val ||| : wiring list -> wiring
  (** Make the extension of two wirings.  Expressing wirings using
   * substitutions
   * by w_1 = s || s_1 || /X_1 (s' || s'_1) and
   *    w_2 = s || s_2 || /X_2 (s' || s'_2) where s_1 and s_2
   * and s'_1 and s'_2 have disjoint inner name sets, we have
   * w_1 + w_2 = s || s_1 || s_2 || /(X_1 U X_2) (s' || w'_1 || w'_2).
   *)
  val + : (wiring * wiring) -> wiring
  (** Make the extension of a list of wirings.  For a list of wirings
   * w_1, ..., w_n that can be written using substitutions as
   * w_i = s || s_i || /X_i (s' || s'_i), where all s'_i's and s_i's
   * have disjoint inner name sets, and X_i are the outer names of
   * s' and s'_i, we have
   * ++[w_1, ..., w_n] = s || s_1 || ... || s_n
   *            || /(X_1 U ... U X_n) (s' || s'_1 || ... || s'_n).
   * ++[] = id_0.
   *)
  val ++ : wiring list -> wiring

  (** Signal that a name is not in the domain of a wiring.
   * @params wiring name errtxt
   * @param wiring  The wiring.
   * @param name    The name not in the domain of the wiring.
   * @param errtxt  Explanatory error text.
   *)
  exception NotInDomain of wiring * name * string
  (** Signal that some names are not in the codomain of a wiring.
   * @params wiring names errtxt
   * @param wiring  The wiring.
   * @param names   The names not in the codomain of the wiring.
   * @param errtxt  Explanatory error text.
   *)
  exception NotInCodomain of wiring * nameset * string

  (** Determine whether the name is in the domain of wiring
   *  (i.e. it is an inner name).
   * @params name wiring
   * @param name    The name.
   * @param wiring  The wiring.
   *)
  val in_domain : name -> wiring -> bool

  (** Determine whether the name is in the codomain of wiring
   *  (i.e. it is an outer name).
   * @params name wiring
   * @param name    The name.
   * @param wiring  The wiring.
   *)
  val in_codomain : name -> wiring -> bool

  (** Rename the outer names of a wiring.  Outer names not in the
   * domain of the renaming will not be changed.
   * @params map wiring
   * @param map     A map representing the renaming.
   * @param wiring  The wiring to rename.
   *)
  val rename_outernames : name namemap -> wiring -> wiring

  (** Compute the set of names to which the wiring maps a given set.
   * @params wiring X
   * @param wiring  The wiring.
   * @param X       The set of names.
   * @exception NotInDomain if X contains a name which is not in
   *                        the domain of the wiring.
   *)
  val app : wiring -> nameset -> nameset
  (** Compute the name to which the wiring maps a given name.
   * @params wiring x
   * @param wiring  The wiring.
   * @param X       The name.
   * @exception NotInDomain if x is not in the domain of the wiring.
   *)
  val app_x : wiring -> name -> name option
  (** Compute the set of names which the wiring maps to a given set.
   * @params wiring X
   * @param wiring  The wiring.d
   * @param X       The set of names.
   * @exception NotInCodomain if X contains a name which is not in
   *                          the codomain of the wiring.
   *)
  val app_inverse : wiring -> nameset -> nameset

  (** Compute the name to which the wiring maps a given name.
   * It is not checked that the wiring is a renaming - only that if the 
   * name is in the domain, then it is mapped to a name in the codomain.
   * @params wiring x
   * @param wiring  The wiring.
   * @param x       The name.
   * @exception NotInDomain if x is not in the domain of the renaming.
   * @exception NotARenaming if the wiring is not a renaming.
   *)
  val app_renaming_x : wiring -> name -> name
  (** Compute the name which the renaming maps to a name.
   * It is not checked that the wiring is a renaming - only that if the
   * name is in the codomain, then precisely one name maps to it.
   * @params wiring x
   * @param wiring  The wiring.
   * @param x       The name.
   * @exception NotInCodomain if x is not in the domain of the renaming.
   * @exception NotARenaming if the wiring is not a renaming.
   *)
  val app_renaming_inverse_x : wiring -> name -> name
  (** Create the inverse of a renaming.
   * @exception NotARenaming if the wiring is not a renaming.
   *)
  val invert_renaming : wiring -> wiring

  (** Restrict a wiring to only map a given set of names.
   * The outer face is trimmed to include only names to which some
   * inner name maps.  Each name in the set not mapped by the wiring
   * will result in a single, closed link.
   *)
  val restrict : wiring -> nameset -> wiring
  (** Restrict a wiring to only map a given set of names.
   * The outer face is unchanged.  The inner face is not increased.
   *)
  val restrict' : wiring -> nameset -> wiring
  (** Restrict a wiring to not map a given set of names.
   * The outer face is unchanged.  The inner face is not increased.
   *)
  val domdiff' : wiring -> nameset -> wiring
  (** Restrict a wiring to only map a given set of names.
   * The outer face is trimmed to include only names to which some
   * inner name maps.  The inner face is not increased.
   *)
  val restrict'' : wiring -> nameset -> wiring
  (** Restrict a wiring to only map a given set of names.
   * The outer face is trimmed to include only names to which some
   * inner name maps, and that satisfy a predicate.
   * The inner face is not increased.
   *)
  val restrict''' : wiring -> nameset -> (name -> bool) -> wiring
  (** Restrict a wiring to only map to a given set of names.
   * The inner face is trimmed to include only names which maps to an
   * outer name.
   *)
  val restrict_outer : wiring -> nameset -> wiring
  (** Split a wiring into two, one that maps from a given domain, and 
   * one that does not map from the domain.
   * @params w X
   * @return {inDom, notInDom} so that w = inDom || notInDom, and the
   *         points of inDom are in X, and the points of notInDom are
   *         not in X.
   *)
  val split : wiring -> nameset -> {inDom : wiring, notInDom : wiring}
  (** Split a wiring into two, one of which maps to a given set of names.
	 * @params w Y
	 * @return {inCod, notInCod}
	 *   Wirings that satisfy w = w_inCod x w_notInCod and
	 *   w_inCod : X -> Y and w_inCod contains no internal edges. 
   *)
  val split_outer : wiring -> nameset -> {inCod : wiring, notInCod : wiring}
  (** Split wiring into closed and open part, then turn closed edges
   * into links with fresh names.    Any fresh
   * names generated will not clash with outer names of w.
   * @params w
   * @param w          Wiring to split and open.
   * @return {opened, rest, newnames}  A wiring containing just the
   *                   opened links, a wiring containing just the
   *                   original open links, the set of fresh names.
   *)
  val splitopen : wiring -> {opened : wiring,
                             rest : wiring,
                             newnames : nameset}
  (** Turn closed edges into links with fresh names.  Any fresh
   * names generated will not clash with outer names of w.
   * @params w
   * @param w          Wiring to open.
   * @return {opened, newnames}
   *                   A wiring containing just open links,
   *                   the set of new names added, and any fresh
   *                   names.  We have w = (id * /newnames) o opened.
   *)
  val openup : wiring -> {opened: wiring,
                          newnames : nameset}
  (** Close some open links. *)
  val closelinks : nameset -> wiring -> wiring
  (** Remove idle edges, i.e., edges with no inner names. *)
  val removeidles : wiring -> wiring
  (** Determine whether some wiring is an identity. *)
  val is_id : wiring -> bool
  (** Determine whether some wiring is a zero identity. *)
  val is_id0 : wiring -> bool
  (** Determine whether some wiring can be written w = id_Y x sigma,
   * where sigma is a substitution (i.e., no closed links).
   * @params Y w
   *)
  val is_id_x_sigma : nameset -> wiring -> bool
  (** Determine whether some wiring can be written w = id_Y x sigma,
   * where sigma is a substitution (i.e., no closed links), returning
   * SOME sigma.
   * If the wiring cannot be written on this form, NONE is returned.
   * @params Y w
   *)
  val remove_id_Y : nameset -> wiring -> wiring option
  (** Determine whether some wiring is a renaming. *)
  val is_renaming : wiring -> bool
  (** Determine whether some wiring has at least one internal edge. *)
  val has_edge : wiring -> bool
  (** Construct an identity wiring on a set of names. *)
  val id_X : nameset -> wiring
  (** Construct an empty identity wiring. *)
  val id_0 : wiring
  (** Construct a wiring that introduces names. *)
  val introduce : nameset -> wiring
  (** Construct a wiring that closes names without linking them together. *)
  val close : nameset -> wiring
  (** Prettyprint a wiring.
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param w       The wiring to output.
   *)
  val pp : int -> PrettyPrint.ppstream -> wiring -> unit
  val oldpp : int -> PrettyPrint.ppstream -> wiring -> unit
  (** Return a prettyprinted string representation of a wiring. *)
  val toString : wiring -> string
end
