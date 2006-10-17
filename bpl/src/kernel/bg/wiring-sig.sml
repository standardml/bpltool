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
  type nameedge
  type link
  type linkset
  type nameset
  type ppstream

  (** The wiring data type. *)
  type wiring
  (** Construct a wiring from a link set.  Inner names must be
   * disjoint, outer names need not be. *)
  val make : linkset -> wiring
  (** Construct a wiring from a list of links.  Inner names must be
   * disjoint, outer names need not be.
   *)
  val make' : link list -> wiring
  (** Deconstruct a wiring. *)
  val unmk : wiring -> linkset

  (** Signal that two wirings cannot be extended due to a link
   * clash involving an outer name of the second wiring.
   *)
  exception CannotExtend of wiring * wiring * nameedge

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

  (** Compute the set of names to which the wiring maps a given set. *)
  val app : wiring -> nameset -> nameset
  (** Compute the set of names which the wiring maps to a given set. *)
  val app_inverse : wiring -> nameset -> nameset
  (** Restrict a wiring to only map a given set of names.
   * The outer face is trimmed to include only names to which some
   * inner name maps.  Each name in the set not mapped by the wiring
   * will result in a single, closed link.
   *)
  val restrict : wiring -> nameset -> wiring
  (** Restrict a wiring to only map to a given set of names.
   * The inner face is trimmed to include only names which maps to an
   * outer name.
   *)
  val restrict_outer : wiring -> nameset -> wiring
  (** Split wiring into closed and open part, then turn closed edges
   * into links with fresh names.    Any fresh
   * names generated will not clash with outer names of w.
   * @params w usednames
   * @param w          Wiring to split and open.
   * @param usednames  Names not to use when generating fresh names.
   * @return {opened, rest, usednames}  A wiring containing just the
   *                   opened links, a wiring containing just the
   *                   original open links, and the set union of the
   *                   original usednames, and any fresh names.
   *)
  val splitopen : nameset -> wiring -> {opened : wiring,
                                        rest : wiring,
                                        usednames : nameset}
  (** Turn closed edges into links with fresh names.  Any fresh
   * names generated will not clash with outer names of w.
   * @params w usednames
   * @param w          Wiring to open.
   * @param usednames  Names not to use when generating fresh names.
   * @return {opened, newnames, usednames}
   *                   A wiring containing just open links,
   *                   the set of new names added, and the union
   *                   of the original usednames, and any fresh
   *                   names.
   *)
  val openup : nameset -> wiring -> {opened: wiring,
                                     newnames : nameset,
                                     usednames : nameset}
  (** Close some open links. *)
  val closelinks : nameset -> wiring -> wiring
  (** Determine whether some wiring is an identity. *)
  val is_id : wiring -> bool
  (** Determine whether some wiring is a zero identity. *)
  val is_id0 : wiring -> bool
  (** Determine whether some wiring can be written w = id_Y x sigma,
   * where sigma is a substitution (i.e., no closed links).
   * @params w Y
   *)
  val is_id_x_sigma : nameset -> wiring -> bool
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
  val pp : int -> ppstream -> wiring -> unit
end
