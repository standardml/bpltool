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

(** Abstract data type for modelling a set of constraints on a partial
 * bijection b on a set U. The constraints are simply pairs (S,T) with
 * S,T \subseteq U  and  |S| = |T|  to be interpreted as "b restricted
 * to S must be a total bijection between S and T".
 *
 * @version $LastChangedRevision$
 *)
signature BIJECTION_CONSTRAINTS =
sig
  (** The type of the domains and codomains of the bijections. *)
  type set
  (** A set of constraints. *)
  type constraints
  (** The empty set of constraints. *)
  val empty : constraints
  (** Signal that a given constraint is invalid, i.e. that the sets S
   * and T have different sizes.
   *
   * @params S T
   * @param S  The domain set.
   * @param T  The codomain set.
   *)
  exception InvalidConstraint of set * set
  (** Signal that a constraint (S, T) overlaps with the domain and/or codomain
   * of a constraint set.
   *
   * @params C S T
   * @param C  The constraint set.
   * @param S  The domain set.
   * @param T  The codomain set.
   *)
  exception Overlap of constraints * set * set
  (** Add the constraint (S, T) to the set of constraints C.
   * (S \cap dom(C)) = Ø  and  (T \cap rng(C)) = Ø  is required
   * 
   * @params S T C
   * @param S  The domain set.
   * @param T  The codomain set.
   * @param C  The constraint set.
   * @exception InvalidConstraint  if |S| <> |T|.
   * @exception Overlap            if the domain and/or codomain of the new
   *                               constraint overlaps with the domain and/or
   *                               codomain of the constraint set.
   *)
  val add : (set * set) * constraints -> constraints
  (** Add a list of constraints [(S1, T1), ..., (Sn, Tn)] to the set of
   * constraints C.
   * (Si \cap dom(C)) = Ø  and  (Ti \cap rng(C)) = Ø  is required and
   * the Sis should be pairwise disjoint (ditto for the Tis).
   * 
   * @params cs C
   * @param cs  the list of constraints.
   * @param C   The constraint set.
   * @exception InvalidConstraint  if |Si| <> |Ti| for some i.
   * @exception Overlap            if the domain and/or codomain of two
   *                               constraints or C overlap.
   *)
  val add_list : (set * set) list * constraints -> constraints
  (** Create a constraint set from a list of constraints.
   * 
   * @params cs
   * @param cs  the list of constraints.
   * @exception InvalidConstraint  if |Si| <> |Ti| for some i.
   * @exception Overlap            if the domain and/or codomain of two
   *                               constraints overlap.
   *)
  val from_list : (set * set) list -> constraints
  (** Signal that two sets of constraints are incompatible, e.g. they
   * have overlapping (plus) or  different (combine) domains and/or codomains.
   * 
   * @params C1 C2
   * @param C1  The first constraint set.
   * @param C2  The second constraint set.
   *)
  exception IncompatibleConstraints of constraints * constraints
  (** Create the union of the two set of constraints.
   *
   * @params C1 C2
   * @param C1  The first constraint set.
   * @param C2  The second constraint set.
   * @exception IncompatibleConstraints  if the constraint sets have overlapping
   *                                     domains and/or codomains.
   *)
  val plus : constraints * constraints -> constraints
  (** Combine two sets of constraints into a single set of constraints
   * that is stronger than the inputs. I.e. any bijection that satisfies
   * the result also satisfies each of the inputs.
   * Note that this means that the domain and codomain of input constraints
   * must be the same - if this is not the case, an exception is raised.
   *
   * If the two sets of constraints cannot be satisfied simultaneously,
   * the function returns NONE.
   *
   * @params C1 C2
   * @param C1  The first constraint set.
   * @param C2  The second constraint set.
   * @exception IncompatibleConstraints  if the constraint sets have different
   *                                     domains and/or codomains.
   *)
  val combine : constraints * constraints -> constraints option
  (** Decide whether combine will return NONE or SOME _ and return false or
   * true respectively.
   *
   * @params C1 C2
   * @param C1  The first constraint set.
   * @param C2  The second constraint set.
   * @exception IncompatibleConstraints  if the constraint sets have different
   *                                     domains and/or codomains.
   *)
  val are_combineable : constraints * constraints -> bool
  (** Signal either that dom and rng has different sizes or that dom is not
   * a subset of the domain of the constraint set or similarly for rng.
   *
   * @params C dom rng
   * @param C    the constraint set.
   * @param dom  the restricted domain.
   * @param rng  the restricted codomain.
   *)
  exception InvalidRestriction of constraints * (set * set)
  (** Restrict a set of constraints to a given range and domain.
   * If the constraints cannot be satisfied by any bijection between dom and rng
   * then NONE is returned.
   *
   * @params C dom rng
   * @param C    the constraint set.
   * @param dom  the restricted domain.
   * @param rng  the restricted codomain.
   * @exception InvalidRestriction  @see InvalidRestriction
   *)
  val restrict : constraints * (set * set) -> constraints option
end
