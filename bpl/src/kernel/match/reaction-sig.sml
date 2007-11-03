(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** Reaction and tactic functions.
 * Suggested precedence for infix operators:
 * <pre>
 *  infixr 4 TIMES_DO
 *  infix  3 ++
 *  infix  2 ORTHEN
 *  infixr 1 THEN
 *  infixr 1 ELSE
 * </pre>
 * @version $LastChangedRevision$
 *)

signature REACTION =
sig
  type 'a lazylist = 'a LazyList.lazylist
  type 'a bgbdnf
  type bgval
  type BR
  type agent = bgval
  type match
  type rulename
  type 'a rulenamemap
  type rule
  type rules = rule rulenamemap
  type matches = match lazylist

  (** Action type: either perform a reaction based on a match,
   * followed by another tactic, or finish with current agent.
   *)
  datatype action
   = MATCH of match * ((rule * matches) rulenamemap -> action)
   | FINISH
   | FAIL

  (** Tactic type: Given a set of rules with their matches,
   * determine the next action.
   *)
  type tactic = (rule * matches) rulenamemap -> action

  (** Perform a single reaction step induced by a match. *)  
  val react : match -> agent

  (** Construct a rule map. *)
  val mknamedrules : (rulename * rule) list -> rules

  (** Construct a rule map, using default names. *)
  val mkrules : rule list -> rules

  (** Return a lazy list of all matches for all rules.
   * The matches are ordered in a round-robin fashion
   * with respect to the list of reaction rules.
   * @params R t a
   * @param R  The reaction rules.
   * @param a  The agent within which to match.
   * @return   The resulting lazy list of matches.
   *)
  val matches : rules -> agent -> matches

  (** Run a system of reaction rules, using a tactic, on an agent.
   * @params R t a
   * @param R  The reaction rules.
   * @param t  The tactic to use.
   * @param a  The initial agent to use.
   * @return   The resulting agent.
   *)
  val run : rules -> tactic -> agent -> agent

  (** Return the steps (applied rules on which agents)
   * resulting from running a system
   * of reaction rules, using a tactic, on an agent.
   * @params R t a
   * @param R  The reaction rules.
   * @param t  The tactic to use.
   * @param a  The initial agent to use.
   * @return   The resulting steps.
   *)
  val steps : rules -> tactic -> agent -> (rulename * agent) list

  (** Return the steps (applied rules on which agents)
   * resulting from running a system
   * of reaction rules, using a tactic, on an agent.
   * @params R t a
   * @param R  The reaction rules.
   * @param t  The tactic to use.
   * @param a  The initial agent to use.
   * @return   The resulting steps.
   *)
  val stepz : rules -> tactic -> agent -> (rulename * agent) lazylist

  (** Finish tactic. *)
  val finish : tactic
  (** Fail tactic. *)
  val fail : tactic
  (** Apply any rule that matches; fail if none match. *)
  val react_rule_any : tactic
  (** Apply a named rule; fail if it does not match. *)
  val react_rule : rulename -> tactic
  (** Apply t1, followed by t2, even if t1 fails. *)
  val ++ : (tactic * tactic) -> tactic
  (** TRY t1 ORTHEN t2:  If t1 fails, apply t2 to its result. *)
  val TRY : tactic -> tactic
  val ORTHEN : (tactic * tactic) -> tactic
  (** IF t1 THEN t2 ELSE t3:
   * If t1 finishes, apply t2 to its result,
   * else apply t3 to its result.
   *)
  type ifbranches
  val IF : tactic -> tactic
  val THEN : (tactic * ifbranches) -> tactic
  val ELSE : (tactic * tactic) -> ifbranches
  (** Repeat tactic until fail. *)
  val REPEAT : tactic -> tactic
  (** Apply tactic a given number of times. *)
  val TIMES_DO : (int * tactic) -> tactic
  (** Apply the rules repeatedly in a round-robin fashion
   * until none match.
   * @return FINISH
   *)
  val roundrobin : tactic
  (** Ask interactively which rule to apply.
   * @params ui
   * @param ui  User interface function that takes a list
   *            of prettyprinters and corresponding valid answers,
   *            prints the prettyprinter to the user,
   *            and returns one of the valid answers.
   *)
  val ask_user : (((PrettyPrint.ppstream -> unit) * string) list -> string) -> tactic
  (** Revision number.*)
  val revision : string
end
