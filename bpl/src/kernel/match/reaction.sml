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

(** Functions for performing reactions.
 * Suggested precedence for infix operators:
 *  infixr 4 TIMES_DO                        <br />
 *  infixl 3 ++                              <br />
 *  infixl 2 ORTHEN                          <br />
 *  infixr 1 THEN                            <br />
 *  infixr 1 ELSE                            <br />
 * @version $LastChangedRevision$
 *)

functor Reaction (
  structure RuleNameMap : MONO_FINMAP
  where type dom = string
  structure Info : INFO
  structure Interface : INTERFACE
  structure Wiring : WIRING
  structure BgVal : BGVAL
  structure BgBDNF : BGBDNF
  structure Match : MATCH
  structure Instantiation : INSTANTIATION
  structure Rule : RULE
  structure Origin : ORIGIN
  structure ErrorHandler : ERRORHANDLER
      where type ppstream    = PrettyPrint.ppstream
        and type break_style = PrettyPrint.break_style
        and type origin      = Origin.origin
  sharing type Match.rule = Rule.rule
  sharing type Wiring.nameset = Interface.nameset
  sharing type Wiring.wiring = BgVal.wiring
  sharing type BgBDNF.bgbdnf =
               Match.bgbdnf =
               Instantiation.bgbdnf =
               Rule.bgbdnf
  sharing type BgBDNF.DR =
               Match.DR =
               Instantiation.DR
  sharing type BgBDNF.BR =
               Rule.BR =
               Match.BR
  sharing type BgVal.bgval =
               BgBDNF.bgval =
               Instantiation.bgval =
               Rule.bgval
  sharing type Instantiation.inst = Rule.inst
  sharing type Info.info =
               BgBDNF.info =
               BgVal.info = Origin.origin
  sharing type Interface.interface = BgBDNF.interface)
  :> REACTION
  where type rulename = string
         and type rule = Rule.rule
         and type 'a rulenamemap = 'a RuleNameMap.map
         and type match = Match.match
         and type bgval = BgVal.bgval
         and type 'a bgbdnf = 'a BgBDNF.bgbdnf
         and type BR = BgBDNF.BR =
struct
  open LazyList
  open Debug
  open ErrorHandler
  type rulename = string
  type 'a rulenamemap = 'a RuleNameMap.map
  type bgval  = BgVal.bgval
  type 'a bgbdnf = 'a BgBDNF.bgbdnf
  type BR        = BgBDNF.BR
  type agent     = bgval
  type match     = Match.match
  type rule      = Rule.rule
  type rules     = rule rulenamemap
  type matches   = match lazylist
  val noinfo     = Info.noinfo

  exception ThisCannotHappen

  datatype action
   = MATCH of match * ((rule * matches) rulenamemap -> action)
   | FINISH
   | FAIL

  type tactic = (rule * matches) rulenamemap -> action

  val Com = BgVal.Com noinfo
  val Ten = BgVal.Ten noinfo
  val Wir = BgVal.Wir noinfo

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/match/react.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  fun react match =
    let
      infix 6 o
      infix 5 *
      val op o = Com
      fun b1 * b2 = Ten [b1, b2]
      val {rule, context, parameter} = Match.unmk match
      val {name, redex, react, inst, info} = Rule.unmk rule
      val instantiation = Instantiation.instantiate inst parameter
      val Z = Interface.glob (BgBDNF.outerface parameter)
      val id_Z = Wir (Wiring.id_X Z)
      val context = BgBDNF.unmk context
      val newagent = context o (react * id_Z) o instantiation
    in
      newagent
    end

  fun mknamedrules nrs = 
    foldl
      (fn ((name, rule), map) => RuleNameMap.add (name, rule, map))
      RuleNameMap.empty
      nrs

  val mkrules =
    mknamedrules o map (fn rule => (#name (Rule.unmk rule), rule))

(*  fun run rules tactic agent =
    let
      fun addmatch rule
        = (rule, Match.matches {agent = agent, rule = rule})
      val rulematches = RuleNameMap.composemap addmatch rules
    in
      case tactic rulematches of
        MATCH (match, next_tactic)
      => run rules next_tactic (react match)
      | _ => agent
    end
*)

  (* steps' runs the rules on the agent using the tactics.
   * at each step, f will be called with the match, the
   * current agent in which the match occurs, and a thunk
   * that, upon activation, will return the result of 
   * the rest of the steps.  The value returned from f
   * is passed back via previous thunks.
   * Reaction stops when the 
   * tactics no longer return any matches, returning
   * g applied to the last agent back to previous thunks.
   *)
  fun steps' f g rules tactic agent =
    let
      val regagent = BgBDNF.regularize (BgBDNF.make agent)
      fun addmatch rule =
       (rule, Match.matches {agent = regagent, rule = rule})
      val rulematches = RuleNameMap.composemap addmatch rules
    in
      case tactic rulematches of
        MATCH (match, next_tactic)
      => f match agent (fn () => steps' f g rules next_tactic (react match))
      | _ => g agent
    end

  val run =
    steps'
      (fn _ => fn _ => fn t => t ())
      (fn a => BgVal.simplify (BgBDNF.unmk (BgBDNF.make a))) 

  val steps =
    steps'
     (fn m => fn a => (fn t =>
      case t () of
        [] => raise ThisCannotHappen
      | ((_, a') :: aas) =>
        ("<INITIAL>:", BgVal.simplify (BgBDNF.unmk (BgBDNF.make a))) ::
        (#name (Rule.unmk (#rule (Match.unmk m))) ^ ":", a') :: aas))
     (fn a => [("<FINAL>:", BgVal.simplify (BgBDNF.unmk (BgBDNF.make a)))])

  val stepz =
    steps'
     (fn m => fn a => (fn t => lzmake (fn () =>
        Cons (
          ("<INITIAL>:", BgVal.simplify (BgBDNF.unmk (BgBDNF.make a))),
          (lzmake (
            fn () =>
              case lzunmk (t ()) of
                Nil => raise ThisCannotHappen
              | Cons ((_, a'), t_aas) =>
                Cons ((
                    #name (Rule.unmk (#rule (Match.unmk m))) ^ ":",
                    a'), t_aas)))))))
     (fn a =>
       lzmake (fn () =>
         Cons (
           ("<FINAL>:", BgVal.simplify (BgBDNF.unmk (BgBDNF.make a))),
           lzNil)))

  fun matches rules agent =
    let
      val agent = BgBDNF.regularize (BgBDNF.make agent)
      fun toMatch rule = Match.matches {agent = agent, rule = rule}
    in
      lzmerge (map toMatch (RuleNameMap.range rules))
    end

  fun finish _ = FINISH    

  fun fail _ = FAIL

  local
	  fun react_rule_any' [] = FAIL
	    | react_rule_any' ((rule, mz) :: rmzs) =
	      case lzunmk mz of
	        Nil => react_rule_any' rmzs
	      | Cons (m, mz') => MATCH (m, finish)
  in
	  fun react_rule_any rulematches
	    = react_rule_any' (RuleNameMap.range rulematches)
  end
  
  exception WrongRuleName of rulename * rulename list
  fun explain_WrongRuleName (WrongRuleName (rulename, rulenames)) =
      [Exp (LVL_USER, Origin.unknown_origin,
            mk_string_pp ("Unknown rule name `" ^ rulename ^ "'"),
            [Exp (LVL_USER, Origin.unknown_origin,
                  mk_list_pp
                    "(Known names: " "." ","
                    (fn i => fn pps => fn s => mk_string_pp s i pps)
                    rulenames,
                  [])])]
    | explain_WrongRuleName _ = raise Match
  val _ = add_explainer
            (mk_explainer "Unknown rule name" explain_WrongRuleName)

  fun react_rule rulename rulematches =
    case RuleNameMap.lookup rulematches rulename of
      SOME (rule, mz)
    => (case lzunmk mz of
	        Nil => FAIL
	      | Cons (m, mz') => MATCH (m, finish))
	  | NONE
	  => raise WrongRuleName (rulename, RuleNameMap.dom rulematches)
	
	infix 3 ++
  fun (t1 ++ t2) rulematches =
    case t1 rulematches of
      MATCH (m, tnext) => MATCH (m, tnext ++ t2)
    | _ => t2 rulematches

  infix 2 ORTHEN
  fun TRY t = t
  fun (t1 ORTHEN t2) rulematches =
    case t1 rulematches of
      MATCH (m, tnext) => MATCH (m, tnext ORTHEN t2)
    | FAIL => t2 rulematches
    | FINISH => FINISH

  type ifbranches = {thent : tactic, elset : tactic}
  infixr 1 THEN
  infixr 1 ELSE
  fun IF t = t
  fun (t1 THEN {thent, elset}) rulematches =
    let
      fun runtest t1 rulematches =
        case t1 rulematches of
          MATCH (m, tnext) => MATCH (m, runtest tnext)
        | FINISH => thent rulematches
        | FAIL => elset rulematches
    in
      runtest t1 rulematches
    end
  fun thent ELSE elset = {thent = thent, elset = elset}

  fun REPEAT t rulematches = (IF t THEN (REPEAT t) ELSE fail) rulematches

  infixr 4 TIMES_DO
  fun 0 TIMES_DO t = finish
    | n TIMES_DO t = t ++ (n - 1) TIMES_DO t
  
  fun roundrobin rulematches =
    let
      fun next ruleindex rulematches =
        let
          val rmzs = RuleNameMap.range rulematches
          val i = if ruleindex < length rmzs then ruleindex else 0
          val rmzs = List.drop (rmzs, i) @ List.take (rmzs, i)
          fun findmatch [] = FINISH
            | findmatch ((rule, mz) :: rmzs)
            = case lzunmk mz of
                Nil => findmatch rmzs
              | Cons (m, mz') => MATCH (m, next (ruleindex + 1))
        in
          findmatch rmzs
        end
    in
      next 0 rulematches
    end
      
  (** Ask interactively which rule to apply.
   * @params ui
   * @param ui  User interface function that takes a list
   *            of prettyprinters and corresponding valid answers,
   *            prints the prettyprinter to the user,
   *            and returns one of the valid answers.
   *)
   (* NOT IMPLEMENTED YET! *)
  fun ask_user (ui : ((PrettyPrint.ppstream -> unit) * string) list -> string) =
    fail
    
  val revision =
    hd (String.tokens (not o Char.isDigit) "$LastChangedRevision$")
end
