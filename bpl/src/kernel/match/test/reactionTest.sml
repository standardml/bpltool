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

(** Testing module for bdnf stuff.
 * @version $LastChangedRevision: 953 $
 *)

functor ReactionTest (
  structure Assert : ASSERT
	structure Test   : TEST
	structure ErrorHandler : ERRORHANDLER
	  where type ppstream    = PrettyPrint.ppstream
      and type break_style = PrettyPrint.break_style
      and type origin      = Origin.origin) =
struct

open LazyList
structure BGADT :> BG_ADT = BGADT (structure ErrorHandler = ErrorHandler)
              
open BGADT
open Reaction
open Assert

datatype 'a expected = HAS of 'a | JUST of 'a

local
  open Sugar
  infix 7 /   infix 7 //
  infix 6 o
  infix 5 *   infix 5 ||   infix 5 `|`
  infix 4 >
  infix 3 &   infix 3 -->  infix 3 --    infix 3 --|>  infix 3----|>
  infix 2 =:  infix 2 -:   infix 2 |->   infix 2 |-->  infix 2 :::
  nonfix @
  nonfix <
  infixr 4 TIMES_DO
  infix  3 ++
  infix  2 ORTHEN
  infixr 1 THEN
  infixr 1 ELSE

  val Sum     = passive0 ("Sum")
	val Send0   = passive  ("Send0" -: 1)
	val Get0    = passive  ("Get0"  =: 0 --> 1)
	val Send2   = passive  ("Send2" -: 3)
	val Get2    = passive  ("Get2"  =: 2 --> 1)
	val (x,  y1,  y2,  z1,  z2) =
	   ("x","y1","y2","z1","z2")
	
  val REACT0 = "REACT0" :::
    Sum o (Send0[x] `|` idp(1)) `|` Sum o (Get0[x][] `|` idp(1))
    --[0 |-> 0, 1 |-> 2]--|>
    x//[] * idp(2)
	
  val REACT2 = "REACT2" :::
        Sum o (Send2[x,y1,y2]     `|` idp(1))
    `|` Sum o (Get2[x][[z1],[z2]] `|` idp(1))
    --[0 |-> 0, 1 |-> 2]--|>
    (x//[] * y1/z1 * y2/z2 * idp(1)) o (idp(1) `|` `[z1,z2]`)
	
	val Car     = atomic   ("Car"     -: 2)
	val Control = atomic   ("Control" -: 8)
	val Trans   = atomic   ("Trans"   -: 4)
	val Idtrans = atomic   ("Idtrans" -: 2)

  val (switch1,  talk1,  lose1,  gain1) =
    ( "switch1","talk1","lose1","gain1")
  val (switch2,  talk2,  lose2,  gain2) =
    ( "switch2","talk2","lose2","gain2")
  val ( talk,  switch,  gain,  lose,  t,  s)
    = ("talk","switch","gain","lose","t","s")

  val DEF_Car = "DEF_Car" ::: 
    Car[talk,switch]
    ----|>
    Sum o (Send0[talk] o Car[talk,switch]
           `|` Get2[switch][[t],[s]] o (<[t,s]> Car[t,s]))

  val DEF_Trans = "DEF_Trans" :::
    Trans[talk,switch,gain,lose]
    ----|>
    Sum o (Get0[talk][] o Trans[talk,switch,gain,lose]
           `|` Get2[lose][[t],[s]]
               o (<[t,s]> Sum o Send2[switch,t,s] o Idtrans[gain,lose]))

  val DEF_Idtrans = "DEF_Idtrans" :::
    Idtrans[gain, lose]
    ----|>
    Sum o Get2[gain][[t],[s]] o (<[t,s]> Trans[t,s,gain,lose])

  val DEF_Control = "DEF_Control" :::
    Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1]
    ----|>
    Sum o Send2[lose1,talk2,switch2] o Sum o Send2[gain2,talk2,switch2]
    o Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2]
  
  val rules = [REACT0, REACT2, DEF_Car, DEF_Trans, DEF_Idtrans, DEF_Control]
  
  val TAC_unfold =
    react_rule "DEF_Car"     ++ react_rule "DEF_Trans"   ++
    react_rule "DEF_Idtrans" ++ react_rule "DEF_Control"
  val TAC_talk =
    react_rule "REACT0"         (* Car talks.                     *)
  val TAC_switch =
    react_rule "REACT2"      ++ (* Control tells Trans to lose.   *)
    react_rule "REACT2"      ++ (* Control tells Idtrans to gain. *)
    react_rule "REACT2"         (* Trans tells Car to switch.     *)
  
  
in
  (** The list of tests.  Each entry consists of:
   * A label describing the test,
   * Agent and redex,
   * A list of matches.
   * 
   * An attempt to match the redex in the agent will
   * be performed, and all the given matches must appear
   * in the resulting list of matches for the test to
   * succeed.
   *)
  val tests = [
    ("Pi book mobile phone example with open agent links",
     {agent =
          Car[talk1,switch1]
      `|` Trans[talk1,switch1,gain1,lose1]
      `|` Idtrans[gain2,lose2]
      `|` Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1],
      rules = mkrules rules,
      tactic = TAC_unfold ++ TAC_switch,
      result = 
          Car[talk2,switch2]
      `|` Trans[talk2,switch2,gain2,lose2]
      `|` Idtrans[gain1,lose1]
      `|` Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2]}),
    ("Pi book mobile phone example with closed agent edges",
     {agent = (-//[talk1,switch1,gain1,lose1,talk2,switch2,gain2,lose2] * idp(1)) o
        (    Car[talk1,switch1]
         `|` Trans[talk1,switch1,gain1,lose1]
         `|` Idtrans[gain2,lose2]
         `|` Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1]),
      rules = mkrules rules,
      tactic = TAC_unfold ++ TAC_switch,
      result = (-//[talk1,switch1,gain1,lose1,talk2,switch2,gain2,lose2] * idp(1)) o
        (  Car[talk2,switch2]
         `|` Trans[talk2,switch2,gain2,lose2]
         `|` Idtrans[gain1,lose1]
         `|` Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2])})
    ]
end
  
  fun inst_id redex = Instantiation.make
    {I = BgVal.innerface redex,
     J = BgVal.innerface redex,
     maps = []}
  
  fun mkTest (label, {agent, rules, tactic, result}) =
    let
      val foundresult = run rules tactic agent
      val bdnfresult = BgBDNF.make result
      val bdnffoundresult = BgBDNF.make foundresult
    in
      (label, fn () =>
      if BgBDNF.eq bdnfresult bdnffoundresult then
        ()
      else
        Assert.fail
         ("unexpected result; expected\n"
          ^ BgBDNF.toString bdnfresult
          ^ "\nbut got\n" ^ BgBDNF.toString bdnffoundresult))
    end

  val suite = (fn () => Test.labelTests (map mkTest tests))
end
