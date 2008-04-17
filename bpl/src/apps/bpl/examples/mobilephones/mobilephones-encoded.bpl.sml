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
 
 (* Mobile phones example from Robin Milner:
 * "Communicating and Mobile Systems: the Pi Calculus,"
 * Cambridge University Press, 1999.
 *)

use "polyadic-pi-encoded.bpl.sml";
Flags.setIntFlag "/debug/level" 10;
SMLofNJ.Internals.GC.messages false;
use_shorthands true;

(* Pi calculus reaction rules *)
val pirules = [REACTinit, REACTstop, REACTloop]

(* Components controls *)
val Car      = atomic ("Car" -: 2)
val Trans    = atomic ("Trans" -: 4)
val Idtrans  = atomic ("Idtrans" -: 2)
val Control  = atomic ("Control" -: 8)

(* System link names *)
val ( talk1,  talk2,  switch1,  switch2,  t,  s,  gain1,  lose1,  gain2,  lose2 )
  = ("talk1","talk2","switch1","switch2","t","s","gain1","lose1","gain2","lose2")

(* Component definition rules *)
val ( talk,  switch,  gain,  lose )
  = ("talk","switch","gain","lose")

val DEF_Car = "DEF_Car" ::: 
  Car[talk,switch]
  ----|>
  Sum o (Send[talk] o Car[talk,switch]
         `|` 
         Get[switch] o GetName[][[t]] o 
           (<[t]> GetName[][[s]] o 
             (<[s]> Car[t,s]))) handle e => explain e;
(*  Sum o (Send0[talk] o Car[talk,switch]
         `|` Get2[switch][[t],[s]] o (<[t,s]> Car[t,s]))*)


val DEF_Trans = "DEF_Trans" :::
  Trans[talk,switch,gain,lose]
  ----|>
  Sum o (Get[talk] o Trans[talk,switch,gain,lose]
         `|` Get[lose] o GetName[][[t]] o 
           (<[t]> GetName[][[s]] o 
             (<[s]> Sum o Send[switch] o 
               (SendName[t] o
                  (SendName[s] o Idtrans[gain,lose])))))
(*  Sum o (Get0[talk][] o Trans[talk,switch,gain,lose]
         `|` Get2[lose][[t],[s]]
             o (<[t,s]> Sum o Send2[switch,t,s] o Idtrans[gain,lose]))*)


val DEF_Idtrans = "DEF_Idtrans" :::
  Idtrans[gain, lose]
  ----|>
  Sum o Get[gain] o 
          GetName[][[t]] o 
           (<[t]> GetName[][[s]] o 
             (<[s]> Trans[t,s,gain,lose]))
(*  Sum o Get2[gain][[t],[s]] o (<[t,s]> Trans[t,s,gain,lose])*)


val DEF_Control = "DEF_Control" :::
  Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1]
  ----|>
  Sum o Send[lose1] o SendName[talk2] o SendName[switch2] o 
    Sum o Send[gain2]  o SendName[talk2] o SendName[switch2] o 
      Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2]
(*  Sum o Send2[lose1,talk2,switch2] o Sum o Send2[gain2,talk2,switch2]
  o Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2]*)


val defrules = [DEF_Car, DEF_Trans, DEF_Idtrans, DEF_Control]

(* All rules *)
val rules = mkrules (List.@ (defrules, pirules))

(* Tactics *)
val TAC_unfold =
  react_rule "DEF_Car"     ++ react_rule "DEF_Trans"   ++
  react_rule "DEF_Idtrans" ++ react_rule "DEF_Control"
val TAC_reactinit = react_rule "REACTinit";
val TAC_reactstop = react_rule "REACTstop";
val TAC_reactloop = react_rule "REACTloop";
val TAC_react = IF   (react_rule "REACTinit")
                THEN (REPEAT (react_rule "REACTloop")
                      ++
                      (react_rule "REACTstop"))
                ELSE fail;

(* System *)
val System1 = simplify (
       Car[talk1,switch1] 
   `|` Trans[talk1,switch1,gain1,lose1]
   `|` Idtrans[gain2,lose2]
   `|` Control[lose1,talk2,switch2,gain2,
               lose2,talk1,switch1,gain1])

val System1_unfolded = run rules TAC_unfold System1;

val mz  = matches rules System1_unfolded;
val mz1 = lzhd mz;
val mz2 = lzhd (lztl mz);
(*print_mv(mz);*)

(*
(*CAR TALKING*)
val System2 = simplify(react mz1);
(*print_mv(matches rules System2);*)
val System2_step1 = run rules TAC_reactstop System2;
(*print_mv(matches rules System2_step1);*)
*)

(*SWITCHING*)
val System3 = simplify(react mz2);
(*print_mv(matches rules System3);*)
val System3_step1 = run rules TAC_reactloop System3;
(*print_mv(matches rules System3_step1);*)
val System3_step2 = run rules TAC_reactloop System3_step1;
(*print_mv(matches rules System3_step2);*)
val System3_step3 = run rules TAC_reactstop System3_step2;
(*print_mv(matches rules System3_step3);*)
(*
val System3_step4 = run rules TAC_reactinit System3_step3;
(*print_mv(matches rules System3_step4);*)
val System3_step5 = run rules TAC_reactloop System3_step4;
(*print_mv(matches rules System3_step5);*)
val System3_step6 = run rules TAC_reactloop System3_step5;
(*print_mv(matches rules System3_step6);*)
val System3_step7 = run rules TAC_reactstop System3_step6;
(*print_mv(matches rules System3_step7);*)
*)
val System3_step7 = run rules TAC_react System3_step3;
(*
val System3_step8 = run rules TAC_reactinit System3_step7;
(*print_mv(matches rules System3_step8);*)
val System3_step9 = run rules TAC_reactloop System3_step8;
(*print_mv(matches rules System3_step9);*)
val System3_step10= run rules TAC_reactloop System3_step9;
(*print_mv(matches rules System3_step10);*)
val System3_step11= run rules TAC_reactstop System3_step10;
(*print_mv(matches rules System3_step11);*)
*)
val System3_step11= run rules TAC_react System3_step7;
