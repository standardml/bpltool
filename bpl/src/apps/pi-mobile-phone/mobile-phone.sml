(* Model of the polyadic π calculus, running the mobile phone system introduced
 * in Milner’s pi book.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

SMLofNJ.Internals.GC.messages false;
val cur_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir cur_dir;
(*use "figinfo.sml";*)

(*Flags.setBoolFlag "/kernel/ast/bgval/pp-simplify" true;*)

val Sum   = passive0 ("Sum")            
val Send0 = passive  ("Send0" -: 0  +  1)  
val Get0  = passive  ("Get0"  -: 0  +  1)
val Send2 = passive  ("Send2" -: 2  +  1)  
val Get2  = passive  ("Get2"  =: 2 --> 1)

val (x,  y1,  y2,  z1,  z2) =
   ("x","y1","y2","z1","z2")

val REACT0 = "REACT0" :::
  Sum o (Send0[x] `|` `[]`) `|` Sum o (Get0[x] `|` `[]`)
  --[0 |-> 0, 1 |-> 2]--|>
  x//[] `|` `[]` `|` `[]`

val REACT2 = "REACT2" :::
      Sum o (Send2[x,y1,y2]     `|` `[]`)
  `|` Sum o (Get2[x][[z1],[z2]] `|` `[]`)
  --[0 |-> 0, 1 |-> 2]--|>
  (x//[] * y1/z1 * y2/z2 * `[]`) o (`[]` `|` `[z1,z2]`)
(* simpler alternative 
val REACT2 = "REACT2" :::
      Sum o (Send2[x,y1,y2]     `|` `[]`)
  `|` Sum o (Get2[x][[z1],[z2]] `|` `[]`)
  --[1&[y1,y2] |--> 2&[z1,z2]]--|>
  x//[] o (`[]` `|` `[y1,y2]`)
*)

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

val System1 = 
      Car[talk1,switch1] 
  `|` Trans[talk1,switch1,gain1,lose1]
  `|` Idtrans[gain2,lose2]
  `|` Control[lose1,talk2,switch2,gain2,
              lose2,talk1,switch1,gain1]

(*
val DEF_Car = "DEF_Car" ::: 
  Car[talk,switch]
  ----|>
  Sum o (    Send0[talk] o Car[talk,switch]
         `|` Get2[switch][[t],[s]] o (<[t,s]> Car[t,s]))
alternative*)
val DEF_Car = "DEF_Car" ::: 
  Car[talk,switch]
  ----|>
  Sum o (    Send0[talk] o Car[talk,switch]
         `|` Get2[switch][[t],[s]] o Car[t,s])

(*
val DEF_Trans = "DEF_Trans" :::
  Trans[talk,switch,gain,lose]
  ----|>
  Sum o (    Get0[talk] o Trans[talk,switch,gain,lose]
         `|` Get2[lose][[t],[s]]
             o (<[t,s]> Sum o Send2[switch,t,s] o Idtrans[gain,lose]))
alternative *)
val DEF_Trans = "DEF_Trans" :::
  Trans[talk,switch,gain,lose]
  ----|>
  Sum o (    Get0[talk] o Trans[talk,switch,gain,lose]
         `|` Get2[lose][[t],[s]]
             o Sum o Send2[switch,t,s] o Idtrans[gain,lose])

(*
val DEF_Idtrans = "DEF_Idtrans" :::
  Idtrans[gain, lose]
  ----|>
  Sum o Get2[gain][[t],[s]] o (<[t,s]> Trans[t,s,gain,lose])
alternative *)
val DEF_Idtrans = "DEF_Idtrans" :::
  Idtrans[gain, lose]
  ----|>
  Sum o Get2[gain][[t],[s]] o Trans[t,s,gain,lose]

val DEF_Control = "DEF_Control" :::
  Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1]
  ----|>
  Sum o Send2[lose1,talk2,switch2] o Sum o Send2[gain2,talk2,switch2]
  o Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2]

(*val rules = [REACT0, REACT2, DEF_Car, DEF_Trans, DEF_Idtrans, DEF_Control]*)

val rules = mkrules [REACT0, REACT2, DEF_Car, DEF_Trans, DEF_Idtrans, DEF_Control]

val _ = print_mv (matches rules System1)

val TAC_unfold =
  react_rule "DEF_Car"     ++ react_rule "DEF_Trans"   ++
  react_rule "DEF_Idtrans" ++ react_rule "DEF_Control"

val System1_unfolded = run rules TAC_unfold System1

val _ = print_mv (matches rules System1_unfolded)

val TAC_switch =
  react_rule "REACT2"      ++ (* Control tells Trans to lose.   *)
  react_rule "REACT2"      ++ (* Control tells Idtrans to gain. *)
  react_rule "REACT2"         (* Trans tells Car to switch.     *)

val System2 = run rules TAC_switch System1_unfolded

val TAC_talk =
  react_rule "REACT0"         (* Car talks.                     *)


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
   {agent = (-//[talk1,switch1,gain1,lose1,talk2,switch2,gain2,lose2] * `[]`) o
      (    Car[talk1,switch1]
       `|` Trans[talk1,switch1,gain1,lose1]
       `|` Idtrans[gain2,lose2]
       `|` Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1]),
    rules = mkrules rules,
    tactic = TAC_unfold ++ TAC_switch,
    result = (-//[talk1,switch1,gain1,lose1,talk2,switch2,gain2,lose2] * `[]`) o
      (  Car[talk2,switch2]
       `|` Trans[talk2,switch2,gain2,lose2]
       `|` Idtrans[gain1,lose1]
       `|` Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2])})
  ]
  
fun mkTest (label, {agent, rules, tactic, result}) =
  let
    val foundresult = run rules tactic agent
    val bdnfresult = (*BgBDNF.make*) norm_v result
    val bdnffoundresult = (*BgBDNF.make*) norm_v foundresult
  in
    (label, fn () =>
    if BG.BgBDNF.eq bdnfresult bdnffoundresult then
      (  println "got expected result:"
       ; print_v result)
    else
      print 
       ("unexpected result; expected\n"
        ^ BG.BgBDNF.toString bdnfresult
        ^ "\nbut got\n" ^ BG.BgBDNF.toString bdnffoundresult))
  end

val suite = map mkTest tests;

fun println s = (print s; print "\n");

val _ = (  println "running suite:"
         ; app (fn (label, f) => (print "  "; println label; print "    "; f (); println ""; println "")) suite);
