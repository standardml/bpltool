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

(* Representation of the higher-order pi-calculus (without
 * restrictions) using binding bigraphs and explicit substitutions.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
*)

val cur_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir cur_dir;

(***********************************)
(*  Auxiliary string declarations  *)
(***********************************)

val (x,y,z,a) = ("x", "y", "z", "a")

(*******************************)
(*          Signature          *)
(*******************************)

val nilp     = "NilP"
val var      = "Var"
val send     = "Send"
val sendPro  = "SendPro"
val sendResi = "SendResi"
val receive  = "Receive"
val def      = "Def"
val sub      = "Sub"

val NilP     = atomic0 (nilp                  );
val Var      = atomic  (var         -:       1);
val Send     = passive (send        -:       1);
val SendPro  = passive0(sendPro               );
val SendResi = passive0(sendResi              );
val Receive  = passive (receive     =: 1 --> 1);
val Def      = passive (def         -:       1);
val Sub      = active  (sub         =: 1 --> 0);

(*******************************)
(*        Reaction rules       *)
(*******************************)

val rule_global_gc = 
    "global gc"     ::: Sub[][[x]] o (`[]` `|` (<[x]>Def[x]))
                           ----|>
                        `[]`


val rule_application =  
    "application"   ::: Send[a] o (SendPro `|` SendResi) `|` 
                        Receive[a][[x]] o (<[x]> `[x]`)
                           --[0 |-> 1, 1 |-> 2, 2 |-> 0]--|>
                        `[]` `|` Sub[][[x]] o (<[x]> `[x]` `|` 
                           Def[x]) * a//[]


(* We cannot have propagation with var, as this might introduce problems *)
(* with divergence *)
val rule_app_var =
    "application variable" ::: Sub[][[x]] o (<[x]>Var[x] `|` `[x]` `|` Def[x])
                                  --[0 |-> 1, 1 |-> 0, 2 |-> 1]--|>
                               Sub[][[x]] o (<[x]> `[]` `|` `[x]` `|` Def[x])


val rule_prop_recei =
    "propagation receive"  ::: Sub[][[z]] o (<[z]> z//[z,y] o
                                  (Receive[a][[x]] o (<[x]> `[x,z]`)
                                   `|` `[y]`
                                   `|` Def[z]
                                  )
                               )
                                  --[0 |-> 0, 1 |-> 2, 2 |-> 1, 3 |-> 2]--|>
                               Receive[a][[x]] o (<[x]> Sub[][[z]] o (
                                  <[z]> `[x,z]` `|` Def[z]))
                               `|`
                               Sub[][[y]] o (<[y]> `[y]` `|` Def[y])


val rule_prop_send =
    "propagation send"  ::: Sub[][[x]] o (<[x]> x//[x,z,y] o
                               (Send[a] o (
                                  (SendPro o  `[x]`) 
                                   `|` (SendResi o `[z]`)
                               ) 
                               `|` `[y]` 
                               `|` Def[z])
                            )
                               --[0 |-> 0, 1 |-> 3, 2 |-> 1, 3 |-> 3, 4 |-> 2, 5 |-> 3]--|>
                            Send[a] o (
                               (SendPro o Sub[][[x]] o ( <[x]> `[x]` `|` Def[x]) )
                               `|` 
                               (SendResi o Sub[][[z]] o ( <[z]> `[z]` `|` Def[z]) )
                            )
                            `|`
                            Sub[][[y]] o (<[y]> `[y]` `|` Def[y])

(* This rule is actually subsumed by rule_global_gc, I think ??? *)
(* The case where the hole is filled with the empty bigraph *)
(*val rule_prop_empty =
    "propagation empty"  ::: Sub[][[x]] o (<[x]> Def[x] )
                                ----|>
                             <->
*)

val rules =
    mkrules [rule_global_gc, rule_application, rule_app_var, 
             rule_prop_recei, rule_prop_send]; (*, rule_prop_empty];*)

val tactic = roundrobin;


(*******************************)
(*       Example processes     *)
(*******************************)

val dummy = "Dummy"
val Dummy     = atomic0 (dummy                    );

(* Represent a simple copy-process a => a(x).(x || x) *)
val copy = fn a => Receive[a][[x]] o (<[x]> Var[x] `|` Var[x]) 
val sender = fn a => Send[a] o (SendPro o Dummy `|` SendResi o NilP) 

val system = fn a => (copy a `|` sender a)
val system2 = (system "a") `|` (system "a")

(* val ms = matches rules (system "a") *)

(* val _ = print_mv ms *)

val c = "c"
val q = "Q"
val Q     = atomic0 (q                    );


val agent1 = Send[a] o (SendPro o (Send[c] o (SendPro o Q `|` SendResi o NilP)) `|` SendResi o (Receive[c][[x]] o (<[x]> Var[x])  `|` Receive[c][[z]] o (<[z]> (NilP * z//[]))))
val agent2 = Receive[a][[y]] o (<[y]> Var[y] `|` Var[y]) 

val myrun = run rules tactic;
val mysteps = steps rules tactic;


