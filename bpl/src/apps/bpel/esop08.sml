(* Formalization of a subset of BPEL using Binding Bigraphical Reactive
 * Systems in BPLtool.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

val cur_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir cur_dir;


(*******************************)
(*          Signature          *)
(*******************************)
(* The binding port of a process is used to delimit the scope of
 * variables (and variable access) within the process to the process
 * itself.
 * The free port should be connected to name of the process. *)
val Process   = passive ("process"   =: 1 --> 1);

(* The free port of a 'running' or 'stopped' node should be connected to
 * the scope port of the parent process/instance.
 *)
val Running   = atomic  ("running"   =: 0 --> 1);
val Stopped   = atomic  ("stopped"   =: 0 --> 1);

val Variables = passive ("variables" =: 0 --> 0);
(* The first free port of a variable should be connected to its name,
 * and the second should be connected to the scope port of the node
 * delimiting its scope. *)
val Variable  = passive ("variable"  =: 0 --> 2);

val Sequence  = active  ("sequence"  =: 0 --> 0);
val Next      = passive ("next"      =: 0 --> 0);

val Flow      = active  ("flow"      =: 0 --> 0);

val While     = passive ("while"     =: 0 --> 0);

val If        = passive ("if"        =: 0 --> 0);
val Condition = passive ("condition" =: 0 --> 0);
val Then      = passive ("then"      =: 0 --> 0);
val Else      = passive ("else"      =: 0 --> 0);
val True      = atomic  ("true"      =: 0 --> 0);
val False     = atomic  ("false"     =: 0 --> 0);

val Assign    = passive ("assign"    =: 0 --> 0);
val Copy      = passive ("copy"      =: 0 --> 0);
(* The first free port of a 'to' or 'from' node should be connected to a
 * variable name, and the second should be connected to the scope port of
 * the node delimiting its scope. *)
val To        = atomic  ("to"        =: 0 --> 2);
val From      = atomic  ("from"      =: 0 --> 2);

(* The free ports of an 'invoke' node should be connected:
 * 
 *   #1 to the name of the operation to be invoked
 *   #2 to the name of the parameter variable
 *   #3 to the same scope port as the parameter variable
 *)
val Invoke    = atomic  ("invoke"    =: 0 --> 3);
(* The free ports of an 'receive' node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the scope port of the enclosing process
 *   #3 to the name of the formal parameter variable
 *   #4 to the same scope port as the formal parameter variable
 *)
val Receive   = atomic  ("receive"   =: 0 --> 4);
(* NB! How is the link between the invoking and invoked process
 *     represented? *)
(* The free ports of an 'reply' node should be connected:
 * 
 *   #1 to the name of the operation to be invoked
 *   #2 to the name of the parameter variable
 *   #3 to the same scope port as the parameter variable
 *)
val Reply     = atomic  ("reply"     =: 0 --> 4);

(* The free port should be connected to the scope port of the
 * enclosing process. *)
val Exit      = atomic  ("exit"      =: 0 --> 1);

(* The first free port of an instance is used to delimit the scope of
 * variables (and variable access) within the instance to the instance
 * itself.
 * The second free port should be connected to name of the process. *)
val Instance  = active  ("instance"  =: 0 --> 2);



(*******************************)
(*        Reaction rules       *)
(*******************************)
(* Structural activities *)
val rule_flow_completed     =
    "flow completed"     ::: Flow[][] o <-> ----|> <->;
val rule_sequence_completed =
    "sequence completed" ::: Sequence[][] o Next[][] ----|> `[]`;
val rule_if_true            =
    "if true"            ::: If[][] o (Condition[][] o True[][]
                                       `|` Then[][] `|` Else[][])
                             ----|>
                             `[]`;
val rule_if_false           =
    "if false"           ::: If[][] o (Condition[][] o False[][]
                                       `|` Then[][] `|` Else[][])
                             --[0|->1]--|>
                             `[]`;
val rule_while_unfold       =
    "while unfold"       ::: While[][] o (Condition[][] `|` `[]`)
                             --[2|->0, 3|->1]--|>
                             If[][]
                              o (Condition[][]
                                 `|` Then[][] o Sequence[][]
                                     o (`[]` 
                                        `|` Next[][]
                                            o While[][] o (Condition[][]
                                                           `|` `[]`))
                                 `|` Else[][] o <->);

(* Variable assignment *)
val rule_variable_copy =
    "variable copy"      ::: (Assign[][] o Copy[][]
                              o (From["f", "scope"][]
                                 `|` To["t", "scope'"][]))
                             || Variable["f", "scope"][]
                             || Variable["t", "scope'"][]
                             --[1|->0]--|>
                             <->
                             || Variable["f", "scope"][]
                             || Variable["t", "scope'"][];


(*******************************)
(*       Example processes     *)
(*******************************)

