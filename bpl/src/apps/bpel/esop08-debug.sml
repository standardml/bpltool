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
(* The binding port of a Process is used to delimit the scope of
 * variables within the process to the process itself.
 * The free port should be connected to name of the process. *)
val Process     = passive0  ("Process");

(* The first free port should be connected to name of the process.
 * The second free port of an instance is the instance identifier
 * which (among other things) is used to determine the scope of
 * variables within the instance to the instance itself. *)
val Instance    = active0   ("Instance");

(* The binding port of a Scope is used to delimit the scope of
 * variables within the scope to the scope itself. *)
val Scope       = passive0  ("Scope");
(* Scopes have to be initialized before they can be used. An ActiveScope
 * is a scope that has has been initialized.
 * The first free port of an ActiveScope is used to delimit the scope of
 * variables within the scope to the scope itself.
 * The second free port should be connected to the instance identifier.
 *)
val ActiveScope = active0   ("ActiveScope");

(* The free port of a 'running' or 'stopped' node should be connected to
 * the scope port of the parent process/instance.
 *)
val Running     = atomic0   ("Running");
val Stopped     = atomic0   ("Stopped");

val Variables   = passive0 ("Variables"             );
(* The first free port of a variable should be connected to its name,
 * and the second should be connected to the scope port of the node
 * delimiting its scope. *)
val Variable    = passive0  ("Variable");
(* Values are atomic nodes connected to a name representing the value.
 *)
val Value       = atomic0   ("Value");

val Sequence    = active0   ("Sequence");
val Next        = passive0 ("Next"                  );

val Flow        = active0   ("Flow");

val While       = passive0  ("While");

val If          = passive0  ("If");
val Condition   = passive0 ("Condition"             );
val Then        = passive0 ("Then"                  );
val Else        = passive0 ("Else"                  );
val True        = atomic0  ("True"                  );
val False       = atomic0  ("False"                 );

val Assign      = passive0  ("Assign");
val Copy        = passive0 ("Copy"                  );
(* The first free port of a To or From node should be connected to a
 * variable name, and the second should be connected to the scope port of
 * the node delimiting its scope. *)
val To          = atomic0   ("To");
val From        = atomic0   ("From");

(* The free ports of an Invoke node should be connected:
 * 
 *   #1 to the name of the operation to be invoked
 *   #2 to the name of the input variable
 *   #3 to the same scope port as the input variable
 *   #4 to the name of the output variable
 *   #5 to the same scope port as the output variable
 *   #6 to the instance identifier
 *)
val Invoke      = atomic0   ("Invoke");

val Proxies     = passive0 ("Proxies"               );
(* The free ports of a RecProxy node should be connected:
 *
 *   #1 to the name of the operation
 *   #2 to the instance identifier
 *)
val RecProxy    = passive0  ("RecProxy");
val Parameter   = passive0 ("Parameter"             );
(* The free ports of a Receive node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the name of the variable
 *   #3 to the same scope port as the variable
 *   #4 to the instance identifier
 *)
val Receive     = atomic0   ("Receive");
(* The free ports of a Reply node should be connected:
 * 
 *   #1 to the name of the variable
 *   #2 to the same scope port as the variable
 *   #3 to the instance identifier of its enclosing instance
 *)
val Reply       = atomic0   ("Reply");
(* The free ports of a GetReply node should be connected:
 * 
 *   #1 to the name of the output variable
 *   #2 to the same scope port as the output variable
 *   #3 to the instance identifier of its enclosing instance
 *   #4 to the instance identifier of the replying instance
 *)
val GetReply    = atomic0   ("GetReply");

(* The free port should be connected to the instance identifier of the
 * enclosing instance. *)
val Exit        = atomic0   ("Exit");


(*******************************)
(*        Reaction rules       *)
(*******************************)
(* Scope *)
val rule_scope_activation   =
    "scope activation"   ::: Scope
                             || Running
                           ----|>
                             ActiveScope
                                o `[]`
                             || Running;
val rule_scope_completed    =
    "scope completed"    ::: ActiveScope o <->
                             || Running
                           ----|>
                             <->
                             || Running;

(* Structural activities *)
val rule_flow_completed     =
    "flow completed"     ::: Flow o <->
                             || Running
                           ----|>
                             <->
                             || Running;
val rule_sequence_completed =
    "sequence completed" ::: Sequence o Next o `[]`
                             || Running
                           ----|>
                             `[]`
                             || Running;
val rule_if_true            =
    "if true"            ::: (If
                              o (Condition o True
                                 `|` (Then o `[]`) `|` (Else o `[]`)))
                             || Running
                           ----|>
                             `[]`
                             || Running;
val rule_if_false           =
    "if false"           ::: (If
                              o (Condition o False
                                 `|` (Then o `[]`) `|` (Else o `[]`)))
                              || Running
                           --[0 |-> 1]--|>
                             `[]`
                             || Running;
val rule_while_unfold       =
    "while unfold"       ::: (While o (Condition `|` `[]`))
                             || Running
                           --[2 |-> 0, 3 |-> 1]--|>
                             (If
                              o (Condition
                                 `|` Then o Sequence
                                     o (`[]` 
                                        `|` Next
                                            o While
                                              o (Condition `|` `[]`))
                                 `|` Else o <->))
                             || Running;

(* Variable assignment *)
val rule_variable_copy      =
    "variable copy"      ::: (Assign o Copy
                              o (From
                                 `|` To))
                             || Variable
                             || Variable
                             || Running
                           --[1 |-> 0]--|>
                             <->
                             || Variable
                             || Variable
                             || Running;

(* Process communication *)
val rule_invoke             =
    "invoke"             ::: Invoke
                             || Variable
                             || Running
                             || Process
                                o ((Proxies
                                     o (RecProxy o <->
                                        `|` `[]`))
                                    `|` `[]`)

                           --[3 |-> 0, 4 |-> 1, 5 |-> 2]--|>

                             GetReply
                                || Variable
                                || Running
                                || ((Process
                                     o ((Proxies
                                          o (RecProxy o <->
                                             `|` `[]`))
                                         `|` `[]`))
                                    `|` (Instance
                                         o ((Proxies
                                             o ((RecProxy
                                                 o Parameter o `[]`)
                                                `|` `[]`))
                                            `|` Running
                                            `|` `[]`)));

val rule_receive            =
    "receive"            ::: Receive
                             || RecProxy o Parameter
                             || Variable
                             || Running
                           ----|>
                             <->
                             || RecProxy o <->
                             || Variable
                             || Running

val rule_reply              =
    "reply"              ::: Reply
                             || Variable
                             || Running
                             || GetReply
                             || Variable
                             || Running
                           --[1 |-> 0]--|>
                             <->
                             || Variable
                             || Running
                             || <->
                             || Variable
                             || Running


(* Process cancellation *)
val rule_exit_stop_inst     =
    "exit stop inst"     ::: Exit
                             || Running
                           ----|>
                             <->
                             || Stopped;
val rule_exit_remove_inst   =
    "exit remove inst"   ::: Instance
                             o (Stopped `|` `[]`)
                           ----|>
                             <->;
                             

val rules =
    mkrules [rule_scope_activation, rule_scope_completed,
             rule_flow_completed, rule_sequence_completed,
             rule_if_true, rule_if_false, rule_while_unfold,
             rule_variable_copy,
             rule_invoke, rule_receive, rule_reply,
             rule_exit_stop_inst, rule_exit_remove_inst];

val tactic = roundrobin;

(*******************************)
(*       Example processes     *)
(*******************************)

(* A simple echo process: it provides an operation "echo" which receives
 * a value and sends the value back.
 *
 * <process name="EchoProcess" scope="EchoId">
 * <variables>
 *   <variable name="x" scope="EchoId">42</variable>
 * </variables>
 * <proxies>
 *   <recproxy op="echo" inst_id="EchoId" />
 * </proxies>
 * <sequence inst_id="EchoId">
 *   <receive op="echo" var="x" var_scope="EchoId" inst_id="EchoId" />
 *   <next>
 *     <sequence inst_id="EchoId">
 *       <reply var="x" var_scope="EchoId" inst_id="EchoId" />
 *       <next>
 *         <exit inst_id="EchoId">
 *       </next>
 *     </sequence>
 *   </next>
 * </sequence>
 * </process>
 *)
val echo_process = Process
                   o (Variables o Variable o Value
                         `|` Proxies o RecProxy o <->
                         `|` Sequence
                             o (Receive
                                `|` Next o Sequence
                                         o (Reply
                                            `|` Next o Exit)));

(* An instance which is about to invoke the echo process:
 * 
 * <instance name"Caller" inst_id="CallerId">
 * <running inst_id="CallerId" />
 * <variables>
 *   <variable name="y" scope="CallerId">foo</variable>
 *   <variable name="z" scope="CallerId">bar</variable>
 * </variables>
 * <invoke op="echo" invar="y" invar_scope="CallerId"
 *                   outvar="z" outvar_scope="CallerId"
 *                   inst_id="CallerId" />
 * </instance>
 *)
val caller_inst = Instance
                  o (Running
                     `|` Variables
                         o (Variable o Value
                            `|` Variable o Value)
                     `|` Invoke);

(* NB! Non-terminating:*)
val ms = matches (mkrules [rule_reply]) (echo_process || caller_inst);
(*val ms = matches (mkrules [rule_invoke]) (echo_process || caller_inst);*)
val _ =print_mv ms;

(*val final_state = run rules tactic (echo_process || caller_inst);*)
