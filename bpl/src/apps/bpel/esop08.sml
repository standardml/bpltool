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
(*     String Declarations     *)
(*******************************)
val process  = "Process"
val instance = "Instance"
val scope    = "Scope"
val activeScope = "ActiveScope"
val running  = "Running"
val stopped  = "Stopped"
val variables = "Variables"
val variable = "Variable"
val value    = "Value" 
val sequence = "Sequence"
val next     = "Next"
val flow     = "Flow"
val whileC   = "While"
val ifC      = "If"
val condition = "Condition"
val thenC    = "Then"
val elseC    = "Else"
val trueC    = "True"
val falseC   = "False"
val assign   = "Assign"
val copy     = "Copy"
val to       = "To"
val from     = "From"
val invoke   = "Invoke"
val proxies  = "Proxies"    
val recProxy = "RecProxy"
val parameter = "Parameter"
val receive  = "Receive"
val reply    = "Reply"
val getReply = "GetReply"
val exit     = "Exit"

(*    For names                *)
val inst_id = "inst_id"
val scope   = "scope"
val scope'  = "scope'"
val f       = "f"
val t       = "t"
val proc_name = "proc_name"
val var     = "var"
val var_scope = "var_scope"
val outvar  = "outvar"
val outvar_scope = "outvar_scope"
val invar   = "invar"
val invar_scope = "invar_scope"
val oper    = "op"
val inst_id_invoker = "inst_id_invoker"
val inst_id_invoked = "inst_id_invoked"


(*******************************)
(*          Signature          *)
(*******************************)
(* The binding port of a Process is used to delimit the scope of
 * variables within the process to the process itself.
 * The free port should be connected to name of the process. *)
val Process     = passive  (process     =: 1 --> 1);

(* The first free port should be connected to name of the process.
 * The second free port of an instance is the instance identifier
 * which (among other things) is used to determine the scope of
 * variables within the instance to the instance itself. *)
val Instance    = active   (instance    -:       2);

(* The binding port of a Scope is used to delimit the scope of
 * variables within the scope to the scope itself. *)
val Scope       = passive  (scope       =: 1 --> 1);
(* Scopes have to be initialized before they can be used. An ActiveScope
 * is a scope that has has been initialized.
 * The first free port of an ActiveScope is used to delimit the scope of
 * variables within the scope to the scope itself.
 * The second free port should be connected to the instance identifier.
 *)
val ActiveScope = active   (activeScope -:       2);

(* The free port of a 'running' or 'stopped' node should be connected to
 * the scope port of the parent process/instance.
 *)
val Running     = atomic   (running     -:       1);
val Stopped     = atomic   (stopped     -:       1);

val Variables   = active0 (variables             );
(* The first free port of a variable should be connected to its name,
 * and the second should be connected to the scope port of the node
 * delimiting its scope. *)
val Variable    = passive  (variable    -:       2);
(* Values are atomic nodes connected to a name representing the value.
 *)
val Value       = atomic   (value       -:       1);

val Sequence    = active   (sequence    -:       1);
val Next        = passive0 (next                  );

val Flow        = active   (flow        -:       1);

val While       = passive  (whileC      -:       1);

val If          = passive  (ifC          -:       1);
val Condition   = passive0 (condition             );
val Then        = passive0 (thenC                  );
val Else        = passive0 (elseC                  );
val True        = atomic0  (trueC                  );
val False       = atomic0  (falseC                 );

val Assign      = passive  (assign      -:       1);
val Copy        = passive0 (copy                  );
(* The first free port of a To or From node should be connected to a
 * variable name, and the second should be connected to the scope port of
 * the node delimiting its scope. *)
val To          = atomic   (to          -:       2);
val From        = atomic   (from        -:       2);

(* The free ports of an Invoke node should be connected:
 * 
 *   #1 to the name of the operation to be invoked
 *   #2 to the name of the input variable
 *   #3 to the same scope port as the input variable
 *   #4 to the name of the output variable
 *   #5 to the same scope port as the output variable
 *   #6 to the instance identifier
 *)
val Invoke      = atomic   (invoke      -:       6);

val Proxies     = passive0 (proxies               );
(* The free ports of a RecProxy node should be connected:
 *
 *   #1 to the name of the operation
 *   #2 to the instance identifier
 *)
val RecProxy    = passive  (recProxy    -:       2);
val Parameter   = passive0 (parameter             );
(* The free ports of a Receive node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the name of the variable
 *   #3 to the same scope port as the variable
 *   #4 to the instance identifier
 *)
val Receive     = atomic   (receive     -:       4);
(* The free ports of a Reply node should be connected:
 * 
 *   #1 to the name of the variable
 *   #2 to the same scope port as the variable
 *   #3 to the instance identifier of its enclosing instance
 *)
val Reply       = atomic   (reply       -:       3);
(* The free ports of a GetReply node should be connected:
 * 
 *   #1 to the name of the output variable
 *   #2 to the same scope port as the output variable
 *   #3 to the instance identifier of its enclosing instance
 *   #4 to the instance identifier of the replying instance
 *)
val GetReply    = atomic   (getReply    -:       4);

(* The free port should be connected to the instance identifier of the
 * enclosing instance. *)
val Exit        = atomic   (exit        -:       1);


(*******************************)
(*        Reaction rules       *)
(*******************************)
(* Scope *)
val rule_scope_activation   =
    "scope activation"   ::: Scope[inst_id][[scope]]
                             || Running[inst_id]
                           ----|>
                             -/scope
                             o (ActiveScope[scope, inst_id]
                                o `[scope]`)
                             || Running[inst_id];

val rule_scope_completed    =
    "scope completed"    ::: -/scope o ActiveScope[scope, inst_id] o <->
                             || Running[inst_id]
                           ----|>
			     <-> || Running[inst_id];

(* Structural activities *)
val rule_flow_completed     =
    "flow completed"     ::: Flow[inst_id] o <->
                             || Running[inst_id]
                           ----|>
                             <->
                             || Running[inst_id];
val rule_sequence_completed =
    "sequence completed" ::: Sequence[inst_id] o Next o `[]`
                             || Running[inst_id]
                           ----|>
                             `[]`
                             || Running[inst_id];
val rule_if_true            =
    "if true"            ::: (If[inst_id]
                              o (Condition o True
                                 `|` (Then o `[]`) `|` (Else o `[]`)))
                             || Running[inst_id]
                           ----|>
                             `[]`
                             || Running[inst_id];
val rule_if_false           =
    "if false"           ::: (If[inst_id]
                              o (Condition o False
                                 `|` (Then o `[]`) `|` (Else o `[]`)))
                              || Running[inst_id]
                           --[0 |-> 1]--|>
                             `[]`
                             || Running[inst_id];
val rule_while_unfold       =
    "while unfold"       ::: (While[inst_id] o (Condition `|` `[]`))
                             || Running[inst_id]
                           --[2 |-> 0, 3 |-> 1]--|>
                             (If[inst_id]
                              o (Condition
                                 `|` Then o Sequence[inst_id]
                                     o (`[]` 
                                        `|` Next
                                            o While[inst_id]
                                              o (Condition `|` `[]`))
                                 `|` Else o <->))
                             || Running[inst_id];

(* Variable assignment *)
val rule_variable_copy      =
    "variable copy"      ::: (Assign[inst_id] o Copy
                              o (From[f, scope]
                                 `|` To[t, scope']))
                             || Variable[f, scope]
                             || Variable[t, scope']
                             || Running[inst_id]
                           --[1 |-> 0]--|>
                             <->
                             || Variable[f, scope]
                             || Variable[t, scope']
                             || Running[inst_id];

(* Process communication *)
val rule_invoke             =
    "invoke"             ::: Invoke[oper, invar, invar_scope,
                                    outvar, outvar_scope,
                                    inst_id_invoker]
                             || Variable[invar, invar_scope]
                             || Running[inst_id_invoker]
                             || Process[proc_name][[scope]]
                                o (<[scope]>
                                   ((Proxies
                                     o (RecProxy[oper, scope] o <->
                                        `|` `[]`))
                                    `|` `[scope]`))

                           --[3 |-> 0, 4 |-> 1, 5&[inst_id_invoked] |--> 2&[scope]]--|>

                             -/inst_id_invoked
                             o (GetReply[outvar, outvar_scope,
                                         inst_id_invoker, inst_id_invoked]
                                || Variable[invar, invar_scope]
                                || Running[inst_id_invoker]
                                || ((Process[proc_name][[scope]]
                                     o (<[scope]>
                                        ((Proxies
                                          o (RecProxy[oper, scope] o <->
                                             `|` `[]`))
                                         `|` `[scope]`)))
                                    `|` (Instance[proc_name, inst_id_invoked]
                                         o ((Proxies
                                             o ((RecProxy[oper, inst_id_invoked]
                                                 o Parameter o `[]`)
                                                `|` `[]`))
                                            `|` Running[inst_id_invoked]
                                            `|` `[inst_id_invoked]`))));

val rule_receive            =
    "receive"            ::: Receive[oper, var, var_scope, inst_id]
                             || RecProxy[oper, inst_id] o Parameter
                             || Variable[var, var_scope]
                             || Running[inst_id]
                           ----|>
                             <->
                             || RecProxy[oper, inst_id] o <->
                             || Variable[var, var_scope]
                             || Running[inst_id]

val rule_reply              =
    "reply"              ::: Reply[var, var_scope, inst_id_invoked]
                             || Variable[var, var_scope]
                             || Running[inst_id_invoked]
                             || GetReply[outvar, outvar_scope,
                                         inst_id_invoker, inst_id_invoked]
                             || Variable[outvar, outvar_scope]
                             || Running[inst_id_invoker]
                           --[1 |-> 0]--|>
                             <->
                             || Variable[var, var_scope]
                             || Running[inst_id_invoked]
                             || <->
                             || Variable[outvar, outvar_scope]
                             || Running[inst_id_invoker]


(* Process cancellation *)
val rule_exit_stop_inst     =
    "exit stop inst"     ::: Exit[inst_id]
                             || Running[inst_id]
                           ----|>
                             <->
                             || Stopped[inst_id];
val rule_exit_remove_inst   =
    "exit remove inst"   ::: -/inst_id o (Instance[proc_name, inst_id]
                                o (Stopped[inst_id] `|` `[]`))
                           ----|>
                             proc_name//[] * <->;
                             

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
val echo_process = Process["EchoProcess"][["EchoId"]]
                   o (<["EchoId"]>
                      ("EchoId"//["EchoId", "EchoScope"] * idp(1))
                      o (Variables o Variable["x", "EchoScope"] o Value["42"]
                         `|` Proxies o RecProxy["echo", "EchoId"] o <->
                         `|` Sequence["EchoId"]
                             o (Receive["echo", "x", "EchoScope", "EchoId"]
                                `|` Next o Sequence["EchoId"]
                                         o (Reply["x", "EchoScope", "EchoId"]
                                            `|` Next o Exit["EchoId"]))));

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
val caller_inst = -/"CallerId"
                  o Instance["Caller", "CallerId"]
                  o ("CallerId"//["CallerId", "CallerScope"] * idp(1))
                  o (Running["CallerId"]
                     `|` Variables
                         o (Variable["y", "CallerScope"] o Value["foo"]
                            `|` Variable["z", "CallerScope"] o Value["bar"])
                     `|` "CallerScope"//["CallerScope", "CallerScope'"]
                         o Invoke["echo", "y", "CallerScope",
                                          "z", "CallerScope'", "CallerId"]);

(* NB! Non-terminating:
val ms = matches (mkrules [rule_reply]) (echo_process || caller_inst);
val ms = matches (mkrules [rule_invoke]) (echo_process || caller_inst);
print_mv ms;*)

(*val final_state = run rules tactic (echo_process || caller_inst);*)
