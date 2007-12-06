(* Formalization of a subset of BPEL using Binding Bigraphical Reactive
 * Systems in BPLtool.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

SMLofNJ.Internals.GC.messages false;
val cur_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir cur_dir;

(*******************************)
(*     String Declarations     *)
(*******************************)
val Process     = "Process"
val Instance    = "Instance"
val Scope       = "Scope"
val ActiveScope = "ActiveScope"
val Running     = "Running"
val Stopped     = "Stopped"
val Variables   = "Variables"
val Variable    = "Variable"
val Value       = "Value" 
val Sequence    = "Sequence"
val Next        = "Next"
val Flow        = "Flow"
val While       = "While"
val If          = "If"
val Condition   = "Condition"
val Then        = "Then"
val Else        = "Else"
val True        = "True"
val False       = "False"
val Assign      = "Assign"
val Copy        = "Copy"
val To          = "To"
val From        = "From"
val Invoke      = "Invoke"
val Proxies     = "Proxies"    
val RecProxy    = "RecProxy"
val Parameter   = "Parameter"
val Receive     = "Receive"
val Reply       = "Reply"
val GetReply    = "GetReply"
val Exit        = "Exit"

(*    For names                *)
val inst_id         = "inst_id"
val scope           = "scope"
val scope1          = "scope1"
val scope2          = "scope2"
val f               = "f"
val t               = "t"
val proc_name       = "proc_name"
val var             = "var"
val var_scope       = "var_scope"
val outvar          = "outvar"
val outvar_scope    = "outvar_scope"
val invar           = "invar"
val invar_scope     = "invar_scope"
val oper            = "op"
val inst_id_invoker = "inst_id_invoker"
val inst_id_invoked = "inst_id_invoked"
val echo            = "echo"
val echo_id         = "echo_id"
val echo_process    = "echo_process"
val val_1           = "val_1"
val val_2           = "val_2"
val val_42          = "val_42"
val x               = "x"
val y               = "y"
val z               = "z"
val caller          = "caller"
val caller_id       = "caller_id"



(*******************************)
(*          Signature          *)
(*******************************)
(* The binding port of a Process is used to delimit the scope of
 * variables within the process to the process itself.
 * The free port should be connected to name of the process. *)
val Process     = passive  (Process     =: 1 --> 1);

(* The first free port should be connected to name of the process.
 * The second free port of an instance is the instance identifier
 * which (among other things) is used to determine the scope of
 * variables within the instance to the instance itself. *)
val Instance    = active   (Instance    -:       2);

(* The binding port of a Scope is used to delimit the scope of
 * variables within the scope to the scope itself. *)
val Scope       = passive  (Scope       =: 1 --> 1);
(* Scopes have to be initialized before they can be used. An ActiveScope
 * is a scope that has has been initialized.
 * The first free port of an ActiveScope is used to delimit the scope of
 * variables within the scope to the scope itself.
 * The second free port should be connected to the instance identifier.
 *)
val ActiveScope = active   (ActiveScope -:       2);

(* The free port of a 'running' or 'stopped' node should be connected to
 * the scope port of the parent process/instance.
 *)
val Running     = atomic   (Running     -:       1);
val Stopped     = atomic   (Stopped     -:       1);

val Variables   = active0  (Variables             );
(* The first free port of a variable should be connected to its name,
 * and the second should be connected to the scope port of the node
 * delimiting its scope. *)
val Variable    = passive  (Variable    -:       2);
(* Values are atomic nodes connected to a name representing the value.
 *)
val Value       = atomic   (Value       -:       1);

val Sequence    = active   (Sequence    -:       1);
val Next        = passive0 (Next                  );

val Flow        = active   (Flow        -:       1);

val While       = passive  (While       -:       1);

val If          = passive  (If          -:       1);
val Condition   = passive0 (Condition             );
val Then        = passive0 (Then                  );
val Else        = passive0 (Else                  );
val True        = atomic0  (True                  );
val False       = atomic0  (False                 );

val Assign      = passive  (Assign      -:       1);
val Copy        = passive0 (Copy                  );
(* The first free port of a To or From node should be connected to a
 * variable name, and the second should be connected to the scope port of
 * the node delimiting its scope. *)
val To          = atomic   (To          -:       2);
val From        = atomic   (From        -:       2);

(* The free ports of an Invoke node should be connected:
 * 
 *   #1 to the name of the operation to be invoked
 *   #2 to the name of the input variable
 *   #3 to the same scope port as the input variable
 *   #4 to the name of the output variable
 *   #5 to the same scope port as the output variable
 *   #6 to the instance identifier
 *)
val Invoke      = atomic   (Invoke      -:       6);

val Proxies     = passive0 (Proxies               );
(* The free ports of a RecProxy node should be connected:
 *
 *   #1 to the name of the operation
 *   #2 to the instance identifier
 *)
val RecProxy    = passive  (RecProxy    -:       2);
val Parameter   = passive0 (Parameter             );
(* The free ports of a Receive node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the name of the variable
 *   #3 to the same scope port as the variable
 *   #4 to the instance identifier
 *)
val Receive     = atomic   (Receive     -:       4);
(* The free ports of a Reply node should be connected:
 * 
 *   #1 to the name of the variable
 *   #2 to the same scope port as the variable
 *   #3 to the instance identifier of its enclosing instance
 *)
val Reply       = atomic   (Reply       -:       3);
(* The free ports of a GetReply node should be connected:
 * 
 *   #1 to the name of the output variable
 *   #2 to the same scope port as the output variable
 *   #3 to the instance identifier of its enclosing instance
 *   #4 to the instance identifier of the replying instance
 *)
val GetReply    = atomic   (GetReply    -:       4);

(* The free port should be connected to the instance identifier of the
 * enclosing instance. *)
val Exit        = atomic   (Exit        -:       1);


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
                              o (From[f, scope1]
                                 `|` To[t, scope2]))
                             || Variable[f, scope1]
                             || Variable[t, scope2]
                             || Running[inst_id]
                           --[1 |-> 0]--|>
                             <->
                             || Variable[f, scope1]
                             || Variable[t, scope2]
                             || Running[inst_id];

(* Process communication *)
val rule_invoke             =
    "invoke"             ::: Invoke[oper, invar, invar_scope,
                                    outvar, outvar_scope,
                                    inst_id_invoker]
                             || Variable[invar, invar_scope]
                             || Running[inst_id_invoker]
                             || Process[proc_name][[scope]]
                                o ((Proxies
                                    o (RecProxy[oper, scope] o <->
                                       `|` `[]`))
                                   `|` `[scope]`)

                           --[3 |-> 0, 4 |-> 1, 5&[inst_id_invoked] |--> 2&[scope]]--|>

                             -/inst_id_invoked
                             o (GetReply[outvar, outvar_scope,
                                         inst_id_invoker, inst_id_invoked]
                                || Variable[invar, invar_scope]
                                || Running[inst_id_invoker]
                                || ((Process[proc_name][[scope]]
                                     o ((Proxies
                                         o (RecProxy[oper, scope] o <->
                                            `|` `[]`))
                                        `|` `[scope]`))
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
 * <process name="echo_process" scope="echo_id">
 * <variables>
 *   <variable name="x" scope="echo_id">val_42</variable>
 * </variables>
 * <proxies>
 *   <recproxy op="echo" inst_id="echo_id" />
 * </proxies>
 * <sequence inst_id="echo_id">
 *   <receive op="echo" var="x" var_scope="echo_id" inst_id="echo_id" />
 *   <next>
 *     <sequence inst_id="echo_id">
 *       <reply var="x" var_scope="echo_id" inst_id="echo_id" />
 *       <next>
 *         <exit inst_id="echo_id">
 *       </next>
 *     </sequence>
 *   </next>
 * </sequence>
 * </process>
 *)
val echo_process = Process[echo_process][[echo_id]]
                   o (Variables o Variable[x, echo_id] o Value[val_42]
                      `|` Proxies o RecProxy[echo, echo_id] o <->
                      `|` Sequence[echo_id]
                          o (Receive[echo, x, echo_id, echo_id]
                             `|` Next o Sequence[echo_id]
                                      o (Reply[x, echo_id, echo_id]
                                         `|` Next o Exit[echo_id])));

(* An instance which is about to invoke the echo process:
 * 
 * <instance name"caller" inst_id="caller_id">
 * <running inst_id="caller_id" />
 * <variables>
 *   <variable name="y" scope="caller_id">val_1</variable>
 *   <variable name="z" scope="caller_id">val_2</variable>
 * </variables>
 * <invoke op="echo" invar="y" invar_scope="caller_id"
 *                   outvar="z" outvar_scope="caller_id"
 *                   inst_id="caller_id" />
 * </instance>
 *)
val caller_inst = -/caller_id
                  o Instance[caller, caller_id]
                  o (Running[caller_id]
                     `|` Variables
                         o (Variable[y, caller_id] o Value[val_1]
                            `|` Variable[z, caller_id] o Value[val_2])
                     `|` Invoke[echo, y, caller_id,
                                z, caller_id, caller_id]);

(* NB! Non-terminating:
val ms = matches (mkrules [rule_reply]) (echo_process || caller_inst);
val ms = matches (mkrules [rule_invoke]) (echo_process || caller_inst);
print_mv ms;*)

(*val final_state = run rules tactic (echo_process || caller_inst);*)
