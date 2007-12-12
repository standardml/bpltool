(* Formalization of a subset of BPEL using Binding Bigraphical Reactive
 * Systems in BPLtool.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

SMLofNJ.Internals.GC.messages false;
val bpel_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir bpel_dir;
use "figinfo.sml";

(*******************************)
(*     String Declarations     *)
(*******************************)
val Process        = "Process"
val Instance       = "Instance"
val Scope          = "Scope"
val ActiveScope    = "ActiveScope"
val Running        = "Running"
val Invoked        = "Invoked"
val Stopped        = "Stopped"
val Variables      = "Variables"
val Variable       = "Variable"
val VariableRef    = "VariableRef" 
val Sequence       = "Sequence"
val Next           = "Next"
val Flow           = "Flow"
val While          = "While"
val If             = "If"
val Condition      = "Condition"
val Then           = "Then"
val Else           = "Else"
val True           = "True"
val False          = "False"
val Assign         = "Assign"
val Copy           = "Copy"
val To             = "To"
val From           = "From"
val Invoke         = "Invoke"
val PartnerLinks   = "PartnerLinks"    
val PartnerLink    = "PartnerLink"
val Message        = "Message"
val Link           = "Link"
val CreateInstance = "CreateInstance"
val Receive        = "Receive"
val Reply          = "Reply"
val GetReply       = "GetReply"
val Exit           = "Exit"

(*    For names                *)
val inst_id              = "inst_id"
val inst_id_invoker      = "inst_id_invoker"
val inst_id_invoked      = "inst_id_invoked"
val scope                = "scope"
val scope1               = "scope1"
val scope2               = "scope2"
val f                    = "f"
val t                    = "t"
val proc_name            = "proc_name"
val var                  = "var"
val var_scope            = "var_scope"
val outvar               = "outvar"
val outvar_scope         = "outvar_scope"
val invar                = "invar"
val invar_scope          = "invar_scope"
val oper                 = "oper"
val partner_link         = "partner_link"
val partner_link_invoker = "partner_link_invoker"
val partner_link_invoked = "partner_link_invoked"
val echo                 = "echo"
val echo_id              = "echo_id"
val echo_process         = "echo_process"
val echo_value           = "echo_value"
val echo_service         = "echo_service"
val x                    = "x"
val y                    = "y"
val caller               = "caller"
val caller_id            = "caller_id"



(*******************************)
(*          Signature          *)
(*******************************)
(* The binding port of a Process is used to delimit the scope of
 * variables within the process to the process itself.
 * The free port should be connected to name of the process. *)
val Process      = passive  (Process     =: 1 --> 1);

(* The first free port should be connected to name of the process.
 * The second free port of an instance is the instance identifier
 * which (among other things) is used to determine the scope of
 * variables within the instance to the instance itself. *)
val Instance     = active   (Instance    -:       2);

(* The binding port of a Scope is used to delimit the scope of
 * variables within the scope to the scope itself. *)
val Scope        = passive  (Scope       =: 1 --> 1);
(* Scopes have to be initialized before they can be used. An ActiveScope
 * is a scope that has has been initialized.
 * The first free port of an ActiveScope is used to delimit the scope of
 * variables within the scope to the scope itself.
 * The second free port should be connected to the instance identifier.
 *)
val ActiveScope  = active   (ActiveScope -:       2);

(* The free port of a 'running', 'invoked', or 'stopped' node should
 * be connected to the scope port of the parent process/instance.
 *)
val Running      = atomic   (Running     -:       1);
val Invoked      = atomic   (Invoked     -:       1);
val Stopped      = atomic   (Stopped     -:       1);

val Variables    = active0  (Variables             );
(* The free ports of a variable should be connected
 *
 *   #1 to its name
 *   #2 to the scope port of the node delimiting its scope
 *)
val Variable     = passive  (Variable    -:       2);
(* The free ports of a variable reference should be connected
 *
 *   #1 to the variable name
 *   #2 to the scope port of the node delimiting the variables scope
 *   #3 to the instance identifier
 *)
val VariableRef  = atomic   (VariableRef -:       3);

val Sequence     = active   (Sequence    -:       1);
val Next         = passive0 (Next                  );

val Flow         = active   (Flow        -:       1);

val While        = passive  (While       -:       1);

val If           = active   (If          -:       1);
val Condition    = active0  (Condition             );
val Then         = passive0 (Then                  );
val Else         = passive0 (Else                  );
val True         = atomic0  (True                  );
val False        = atomic0  (False                 );

val Assign       = passive  (Assign      -:       1);
val Copy         = passive0 (Copy                  );
(* The first free port of a To or From node should be connected to a
 * variable name, and the second should be connected to the scope port of
 * the node delimiting its scope. *)
val To           = atomic   (To          -:       2);
val From         = atomic   (From        -:       2);

(* The free ports of an Invoke node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the name of the operation to be invoked
 *   #3 to the name of the input variable
 *   #4 to the same scope port as the input variable
 *   #5 to the name of the output variable
 *   #6 to the same scope port as the output variable
 *   #7 to the instance identifier
 *)
val Invoke       = atomic   (Invoke      -:       7);

val PartnerLinks = passive0 (PartnerLinks          );
(* The free ports of a PartnerLink node should be connected:
 *
 *   #1 to the name of the partner link
 *   #2 to the instance identifier
 *)
val PartnerLink  = passive  (PartnerLink -:       2);
(* The free port of a Message should be connected:
 *
 *   #1 to the name of the operation the message pertains to
 *)
val Message      = passive  (Message     -:       1);
(* The free port of a Link should be connected:
 *
 *   #1 to the instance identifier of a partner instance
 *)
val Link         = atomic   (Link        -:       1);
(* The free port of a Link should be connected:
 *
 *   #1 to the name of the operation which can create instances
 *        of the enclosing process
 *)
val CreateInstance = atomic (CreateInstance -:    1);
(* The free ports of a Receive node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the name of the operation
 *   #3 to the name of the variable
 *   #4 to the same scope port as the variable
 *   #5 to the instance identifier
 *)
val Receive      = atomic   (Receive     -:       5);
(* The free ports of a Reply node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the name of the operation
 *   #3 to the name of the variable
 *   #4 to the same scope port as the variable
 *   #5 to the instance identifier of its enclosing instance
 *)
val Reply        = atomic   (Reply       -:       5);
(* The free ports of a GetReply node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the name of the operation
 *   #3 to the name of the output variable
 *   #4 to the same scope port as the output variable
 *   #5 to the instance identifier of its enclosing instance
 *)
val GetReply     = atomic   (GetReply    -:       5);

(* The free port should be connected to the instance identifier of the
 * enclosing instance. *)
val Exit         = atomic   (Exit        -:       1);


(*******************************)
(*        Reaction rules       *)
(*******************************)
(* Scope *)
val rule_scope_activation = "scope activation" :::

Scope[inst_id][[scope]]
|| Running[inst_id]
  ----|>
-/scope o (ActiveScope[scope, inst_id] o `[scope]`)
|| Running[inst_id];


val rule_scope_completed = "scope completed" :::

-/scope o (ActiveScope[scope, inst_id] o <->)
|| Running[inst_id]
  ----|>
<->
|| Running[inst_id];


(* Structural activities *)
val rule_flow_completed = "flow completed" :::

Flow[inst_id] o <->
|| Running[inst_id]
  ----|>
<->
|| Running[inst_id];


val rule_sequence_completed = "sequence completed" :::

Sequence[inst_id] o Next o `[]`
|| Running[inst_id]
  ----|>
`[]`
|| Running[inst_id];


val rule_if_true = "if true" :::

If[inst_id] o (Condition o True
               `|` Then o `[]`
               `|` Else o `[]`)
|| Running[inst_id]
  ----|>
`[]`
|| Running[inst_id];


val rule_if_false = "if false" :::
If[inst_id] o (Condition o False
               `|` Then o `[]`
               `|` Else o `[]`)
|| Running[inst_id]
  --[0 |-> 1]--|>
`[]`
|| Running[inst_id];


val rule_while_unfold = "while unfold" :::
While[inst_id] o (Condition o `[]` `|` `[]`)
|| Running[inst_id]
  --[2 |-> 0, 3 |-> 1]--|>
If[inst_id] o (Condition o `[]`
               `|` Then o Sequence[inst_id]
                          o (`[]` 
                             `|` Next
                                 o While[inst_id] o (Condition o `[]` `|` `[]`))
               `|` Else o <->)
|| Running[inst_id];


(* Expression evaluation *)
val rule_variable_reference = "variable reference" :::
VariableRef[var, var_scope, inst_id]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id]
  --[1 |-> 0]--|>
`[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id];


(* Variable assignment *)
val rule_variable_copy = "variable copy" :::
Assign[inst_id] o Copy o (From[f, scope1]
                          `|` To[t, scope2])
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id]
  --[1 |-> 0]--|>
<->
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id];


(* Process communication *)
val rule_invoke = "invoke" :::
Invoke[partner_link_invoker, oper, invar, invar_scope,
       outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, inst_id_invoker] o <->
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker]
|| Process[proc_name][[scope]]
   o (PartnerLinks
      o (PartnerLink[partner_link, scope] o (CreateInstance[oper] `|` `[]`)
         `|` `[]`)
      `|` `[scope]`)

  --[4 |-> 0, 5 |-> 2, 6&[inst_id_invoked] |--> 3&[scope]]--|>

-/inst_id_invoked
o (GetReply[partner_link, oper, outvar, outvar_scope, inst_id_invoker]
   || PartnerLink[partner_link_invoker, inst_id_invoker]
      o Link[inst_id_invoked]
   || Variable[invar, invar_scope] o `[]`
   || Running[inst_id_invoker]
   || (Process[proc_name][[scope]]
       o (PartnerLinks
          o (PartnerLink[partner_link, scope] o (CreateInstance[oper] `|` `[]`)
             `|` `[]`)
          `|` `[scope]`)
       `|` Instance[proc_name, inst_id_invoked]
           o (PartnerLinks
              o (PartnerLink[partner_link, inst_id_invoked]
                 o (Link[inst_id_invoker] `|` Message[oper] o `[]`)
                 `|` `[]`)
              `|` Invoked[inst_id_invoked]
              `|` `[inst_id_invoked]`)));


val rule_receive = "receive" :::
Receive[partner_link, oper, var, var_scope, inst_id]
|| PartnerLink[partner_link, inst_id] o (`[]` `|` Message[oper] o `[]`)
|| Variable[var, var_scope] o `[]`
|| Invoked[inst_id]
  ----|>
<-> || oper//[]
|| PartnerLink[partner_link, inst_id] o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id];


val rule_invoke_receive = "invoke_receive" :::
Invoke[partner_link_invoker, oper,
       invar, invar_scope, outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, inst_id_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker]
|| Receive[partner_link_invoked, oper, var, var_scope, inst_id_invoked]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]

  --[2 |-> 1]--|>

GetReply[partner_link_invoker, oper, outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, inst_id_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker]
|| <-> || partner_link_invoked//[]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked];


val rule_reply = "reply" :::
Reply[partner_link_invoked, oper, var, var_scope, inst_id_invoked]
|| PartnerLink[partner_link_invoked, inst_id_invoked]
   o (Link[inst_id_invoker] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]
|| GetReply[partner_link_invoker, oper,
            outvar, outvar_scope, inst_id_invoker]
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker]

  --[2 |-> 1]--|>

<-> || oper//[]
|| PartnerLink[partner_link_invoked, inst_id_invoked]
   o (Link[inst_id_invoker] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]
|| <-> || partner_link_invoker//[]
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker];


(* Process cancellation *)
val rule_exit_stop_inst = "exit stop inst" :::
Exit[inst_id]
|| Running[inst_id]
  ----|>
<->
|| Stopped[inst_id];


val rule_exit_remove_inst = "exit remove inst" :::
-/inst_id
o (Instance[proc_name, inst_id]
   o (Stopped[inst_id] `|` `[]`))
  ----|>
<-> || proc_name//[];
                             

val rules =
    mkrules [rule_scope_activation, rule_scope_completed,
             rule_flow_completed, rule_sequence_completed,
             rule_if_true, rule_if_false, rule_while_unfold,
             rule_variable_copy,
             rule_invoke, rule_receive, rule_invoke_receive, rule_reply,
             rule_exit_stop_inst, rule_exit_remove_inst];

val tactic = roundrobin;

(*******************************)
(*       Example processes     *)
(*******************************)

(* A simple echo process: it provides an operation "echo" which receives
 * a value and sends the value back.
 *
 * <process name="echo_process">
 * <partnerLinks>
 *   <partnerLink name="echo_service" />
 * </partnerLinks>
 * <variables>
 *   <variable name="x"><from>true()</from></variable>
 * </variables>
 * <sequence>
 *   <receive partnerLink="echo_service" operation="echo"
 *            createInstance="yes" variable="x" />
 *   <reply   partnerLink="echo_service" operation="echo" variable="x" />
 *   <exit>
 * </sequence>
 * </process>
 *)
val echo_process1 =
Process[echo_process][[echo_id]]
o (PartnerLinks o PartnerLink[echo_service, echo_id] o CreateInstance[echo]
   `|` Variables o Variable[x, echo_id] o True
   `|` Sequence[echo_id]
       o (Receive[echo_service, echo, x, echo_id, echo_id]
          `|` Next o Sequence[echo_id]
                   o (Reply[echo_service, echo, x, echo_id, echo_id]
                      `|` Next o Exit[echo_id])));

(* An instance which is about to invoke the echo process:
 * 
 * <instance name"caller">
 * <partnerLinks>
 *   <partnerLink name="echo_service" />
 * </partnerLinks>
 * <variables>
 *   <variable name="x"><from>true()</from></variable>
 *   <variable name="y"><from>false()</from></variable>
 * </variables>
 * <invoke partnerLink="echo_service" operation="echo"
 *         inputVariable="x" outputVariable="y" />
 * </instance>
 *)
val caller_inst1 =
-/caller_id
o (Instance[caller, caller_id]
   o (PartnerLinks o PartnerLink[echo_service, caller_id] o <->
      `|` Running[caller_id]
      `|` Variables
           o (Variable[x, caller_id] o True
              `|` Variable[y, caller_id] o False)
          `|` Invoke[echo_service, echo, x, caller_id,
                     y, caller_id, caller_id]));


(* A slightly more advanced echo process: 
 * It's initiated by the operation "echo" and, if the received value isn't
 * False, then provides an operation "echo_value" to the calling process
 * which receives values and sends them back. If the value received was
 * False the process replies False to the initial "echo" operation and exits.
 *
 * <process name="echo_process">
 * <partnerLinks>
 *   <partnerLink name="echo_service" />
 * </partnerLinks>
 * <variables>
 *   <variable name="x"><from>true()</from></variable>
 * </variables>
 * <sequence>
 *   <receive partnerLink="echo_service" operation="echo"
 *            createInstance="yes" variable="x" />
 *   <while>
 *     <condition>$x</condition>
 *     <scope>
 *       <variables>
 *         <variable name="y" />
 *       </variables>
 *       <sequence>
 *         <receive partnerLink="echo_service" operation="echo_value" variable="y" />
 *         <reply   partnerLink="echo_service" operation="echo_value" variable="y" />
 *         <assign>
 *           <copy>
 *             <from variable="y" />
 *             <to variable="x" />
 *           </copy>
 *         </assign>
 *       </sequence>
 *     </scope>
 *   </while>
 *   <reply partnerLink="echo_service" operation="echo" variable="x" />
 * </sequence>
 * </process>
 *)
val echo_process2 = 
Process[echo_process][[echo_id]]
o (PartnerLinks o PartnerLink[echo_service, echo_id] o CreateInstance[echo]
   `|` Variables o Variable[x, echo_id] o True
   `|` Sequence[echo_id]
       o (Receive[echo_service, echo, x, echo_id, echo_id]
          `|` Next
              o Sequence[echo_id]
              o (While[echo_id]
                 o (Condition o VariableRef[x, echo_id, echo_id]
                    `|` Scope[echo_id][[scope]]
                        o (Variables o Variable[y, scope] o <->
                           `|` Sequence[echo_id]
                               o (Receive[echo_service, echo_value,
                                          y, scope, echo_id]
                                  `|` Next
                                      o Sequence[echo_id]
                                      o (Reply[echo_service, echo_value,
                                               y, scope, echo_id]
                                         `|` Next
                                             o Assign[echo_id]
                                             o Copy
                                             o (From[y, scope]
                                                `|` To[x, echo_id])))))
                 `|` Next o Reply[echo_service, echo, x, echo_id, echo_id])));

(* An instance which is about to invoke the advanced echo process:
 * 
 * <instance name"caller">
 * <partnerLinks>
 *   <partnerLink name="echo_service" />
 * </partnerLinks>
 * <variables>
 *   <variable name="x"><from>true()</from></variable>
 *   <variable name="y"><from>false()</from></variable>
 * </variables>
 * <flow>
 *   <invoke partnerLink="echo_service" operation="echo"
 *           inputVariable="x" outputVariable="y" />
 *   <invoke partnerLink="echo_service" operation="echo_value"
 *           inputVariable="y" outputVariable="y" />
 *   <exit />
 * </flow>
 * </instance>
 *)
val caller_inst2 =
-/caller_id
o (Instance[caller, caller_id]
   o (PartnerLinks o PartnerLink[echo_service, caller_id] o <->
      `|` Running[caller_id]
      `|` Variables
          o (Variable[x, caller_id] o True
             `|` Variable[y, caller_id] o False)
      `|` Flow[caller_id]
          o (Invoke[echo_service, echo,
                    x, caller_id, y, caller_id, caller_id]
             `|` Invoke[echo_service, echo_value,
                        y, caller_id, y, caller_id, caller_id]
             `|` Exit[caller_id])));


(* 
val ms = matches (mkrules [rule_reply]) (echo_process || caller_inst);
val ms = matches (mkrules [rule_invoke]) (echo_process || caller_inst);
print_mv ms;*)

(*val final_state = run rules tactic (echo_process || caller_inst);*)
