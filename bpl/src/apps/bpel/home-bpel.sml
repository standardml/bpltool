(* Formalization of a subset of BPEL extended with mobile processes
 * using Binding Bigraphical Reactive Systems in BPLtool.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

SMLofNJ.Internals.GC.messages false;
val bpel_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir bpel_dir;
use "figinfo.sml";
Flags.setBoolFlag "/kernel/ast/bgval/tensor2parallel" true;

(*******************************)
(*     String Declarations     *)
(*******************************)
val Process        = "Process"
val Instance       = "Instance"
(*val SubProcesses   = "SubProcesses"*)
(*val SubProcess     = "SubProcess"*)
val Instances      = "Instances"
(*val Instances   = "Instances"*)
(*val SubInstance    = "SubInstance"*)
(*val FrozenSub      = "FrozenSub"*)
val Scope          = "Scope"
val ActiveScope    = "ActiveScope"
val Invoked        = "Invoked"
val Running        = "Running"
val Freezing       = "Freezing"
val Stopped        = "Stopped"
val TopRunning     = "TopRunning"
val SubTransition  = "SubTransition"
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
val ToPLink        = "ToPLink"
val FromPLink      = "FromPLink"
val Exit           = "Exit"
val PartnerLinks   = "PartnerLinks"    
val PartnerLink    = "PartnerLink"
val SubLinks       = "SubLinks"
val SubLink        = "SubLink"
val FrozenSupLink  = "FrozenSupLink"
val Message        = "Message"
val Link           = "Link"
val CreateInstance = "CreateInstance"
val Invoke         = "Invoke"
val Receive        = "Receive"
val Reply          = "Reply"
val GetReply       = "GetReply"
val InvokeSub      = "InvokeSub"
val ReceiveSub     = "ReceiveSub"
val ReplySub       = "ReplySub"
val GetReplySub    = "GetReplySub"
val InvokeSup      = "InvokeSup"
val ReceiveSup     = "ReceiveSup"
val ReplySup       = "ReplySup"
val GetReplySup    = "GetReplySup"
val FreezeSub      = "FreezeSub"
val FreezingSub    = "FreezingSub"
val ThawSub        = "ThawSub"

(*    For names                *)
val inst_id                    = "inst_id"
val inst_id1                   = "inst_id1"
val inst_id2                   = "inst_id2"
val inst_id3                   = "inst_id3"
val inst_id4                   = "inst_id4"
val inst_id_invoker            = "inst_id_invoker"
val inst_id_invoked            = "inst_id_invoked"
val inst_id_invoked1           = "inst_id_invoked1"
val inst_id_invoked2           = "inst_id_invoked2"
val inst_id_top                = "inst_id_top"
val inst_id_top_invoker        = "inst_id_top_invoker"
val inst_id_top_invoked        = "inst_id_top_invoked"
val inst_id_sup                = "inst_id_sup"
val inst_id_sub                = "inst_id_sub"
val scope                      = "scope"
val scope1                     = "scope1"
val scope2                     = "scope2"
val scope3                     = "scope3"
val scope4                     = "scope4"
val active_scopes              = "active_scopes"
val active_scopes_invoker      = "active_scopes_invoker"
val active_scopes_invoked      = "active_scopes_invoked"
val active_scopes_sup          = "active_scopes_sup"
val active_scopes_sub          = "active_scopes_sub"
val active_sub_scopes          = "active_sub_scopes"
val scopes_invoked             = "scopes_invoked"
val scopes_invoker             = "scopes_invoker"
val f                          = "f"
val t                          = "t"
val name                       = "name"
val proc_name                  = "proc_name"
val sub_name                   = "sub_name"
val var                        = "var"
val var_scope                  = "var_scope"
val outvar                     = "outvar"
val outvar_scope               = "outvar_scope"
val invar                      = "invar"
val invar_scope                = "invar_scope"
val oper                       = "oper"
val partner_link               = "partner_link"
val partner_link_invoker       = "partner_link_invoker"
val partner_link_invoked       = "partner_link_invoked"
val partner_link_scope         = "partner_link_scope"
val partner_link_scope_invoker = "partner_link_scope_invoker"
val partner_link_scope_invoked = "partner_link_scope_invoked"
val sub                        = "sub"
val sub_scope                  = "sub_scope"
val sub_link                   = "sub_link"
val sub_link_scope             = "sub_link_scope"
val echo                       = "echo"
val echo_id                    = "echo_id"
val echo_process               = "echo_process"
val echo_value                 = "echo_value"
val echo_service               = "echo_service"
val echo_client                = "echo_client"
val x                          = "x"
val y                          = "y"
val z                          = "z"
val v                          = "v"
val caller                     = "caller"
val caller_id                  = "caller_id"



(*******************************)
(*          Signature          *)
(*******************************)
(* The binding ports of a Process
 *
 *   #1 should be used to delimit the scope of variables within the
 *        process to the process itself.
 *
 * The free ports
 *
 *   #1 should be connected to the name of the process.
 *)
val Process      = passive  (Process     =: 1 --> 1);

(* The free ports of an Instance
 * 
 *   #1 should be connected to the name of the process.
 *   #2 is the instance identifier which (among other things) is used to
 *        determine the scope of variables within the instance to the
 *        instance itself. 
 *   #3 if the instance has a parent the port should be connected to the
 *        state node of the parent instance (like active scopes).
 *)
val Instance     = active   (Instance    -:       3);

(*(* SubProcesses is just a container node for SubProcess nodes.
 *)
val SubProcesses = active0  (SubProcesses          );*)

(*(* We use a different control for sub processes than processes to
 * prevent the invokation of sub processes using the Invoke activity -
 * in particular it prevents a process from invoking sub processes of
 * other processes.
 * 
 * The binding ports of a SubProcess
 *
 *   #1 should be used to delimit the scope of variables within the
 *        process to the process itself.
 *
 * The free ports should be connected
 *
 *   #1 to the name of the process.
 *   (#2 to the scope port of the node delimiting its scope.)
 *)
val SubProcess   = passive  (SubProcess  =: 1 --> 1);*)

(* Instances is just a container node for Instance nodes.
 *)
val Instances    = active0  (Instances             );

(*(* SubInstances is just a container node for SubInstance nodes.
 *)
val SubInstances = active0  (SubInstances          );*)

(*(* The free ports of a SubInstance
 * 
 *   #1 should be connected to the name of the process.
 *   #2 is the sub-instance identifier analogous to the instance
 *        identifier for instances.
 *   #3 should be connected to the state node of the immediately
 *        enclosing instance (like active scopes).
 *)
val SubInstance  = active   (SubInstance -:       3);*)

(*(* The binding ports of a FrozenSub
 *
 *   #1 should be used to delimit the scope of variables within the
 *        process to the process itself.
 *
 * The free ports should be connected
 *
 *   #1 to the name of the process.
 *)
val FrozenSub    = passive  (FrozenSub   =: 1 --> 1);*)

(* The following controls are used to control the execution of an
 * instance and all its subinstances as a whole. They are used to make
 * the freeze and exit activities atomic though several transitions are
 * required to perform them.
 *
 *   TopRunning:    indicates that the instance and its children are
 *                    allowed to take normal transitions.
 *
 *   SubTransition: indicates that a multi-transition activity is in
 *                    progress within the instance.
 *
 * The free ports of a TopRunning or SubTransition node
 *
 *   #1 should be connected to the instance identifier of the enclosing
 *        top-level instance
 *)
val TopRunning    = atomic   (TopRunning    -:       1);
val SubTransition = atomic   (SubTransition -:       1);

(* The following controls are used to keep track of the state of
 * individual instances:
 *
 *   Invoked:  an instance which has just been instantiated and still
 *               needs to copy the contents of the invoke message into
 *               the proper variable before beginning execution.
 *
 *   Running:  an executing instance.
 *
 *   Freezing: an instance in the process of being frozen.
 *
 *   Stopped:  a stopped instance which must not take any steps and
 *               should be garbage collected.
 *
 * The free ports of an Invoked, Running, Freezing, or Stopped node
 * should be connected:
 *
 *   #1 to the scope port of the parent process/instance.
 *   #2 to all the ActiveScope and SubProcess nodes of the instance.
 *   #3 to the instance identifier of the enclosing top-level instance
 *)
val Invoked      = atomic   (Invoked     -:       3);
val Running      = atomic   (Running     -:       3);
val Freezing     = atomic   (Freezing    -:       3);
val Stopped      = atomic   (Stopped     -:       3);

(* The binding ports of a Scope
 *
 *   #1 is used to delimit the scope of variables within the scope to
 *        the scope itself.
 *
 * The free ports
 *
 *   #1 should be connected to the scope port of the parent
 *        process/instance.
 *)
val Scope        = passive  (Scope       =: 1 --> 1);

(* Scopes have to be initialized before they can be used. An ActiveScope
 * is a scope that has has been initialized.
 *
 * The free ports of an ActiveScope
 *
 *   #1 is used to delimit the scope of variables within the scope to
 *        the scope itself.
 *   #2 should be connected to the state node of the instance.
 *)
val ActiveScope  = active   (ActiveScope -:       2);

(* Variables is just a container node for Variable nodes.
 *)
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

(* The free ports of the main node (Sequence, Flow, While, If, Assign)
 * of a structural activity should be connected
 * 
 *   #1 to the instance identifier
 *)
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

(* The free ports of a To or From node should be connected
 *
 *   #1 to a variable name.
 *   #2 to the scope port of the node delimiting the variable's scope.
 *)
val To           = atomic   (To          -:       2);
val From         = atomic   (From        -:       2);

(* The free ports of a ToPLink or FromPLink node should be connected
 *
 *   #1 to a partner link name.
 *   #2 to the scope port of the node delimiting the partner link's scope.
 *)
val ToPLink      = atomic   (ToPLink     -:       2);
val FromPLink    = atomic   (FromPLink   -:       2);

(* The free ports of an Invoke node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation to be invoked
 *   #4 to the name of the input variable
 *   #5 to the same scope port as the input variable
 *   #6 to the name of the output variable
 *   #7 to the same scope port as the output variable
 *   #8 to the instance identifier
 *)
val Invoke       = atomic   (Invoke      -:       8);

(* The free ports of an InvokeSub node should be connected:
 * 
 *   #1 to the name of the sub-link
 *   #2 to the same scope port as the sub-link
 *   #3 to the name of the operation to be invoked
 *   #4 to the name of the input variable
 *   #5 to the same scope port as the input variable
 *   #6 to the name of the output variable
 *   #7 to the same scope port as the output variable
 *   #8 to the instance identifier
 *)
val InvokeSub    = atomic   (InvokeSub   -:       8);

(* The free ports of an InvokeSup node should be connected:
 * 
 *   #1 to the name of the operation to be invoked
 *   #2 to the name of the input variable
 *   #3 to the same scope port as the input variable
 *   #4 to the name of the output variable
 *   #5 to the same scope port as the output variable
 *   #6 to the instance identifier
 *)
val InvokeSup    = atomic   (InvokeSup   -:       6);

(* PartnerLinks is just a container node for PartnerLink nodes.
 *)
val PartnerLinks = active0  (PartnerLinks          );
(* The free ports of a PartnerLink node should be connected:
 *
 *   #1 to the name of the partner link
 *   #2 to the scope port of the node delimiting its scope
 *)
val PartnerLink  = passive  (PartnerLink -:       2);

(* SubLinks is just a container node for SubLink nodes.
 *)
val SubLinks     = active0  (SubLinks              );
(* The free ports of a SubLink node should be connected:
 *
 *   #1 to the name of the sub link
 *   #2 to the scope port of the node delimiting its scope
 *)
val SubLink      = passive  (SubLink     -:        2);
(* The connection between a sub-link and a sub-instance cannot be
 * maintained when the surrounding instance is frozen, since the
 * sub-instance has to be frozen as well which means its instance-id
 * must be bound.
 * Thus we break the link, and instead insert a FrozenSupLink in the
 * frozen sub-instance, which remembers which sub-link it was connected
 * to.
 *
 * The free ports of a FrozenSupLink node should be connected:
 *
 *   #1 to the name of the sub link
 *   #2 to the scope port of the node delimiting its scope
 *)
val FrozenSupLink = atomic (FrozenSupLink -: 2);

(* The free port of a Message should be connected:
 *
 *   #1 to the name of the operation the message pertains to
 *)
val Message      = passive  (Message     -:        1);

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
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation
 *   #4 to the name of the variable
 *   #5 to the same scope port as the variable
 *   #6 to the instance identifier
 *)
val Receive      = atomic   (Receive     -:       6);

(* The free ports of a ReceiveSub node should be connected:
 * 
 *   #1 to the name of the sub-link
 *   #2 to the same scope port as the sub-link
 *   #3 to the name of the operation
 *   #4 to the name of the variable
 *   #5 to the same scope port as the variable
 *   #6 to the instance identifier
 *)
val ReceiveSub   = atomic   (ReceiveSub  -:       6);

(* The free ports of a ReceiveSup node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the name of the variable
 *   #3 to the same scope port as the variable
 *   #4 to the instance identifier
 *)
val ReceiveSup   = atomic   (ReceiveSup  -:       4);

(* The free ports of a Reply node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation
 *   #4 to the name of the variable
 *   #5 to the same scope port as the variable
 *   #6 to the instance identifier of its enclosing instance
 *)
val Reply        = atomic   (Reply       -:       6);

(* The free ports of a ReplySub node should be connected:
 * 
 *   #1 to the name of the sub-link
 *   #2 to the same scope port as the sub-link
 *   #3 to the name of the operation
 *   #4 to the name of the variable
 *   #5 to the same scope port as the variable
 *   #6 to the instance identifier of its enclosing instance
 *)
val ReplySub     = atomic   (ReplySub    -:       6);

(* The free ports of a ReplySup node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the name of the variable
 *   #3 to the same scope port as the variable
 *   #4 to the instance identifier of its enclosing instance
 *)
val ReplySup     = atomic   (ReplySup    -:       4);

(* The free ports of a GetReply node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation
 *   #4 to the name of the output variable
 *   #5 to the same scope port as the output variable
 *   #6 to the instance identifier of its enclosing instance
 *)
val GetReply     = atomic   (GetReply    -:       6);

(* As a sub-link doesn't uniquely identify the sub-instance to which the
 * corresponding InvokeSub sent a message, GetReplySub needs a direct
 * link to the instance identifier of that particular sub-instance.
 *
 * The free ports of a GetReplySub node should be connected:
 * 
 *   #1 to the name of the sub-link
 *   #2 to the same scope port as the sub-link
 *   #3 to the instance identifier of the sub-instance
 *   #4 to the name of the operation
 *   #5 to the name of the output variable
 *   #6 to the same scope port as the output variable
 *   #7 to the instance identifier of its enclosing instance
 *)
val GetReplySub  = atomic   (GetReplySub -:       7);

(* The free ports of a GetReplySup node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the name of the output variable
 *   #3 to the same scope port as the output variable
 *   #4 to the instance identifier of its enclosing instance
 *)
val GetReplySup  = atomic   (GetReplySup -:       4);

(* The free ports of an Exit node should be connected
 *
 *   #1 to the instance identifier of the enclosing instance.
 *)
val Exit         = atomic   (Exit        -:       1);

(* Freezing a sub-instance means stopping its execution and moving it to
 * a variable.
 *
 * The free ports of an FreezeSub node should be connected
 *
 *   #1 to the name of the sub-link connected to the instance which
 *        should be frozen.
 *   #2 to the same scope port as the sub-link.
 *   #3 to the name of the variable where the frozen sub-instance should
 *        be stored.
 *   #4 to the same scope port as the above variable.
 *   #5 to the instance identifier of the enclosing instance.
 *)
val FreezeSub    = atomic   (FreezeSub   -:       5);

(* While waiting for the sub-instance to be frozen, the FreezeSub
 * activity is changed to a FreezingSub activity.
 *
 * The free ports of an FreezingSub node should be connected
 *
 *   #1 to the name of the sub-link connected to the instance which
 *        should be frozen.
 *   #2 to the same scope port as the sub-link.
 *   #3 to the name of the variable where the frozen sub-instance should
 *        be stored.
 *   #4 to the same scope port as the above variable.
 *   #5 to the instance identifier of the enclosing instance.
 *)
val FreezingSub  = atomic   (FreezingSub -:       5);

(* Thawing a sub-instance means restarting its execution by moving it
 * from a variable and connecting it to a sub-link.
 *
 * The free ports of an FreezeSub node should be connected
 *
 *   #1 to the name of the sub-link to connect to the instance which
 *        is thawed.
 *   #2 to the same scope port as the sub-link.
 *   #3 to the name of the variable where the frozen sub-instance is
 *        stored.
 *   #4 to the same scope port as the above variable.
 *   #5 to the instance identifier of the enclosing instance.
 *)
val ThawSub      = atomic   (ThawSub      -:       5);


(*******************************)
(*        Reaction rules       *)
(*******************************)

(* Structural activities *)

(* When a Flow is completed (i.e. there are no more instructions in the
 * flow to be executed) we garbage collect the flow.  In the same manner,
 * we garbage collect a Sequence if the current instruction is completed.
 * We then make the following instruction the next to be executed.
 *)
val rule_flow_completed = "flow completed" :::

   Flow[inst_id] o <->
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  ----|>
   <->
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];


val rule_sequence_completed = "sequence completed" :::

   Sequence[inst_id] o Next o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  ----|>
   `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];

(* The rules for evaluating an if-then-else statement is as expected. If
 * the condition is True we execute the then-branch, otherwise we execute
 * the else-branch.
 *)
val rule_if_true = "if true" :::

   If[inst_id] o (    Condition o True
                  `|` Then o `[]`
                  `|` Else o `[]`)
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  ----|>
   `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];


val rule_if_false = "if false" :::

   If[inst_id] o (    Condition o False
                  `|` Then o `[]`
                  `|` Else o `[]`)
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  --[0 |-> 1]--|>
   `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];

(* We give semantics to a while-loop in the traditional manner, by
 * unfolding the loop once and using an if-then-else construct with the
 * loop condition.
 *)
val rule_while_unfold = "while unfold" :::

   While[inst_id] o (Condition o `[]` `|` `[]`)
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]

  --[2 |-> 0, 3 |-> 1]--|>

   If[inst_id] o (    Condition o `[]`
                  `|` Then o Sequence[inst_id]
                             o (`[]` 
                                `|` Next
                                    o While[inst_id]
                                      o (Condition o `[]` `|` `[]`))
                  `|` Else o <->)
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];


(* Expression evaluation *)

(* Our current formalization only supports one type of expressions,
 * namely variable references. But one can easily extend the semantics
 * to more expression types, simply by adding rules describing how to
 * evaluate them -- without having to alter the current rules.
 *)

(* A variable reference is evaluated by locating the referenced variable,
 * using its name and "scope"-link, and then replacing the VariableRef
 * node by the current content of the variable.
 *)
val rule_variable_reference = "variable reference" :::

   VariableRef[var, var_scope, inst_id]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]

  --[1 |-> 0]--|>

   `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];


(* Assignment *)
(* As for expressions, the semantics of assignment only supports one
 * type of assignment, but more can be supported by adding rules
 * describing their semantics.
 *)

(* The assign copy activity copies the content of a Variable/PartnerLink
 * to a Variable/PartnerLink.
 *)
val rule_assign_copy_var2var = "assign copy var2var" :::

   Assign[inst_id] o Copy o (    From[f, scope1]
                             `|` To[t, scope2])
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]

  --[1 |-> 0]--|>

   <->
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];

val rule_assign_copy_var2plink = "assign copy var2plink" :::

   Assign[inst_id] o Copy o (    From[f, scope1]
                             `|` ToPLink[t, scope2])
|| Variable[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]

  --[1 |-> 0]--|>

   <->
|| Variable[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];

val rule_assign_copy_plink2var = "assign copy plink2var" :::

   Assign[inst_id] o Copy o (    FromPLink[f, scope1]
                             `|` To[t, scope2])
|| PartnerLink[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]

  --[1 |-> 0]--|>

   <->
|| PartnerLink[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];

val rule_assign_copy_plink2plink = "assign copy plink2plink" :::

   Assign[inst_id] o Copy o (    FromPLink[f, scope1]
                             `|` ToPLink[t, scope2])
|| PartnerLink[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]

  --[1 |-> 0]--|>

   <->
|| PartnerLink[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];



(* Scope *)

(* When we ``execute'' a scope we replace the passive Scope node with an
 * active node of control ActiveScope and we replace the binding port
 * with an edge (which we call scope).
 *)
val rule_scope_activation = "scope activation" :::

   Scope[inst_id][[scope]] o `[scope]`
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  ----|>
   -/scope o (ActiveScope[scope, active_scopes] o `[scope]`)
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];

(* When we are finished executing the body and all sub-instances of the
 * scope we remove the scope, including its variables, partner links,
 * and its associated "scope"-edge. *)
val rule_scope_completed = "scope completed" :::

   -/scope
   o (ActiveScope[scope, active_scopes]
      o (    Variables    o (scope//[scope1] o `[scope1]`)
         `|` PartnerLinks o (scope//[scope2] o `[scope2]`)
         `|` SubLinks     o (scope//[scope3] o `[scope3]`)
         `|` Instances o <->))
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  ----|>
   <->
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top];




(* Process termination *)
(* Processes can terminate in two ways:
 * 1) normally, i.e. when no more activities remain, or 2) abnormally by
 * executing an Exit activity.
 *)

(* In the first case, we simply remove the instance in the same way as
 * for scopes.
 *)
val rule_inst_completed = "inst completed" :::

   Instance[proc_name, inst_id, active_scopes_sup]
   o (    Variables    o (inst_id//[inst_id1] o `[inst_id1]`)
      `|` PartnerLinks o (inst_id//[inst_id2] o `[inst_id2]`)
      `|` SubLinks     o (inst_id//[inst_id3] o `[inst_id3]`)
      `|` Instances    o <->
      `|` Running[inst_id, active_scopes, inst_id_top])
|| TopRunning[inst_id_top]

  ----|>

   <-> || proc_name//[] || inst_id//[]
       || active_scopes_sup//[] || active_scopes//[]
|| TopRunning[inst_id_top];

(*val rule_inst_completed = "inst completed" :::

-//[inst_id, active_scopes]
o (Instance[proc_name, inst_id, active_scopes_sup]
   o (    Variables    o (inst_id//[inst_id1] o `[inst_id1]`)
      `|` PartnerLinks o (inst_id//[inst_id2] o `[inst_id2]`)
      `|` SubLinks     o (inst_id//[inst_id3] o `[inst_id3]`)
      `|` Instances    o <->
      `|` Running[inst_id, active_scopes, inst_id]
      `|` TopRunning[inst_id]))
  ----|>
<-> || proc_name//[] || active_scopes_sup//[];
*)
(*
(* we do the same for sub-instances, and detach the attached sub-link. *)
val rule_sub_completed = "sub completed" :::

   -//[inst_id, active_scopes]
   o (    SubLinks
          o (SubLink[sub_link, sub_link_scope] o Link[inst_id] `|` `[]`)
      `|` Instances
          o (    Instance[proc_name, inst_id, active_scopes_sup]
                 o (    Variables    o (inst_id//[inst_id1] o `[inst_id1]`)
                    `|` PartnerLinks o (inst_id//[inst_id2] o `[inst_id2]`)
                    `|` SubLinks     o (inst_id//[inst_id3] o `[inst_id3]`)
                    `|` Instances    o <->
                    `|` Running[inst_id, active_scopes, inst_id_top])
             `|` `[]`))
|| TopRunning[inst_id_top]

  --[1 |-> 4]--|>

   proc_name//[] || active_scopes_sup//[]
|| (    SubLinks
        o (SubLink[sub_link, sub_link_scope] o <-> `|` `[]`)
    `|` Instances o `[]`)
|| TopRunning[inst_id_top];
*)

(* In the case of an Exit activity we change the status of the
 * (sub-)instance from running to stopped by replacing the Running node
 * inside the instance with a Stopped node. This prevents other rules
 * from being used, except for the rule "exit remove instance" below, 
 * effectively stopping any other activity from proceeding.
 *)
val rule_exit_stop_inst = "exit stop inst" :::

   Exit[inst_id]
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  ----|>
   <->
|| Stopped[inst_id, active_scopes, inst_id_top]
|| SubTransition[inst_id_top];

(* Once an Instance node contains a Stopped node we garbage collect the
 * instance together with all its remaining content.
 *)
val rule_exit_remove_inst = "exit remove inst" :::

   Instance[proc_name, inst_id, active_scopes_sup]
   o (    Stopped[inst_id, active_scopes, inst_id_top]
      `|` `[inst_id, active_scopes]`)
|| SubTransition[inst_id_top]

  ----|>

   <-> || proc_name//[] || inst_id//[]
       || active_scopes_sup//[] || active_scopes//[]
|| TopRunning[inst_id_top];

(* If a top-level instance has completed or has been stopped, we
 * garbage-collect the associated TopRunning node. *)
val rule_gc_top_running = "gc top running" :::

-/inst_id_top o TopRunning[inst_id_top]

  ----|>

<->;

(*(* We do the same for sub-instances, and detach the attached sub-link. *)
val rule_exit_remove_sub = "exit remove sub" :::

   -//[inst_id, active_scopes]
   o (    SubLinks
          o (SubLink[sub_link, sub_link_scope] o Link[inst_id] `|` `[]`)
      `|` Instances
          o (    SubInstance[proc_name, inst_id, active_scopes_sup]
                 o (    Stopped[inst_id, active_scopes, inst_id_top]
                    `|` `[inst_id, active_scopes]`)
             `|` `[]`))
|| SubTransition[inst_id_top]

  --[1 |-> 2]--|>

   proc_name//[] || active_scopes_sup//[]
|| (   SubLinks o (SubLink[sub_link, sub_link_scope] o <-> `|` `[]`)
    `|` Instances o `[]`)
|| TopRunning[inst_id_top] handle e => explain e;*)



(* Process communication *)
(* Our formalization includes synchronous request-response communication,
 * which is achieved in BPEL using, in order, the invoke, receive, and
 * reply activities. There are two cases: the receive can either 1) be an
 * activity of a running instance, or 2) it can create a new instance of
 * a process.
 *
 * The first case is implemented by the ``invoke instance'' rule which
 * handles both the invoke and receive in one step, while the second is
 * modeled by two rules: ``invoke'' and ``receive''.
 *)

(* The invoke rule represents the case where an Invoke activity is
 * executed inside a running instance and we have a process
 * with the appropriate operation available and marked as being able to
 * create new instances. The reactum 1) replaces the Invoke activity in
 * the calling instance with a GetReply activity, which is used to
 * represent that the instance is waiting for the reply, and 2) creates
 * a new instance with the body of the process definition and the value
 * of the input variable in a Message node within the relevant
 * PartnerLink node. The partner links are updated to reflect the
 * connection between the two instances: A Link node is inserted into
 * the PartnerLink nodes of the instances, with a connection to the
 * scope link of the other instance.
 *)
val rule_invoke = "invoke" :::

   Invoke[partner_link_invoker, partner_link_scope_invoker, oper,
          invar, invar_scope, outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker] o <->
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker, active_scopes_invoker, inst_id_top_invoker]
|| TopRunning[inst_id_top_invoker]
|| Process[proc_name][[scope]]
   o (    PartnerLinks
          o (    PartnerLink[partner_link_invoked, scope]
                 o (CreateInstance[oper] `|` `[]`)
             `|` scope//[scope1] o `[scope1]`)
      `|` scope//[scope2] o `[scope2]`)

  --[4 |-> 0, 5&[inst_id_invoked1] |--> 2&[scope1],
     6&[inst_id_invoked2] |--> 3&[scope2]]--|>

-/inst_id_invoked
o (   GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
               outvar, outvar_scope, inst_id_invoker]
   || PartnerLink[partner_link_invoker, partner_link_scope_invoker]
      o Link[inst_id_invoked]
   || Variable[invar, invar_scope] o `[]`
   || Running[inst_id_invoker, active_scopes_invoker, inst_id_top_invoker]
   || TopRunning[inst_id_top_invoker]
   || (Process[proc_name][[scope]]
       o (    PartnerLinks
              o (    PartnerLink[partner_link_invoked, scope]
                     o (CreateInstance[oper] `|` `[]`)
                 `|` scope//[scope1] o `[scope1]`)
          `|` scope//[scope2] o `[scope2]`)
       `|` -/active_scopes_sup
           o Instance[proc_name, inst_id_invoked, active_scopes_sup]
             o (    PartnerLinks
                    o (    PartnerLink[partner_link_invoked, inst_id_invoked]
                           o (Link[inst_id_invoker] `|` Message[oper] o `[]`)
                       `|` inst_id_invoked//[inst_id_invoked1]
                           o `[inst_id_invoked1]`)
                `|` -/active_scopes
                    o Invoked[inst_id_invoked, active_scopes, inst_id_invoked]
                `|` inst_id_invoked//[inst_id_invoked2]
                    o `[inst_id_invoked2]`)
       `|` SubTransition[inst_id_invoked]));


(* The receive rule takes care of activating the instance, by removing a
 * receive node associated to the partner link and the operation
 * (indicated by the link of the Message), copying the content of the
 * Message in the PartnerLink to the proper input variable, and changing
 * the Invoked node to a Running node.
 *)
val rule_receive = "receive" :::

   Receive[partner_link, partner_link_scope, oper, var, var_scope, inst_id]
|| PartnerLink[partner_link, partner_link_scope]
   o (`[]` `|` Message[oper] o `[]`)
|| Variable[var, var_scope] o `[]`
|| Invoked[inst_id, active_scopes, inst_id]
|| SubTransition[inst_id]

  ----|>

   <-> || oper//[]
|| PartnerLink[partner_link, partner_link_scope]
   o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id, active_scopes, inst_id]
|| TopRunning[inst_id];


(* The invoke instance rule executes an Invoke activity in one instance
 * simultaneously with a corresponding Receive activity in another
 * instance, replacing the Invoke with a GetReply activity and removing
 * the Receive. The invoking process' PartnerLink is used to identify
 * the receiving instance. The content of the output variable is copied
 * to the appropriate variable of the receiving instance.
 *)
val rule_invoke_instance = "invoke_instance" :::

   Invoke[partner_link_invoker, partner_link_scope_invoker, oper,
          invar, invar_scope, outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker, active_scopes_invoker, inst_id_top_invoker]
|| TopRunning[inst_id_top_invoker]
|| Receive[partner_link_invoked, partner_link_scope_invoked, oper,
           var, var_scope, inst_id_invoked]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked, active_scopes_invoked, inst_id_top_invoked]
|| TopRunning[inst_id_top_invoked]

  --[2 |-> 1]--|>

   GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker, active_scopes_invoker, inst_id_top_invoker]
|| TopRunning[inst_id_top_invoker]
|| <-> || partner_link_invoked//[] || partner_link_scope_invoked//[]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked, active_scopes_invoked, inst_id_top_invoked]
|| TopRunning[inst_id_top_invoked];


(* The Reply activity inside one instance can synchronize together with
 * a GetReply activity inside another instance, thereby copying the
 * content from variable var to variable outvar.
 *)
val rule_reply = "reply" :::

   Reply[partner_link_invoked, partner_link_scope_invoked, oper,
         var, var_scope, inst_id_invoked]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked]
   o (Link[inst_id_invoker] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked, active_scopes_invoked, inst_id_top_invoked]
|| TopRunning[inst_id_top_invoked]
|| GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker]
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker, active_scopes_invoker, inst_id_top_invoker]
|| TopRunning[inst_id_top_invoker]

  --[2 |-> 1]--|>

   <-> || oper//[]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked]
   o (Link[inst_id_invoker] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked, active_scopes_invoked, inst_id_top_invoked]
|| TopRunning[inst_id_top_invoked]
|| <-> || partner_link_invoker//[] || partner_link_scope_invoker//[]
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker, active_scopes_invoker, inst_id_top_invoker]
|| TopRunning[inst_id_top_invoker];



(* Communication between parent and child instances *)
(* Send a message from a parent instance to a sub-instance using the
 * invokeSub and receiveSup activities respectively.
 *)
val rule_invoke_sub = "invoke sub" :::

   InvokeSub[sub_link, sub_link_scope, oper, invar, invar_scope,
             outvar, outvar_scope, inst_id_sup]
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| ReceiveSup[oper, var, var_scope, inst_id_sub]
|| Variable[var, var_scope]
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| TopRunning[inst_id_top]

  --[2 |-> 1]--|>

   GetReplySub[sub_link, sub_link_scope, inst_id_sub, oper,
               outvar, outvar_scope, inst_id_sup]
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| <->
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| TopRunning[inst_id_top];


(* The ReplySup activity inside one instance can synchronize together
 * with a GetReplySub activity inside another instance, thereby copying
 * the content from variable var to variable outvar.
 *)
val rule_reply_sup = "reply sup" :::

   ReplySup[oper, var, var_scope, inst_id_sub]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| GetReplySub[sub_link, sub_link_scope, inst_id_sub, oper,
               outvar, outvar_scope, inst_id_sup]
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top]

  --[2 |-> 0]--|>

   <-> || oper//[]
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| <->
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top];


(* Symmetrically, a sub-instance can invoke its parent. *)
(* Send a message from a sub-instance to its parent instance using the
 * invokeSup and receiveSub activities respectively.
 *)
val rule_invoke_sup = "invoke sup" :::

   InvokeSup[oper, invar, invar_scope, outvar, outvar_scope, inst_id_sub]
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| ReceiveSub[sub_link, sub_link_scope, oper, var, var_scope, inst_id_sup]
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[var, var_scope]
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top]

  --[2 |-> 0]--|>

   GetReplySup[oper, outvar, outvar_scope, inst_id_sub]
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| <->
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top];


(* The ReplySub activity inside one instance can synchronize together
 * with a GetReplySup activity inside a sub-instance, thereby copying
 * the content from variable var to variable outvar.
 *)
val rule_reply_sub = "reply sub" :::

   ReplySub[sub_link, sub_link_scope, oper, var, var_scope, inst_id_sup]
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| GetReplySup[oper, outvar, outvar_scope, inst_id_sub]
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| TopRunning[inst_id_top]

  --[2 |-> 1]--|>

   <-> || oper//[]
|| SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| <->
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_sub, active_scopes_sub, inst_id_top]
|| TopRunning[inst_id_top];



(* Sub-instance hibernation *)
(* Freezing a sub-instance requires several transitions, initiated by a
 * FreezeSub activity.
 *
 * FIXME give tactic describing the intended use of the freezing rules
 *)
val rule_freeze_sub = "freeze sub" :::

   FreezeSub[sub_link, sub_link_scope, var, var_scope, inst_id_sup]
|| (    SubLinks o (    SubLink[sub_link, sub_link_scope]
                        o (Link[inst_id_sub] `|` `[]`)
                    `|` `[]`)
    `|` Instances
        o (    Instance[sub_name, inst_id_sub, active_scopes_sup]
               o (Running[inst_id_sub, active_scopes_sub, inst_id_top] `|` `[]`)
           `|` `[]`))
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top]

  ----|>

   FreezingSub[sub_link, sub_link_scope, var, var_scope, inst_id_sup]
|| (    SubLinks o (    SubLink[sub_link, sub_link_scope]
                        o (Link[inst_id_sub] `|` `[]`)
                    `|` `[]`)
    `|` Instances
        o (    Instance[sub_name, inst_id_sub, active_scopes_sup]
               o (Freezing[inst_id_sub, active_scopes_sub, inst_id_top] `|` `[]`)
           `|` `[]`))
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| SubTransition[inst_id_top];


(* An active scope can be frozen when all nested scopes and
 * sub-instances have been frozen. This is ensured by requiring that the
 * content of the scope does not refer to the active-scopes link of the
 * enclosing sub-instance.
 *)
val rule_freeze_scope = "freeze scope" :::

-/active_scopes
o (   Freezing[inst_id, active_scopes, inst_id_top]
   || -/scope o (ActiveScope[scope, active_scopes] o `[scope]`)
   || `[active_scopes]`)

  ----|>

-/active_scopes
o (   Freezing[inst_id, active_scopes, inst_id_top]
   || Scope[inst_id][[scope]] o `[scope]`
   || `[active_scopes]`);


(* Sub-instances of a sub-instance which is being frozen, are frozen by
 * propagating the freezing state, which again allows its scopes and
 * subinstances to be frozen.
 *)
val rule_freeze_sub_instance = "freeze sub instance" :::

   Freezing[inst_id, active_scopes, inst_id_top]
|| Instance[sub_name, inst_id_sub, active_scopes]
   o (    Running[inst_id_sub, active_scopes_sub, inst_id_top]
      `|` `[]`)

  ----|>

   Freezing[inst_id, active_scopes, inst_id_top]
|| Instance[sub_name, inst_id_sub, active_scopes]
   o (    Freezing[inst_id_sub, active_scopes_sub, inst_id_top]
      `|` `[]`);

(* When all those are frozen, ie. the active_scopes link of the
 * sub-sub-instance is only connected to the state node, the
 * sub-sub-instance is frozen (remaining at the same location) and a
 * FrozenSupLink is inserted in the frozen instance to remember which
 * SubLink it was connected to.
 *)
val rule_freeze_sub_instance2 = "freeze sub instance2" :::

-/inst_id_sub
o (   Freezing[inst_id_sup, active_scopes, inst_id_top]
   || (    SubLinks o (    SubLink[sub_link, sub_link_scope]
                           o (Link[inst_id_sub] `|` `[]`)
                       `|` `[]`)
       `|` Instances
           o (    Instance[sub_name, inst_id_sub, active_scopes]
                  o (    -/active_scopes_sub
                         o Freezing[inst_id_sub, active_scopes_sub, inst_id_top]
                     `|` `[inst_id_sub]`)
              `|` `[]`)))

  ----|>

   Freezing[inst_id_sup, active_scopes, inst_id_top]
|| (    SubLinks o (SubLink[sub_link, sub_link_scope] o `[]` `|` `[]`)
    `|` Instances
        o (    Process[sub_name][[inst_id_sub]]
               o (    FrozenSupLink[sub_link, sub_link_scope]
                  `|` `[inst_id_sub]`)
           `|` `[]`));


(* When no more sub-instances and scopes are connected to the
 * active_scopes link of the sub-instance being frozen, it can itself be
 * frozen and placed into the proper variable.
 *)
val rule_freeze_complete = "freeze complete" :::

-/inst_id_sub
o (   FreezingSub[sub_link, sub_link_scope, var, var_scope, inst_id_sup]
   || Variable[var, var_scope] o `[]`
   || SubLink[sub_link, sub_link_scope] o (Link[inst_id_sub] `|` `[]`)
   || Running[inst_id_sup, active_scopes_sup, inst_id_top]
   || SubTransition[inst_id_top]
   || Instance[sub_name, inst_id_sub, active_scopes_sup]
      o (    -/active_scopes_sub
             o Freezing[inst_id_sub, active_scopes_sub, inst_id_top]
         `|` `[inst_id_sub]`))

  --[0 |-> 2]--|>

   <->
|| Variable[var, var_scope]
   o Process[sub_name][[inst_id_sub]] o `[inst_id_sub]`
|| SubLink[sub_link, sub_link_scope] o `[]`
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top]
|| <->;


(* Thawing *)
val rule_thaw_sub = "thaw sub" :::

   ThawSub[sub_link, sub_link_scope, var, var_scope, inst_id_sup]
|| Variable[var, var_scope]
   o Process[sub_name][[sub_scope]] o `[sub_scope]`
|| (    SubLinks o (SubLink[sub_link, sub_link_scope] o `[]` `|` `[]`)
    `|` Instances o `[]`)
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top]

  --[4&[inst_id_sub] |--> 0&[sub_scope]]--|>

   <->
|| Variable[var, var_scope]
   o Process[sub_name][[sub_scope]] o `[sub_scope]`
|| -/inst_id_sub
   o (    SubLinks o (    SubLink[sub_link, sub_link_scope]
                          o (Link[inst_id_sub] `|` `[]`)
                      `|` `[]`)
      `|` Instances
          o (    `[]`
             `|` Instance[sub_name, inst_id_sub, active_scopes_sup]
                 o (    -/active_scopes_sub
                        o Running[inst_id_sub, active_scopes_sub, inst_id_top]
                    `|` `[inst_id_sub]`)))
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top];


(* Thaw a sub-instance which was frozen in place (i.e. it is a
 * descendant of a sub-instance which was frozen).
 * 
 * This rule is the inverse of rule_freeze_sub_instance2.
 *)
val rule_thaw_sub_instance = "thaw sub instance" :::

   (    SubLinks o (SubLink[sub_link, sub_link_scope] o `[]` `|` `[]`)
    `|` Instances
        o (    Process[sub_name][[inst_id_sub]]
               o (    FrozenSupLink[sub_link, sub_link_scope]
                  `|` `[inst_id_sub]`)
           `|` `[]`))
|| Running[inst_id_sup, active_scopes_sup, inst_id_top]
|| TopRunning[inst_id_top]

  ----|>

-/inst_id_sub
o (   (    SubLinks o (    SubLink[sub_link, sub_link_scope]
                           o (Link[inst_id_sub] `|` `[]`)
                       `|` `[]`)
       `|` Instances
           o (    Instance[sub_name, inst_id_sub, active_scopes_sup]
                  o (    -/active_scopes_sub
                         o Running[inst_id_sub, active_scopes_sub, inst_id_top]
                     `|` `[inst_id_sub]`)
              `|` `[]`))
   || Running[inst_id_sup, active_scopes_sup, inst_id_top]
   || TopRunning[inst_id_top]);




val rules =
    mkrules [rule_scope_activation, rule_scope_completed,
             rule_flow_completed, rule_sequence_completed,
             rule_if_true, rule_if_false, rule_while_unfold,
             rule_assign_copy_var2var,
	     rule_assign_copy_var2plink,
	     rule_assign_copy_plink2var,
	     rule_assign_copy_plink2plink,
             rule_invoke,
             rule_receive,
             rule_invoke_instance, rule_reply,
             rule_exit_stop_inst, rule_exit_remove_inst,
             rule_inst_completed];

val tactic = roundrobin;

(* The rest is unchanged from the GT-VMT version. *)

(*******************************)
(*       Example processes     *)
(*******************************)
(*
(* A simple echo process: it provides an operation "echo" which receives
 * a value and sends the value back.
 *
 * <process name="echo_process">
 * <partnerLinks>
 *   <partnerLink name="echo_client" />
 * </partnerLinks>
 * <variables>
 *   <variable name="x" />
 * </variables>
 * <sequence>
 *   <receive partnerLink="echo_client" operation="echo"
 *            createInstance="yes" variable="x" />
 *   <reply   partnerLink="echo_client" operation="echo" variable="x" />
 *   <exit>
 * </sequence>
 * </process>
 *)
val echo_process1 =
Process[echo_process][[echo_id]]
o (    PartnerLinks o PartnerLink[echo_client, echo_id] o CreateInstance[echo]
   `|` Variables o Variable[x, echo_id] o <->
   `|` Sequence[echo_id]
       o (    Receive[echo_client, echo_id, echo, x, echo_id, echo_id]
          `|` Next o Sequence[echo_id]
                   o (    Reply[echo_client, echo_id, echo, x, echo_id, echo_id]
                      `|` Next o Exit[echo_id])));

(* An instance which is about to invoke the echo process:
 * 
 * <instance name"caller">
 * <partnerLinks>
 *   <partnerLink name="echo_service" />
 * </partnerLinks>
 * <variables>
 *   <variable name="y"><from>true()</from></variable>
 *   <variable name="z"><from>false()</from></variable>
 * </variables>
 * <invoke partnerLink="echo_service" operation="echo"
 *         inputVariable="y" outputVariable="z" />
 * </instance>
 *)
val caller_inst1 =
-//[caller_id, caller_active_scopes]
o (Instance[caller, caller_id]
   o (    Running[caller_id, caller_active_scopes]
      `|` PartnerLinks o PartnerLink[echo_service, caller_id] o <->
      `|` Variables
           o (    Variable[y, caller_id] o True
              `|` Variable[z, caller_id] o False)
          `|` Invoke[echo_service, caller_id, echo,
                     y, caller_id, z, caller_id, caller_id]));


(* A slightly more advanced echo process: 
 * It's initiated by the operation "echo" and, if the received value isn't
 * False, then provides an operation "echo_value" to the calling process
 * which receives values and sends them back. If the value received was
 * False the process replies False to the initial "echo" operation and exits.
 *
 * <process name="echo_process">
 * <partnerLinks>
 *   <partnerLink name="echo_client" />
 * </partnerLinks>
 * <variables>
 *   <variable name="x" />
 * </variables>
 * <sequence>
 *   <receive partnerLink="echo_client" operation="echo"
 *            createInstance="yes" variable="x" />
 *   <reply   partnerLink="echo_client" operation="echo" variable="x" />
 *   <while>
 *     <condition>$x</condition>
 *     <scope>
 *       <variables>
 *         <variable name="y" />
 *       </variables>
 *       <sequence>
 *         <receive partnerLink="echo_client" operation="echo_value" variable="y" />
 *         <reply   partnerLink="echo_client" operation="echo_value" variable="y" />
 *         <assign>
 *           <copy>
 *             <from variable="y" />
 *             <to variable="x" />
 *           </copy>
 *         </assign>
 *       </sequence>
 *     </scope>
 *   </while>
 * </sequence>
 * </process>
 *)
val while_loop =
While[echo_id]
o (    Condition o VariableRef[x, echo_id, echo_id]
   `|` Scope[echo_id][[scope]]
       o (    PartnerLinks o <->
          `|` Variables    o Variable[y, scope] o <->
          `|` Sequence[echo_id]
              o (    Receive[echo_client, echo_id, echo_value, y, scope, echo_id]
                 `|` Next
                     o Sequence[echo_id]
                     o (    Reply[echo_client, echo_id, echo_value,
                                  y, scope, echo_id]
                        `|` Next
                            o Assign[echo_id]
                            o Copy o (    From[y, scope]
                                      `|` To[x, echo_id])))));
val echo_process2_context = 
Process[echo_process][[echo_id]]
o (    PartnerLinks o PartnerLink[echo_client, echo_id] o CreateInstance[echo]
   `|` Variables    o Variable[x, echo_id] o <->
   `|` Sequence[echo_id]
       o (    Receive[echo_client, echo_id, echo, x, echo_id, echo_id]
          `|` Next
              o Sequence[echo_id]
              o (    Reply[echo_client, echo_id, echo, x, echo_id, echo_id]
                 `|` Next o `[]`)));

val echo_process2 = echo_process2_context o while_loop;
val echo_process2_emptyloop = echo_process2_context o While[echo_id] o <->;

(* An instance which is about to invoke the advanced echo process:
 * 
 * <instance name"caller">
 * <partnerLinks>
 *   <partnerLink name="echo_service" />
 * </partnerLinks>
 * <variables>
 *   <variable name="z"><from>true()</from></variable>
 *   <variable name="v"><from>false()</from></variable>
 * </variables>
 * <flow>
 *   <sequence>
 *     <invoke partnerLink="echo_service" operation="echo"
 *             inputVariable="z" outputVariable="v" />
 *     <invoke partnerLink="echo_service" operation="echo_value"
 *             inputVariable="v" outputVariable="v" />
 *   </sequence>
 *   <exit />
 * </flow>
 * </instance>
 *)
val caller_inst2 =
-//[caller_id, caller_active_scopes]
o (Instance[caller, caller_id]
   o (    Running[caller_id]
      `|` PartnerLinks o PartnerLink[echo_service, caller_id] o <->
      `|` Variables
          o (    Variable[z, caller_id] o True
             `|` Variable[v, caller_id] o False)
      `|` Flow[caller_id]
          o (    Sequence[caller_id]
                 o (    Invoke[echo_service, echo,
                               z, caller_id, v, caller_id, caller_id]
                    `|` Next o Invoke[echo_service, echo_value,
                                      v, caller_id, v, caller_id, caller_id])
             `|` Exit[caller_id])));


val _ = use_shorthands on;

(*
val mz1 = matches (mkrules [rule_reply]) (caller_inst1 `|` echo_process1);
val mz2 = matches (mkrules [rule_invoke]) (caller_inst1 `|` echo_process1);
val mz3 = matches (mkrules [rule_invoke]) (caller_inst1 `|` echo_process1);
print_mv mz2;*)

(*val final_state = run rules tactic (echo_process || caller_inst);*)
(*val final_state = run rules (react_rule "invoke") (caller_inst1 `|` echo_process1);*)

(*
val state1_0 = caller_inst1 `|` echo_process1
val state2_0 = caller_inst2 `|` echo_process2;
val state_0 = caller_inst2 `|` echo_process2_emptyloop;

val tac_invoke =
  react_rule "invoke" ++
  react_rule "receive" ++
  react_rule "sequence completed"

val state1_invokedz = stepz rules tac_invoke state1_0;
(*val state2_invokedz = stepz rules tac_invoke state2_0;*)
val state_invokedz = stepz rules tac_invoke state_0;

val tac_while =
  react_rule "while unfold" ++
  react_rule "variable reference" ++
  react_rule "if true" ++
  react_rule "scope activation" ++
  react_rule "invoke" ++
  react_rule "invoke receive" ++
  react_rule "sequence completed" ++
  react_rule "reply" ++
  react_rule "sequence completed" ++
  react_rule "variable copy" ++
  react_rule "sequence completed"

fun state_unroll1z state_invoked = stepz rules tac_while state_invoked

fun showsteps ((rulename, agent), t) = (
  print (
    "----------------------------------------------------\n\
    \New agent after using rule " ^ rulename ^
    "\n\n" ^ str_v agent ^ "\n");
  fn _ => t () agent)
fun init agent = agent

(*
val state1_invoked = lzfoldr showsteps init state1_invokedz <->;
val _ = outputsvgdoc_v "state1_invoked.svg" (SOME smallcfg) state1_invoked;

val state2_invoked = lzfoldr showsteps init state2_invokedz <->;
val _ = outputsvgdoc_v "state2_invoked.svg" (SOME smallcfg) state2_invoked;
val state2_unroll1 =
  lzfoldr showsteps init (state2_unroll1z state2_invoked);
val _ = outputsvgdoc_v "state2_unroll1.svg" (SOME smallcfg) state2_unroll1;
*)

val state_invoked = lzfoldr showsteps init state_invokedz <->;
val _ = outputsvgdoc_v "state_invoked.svg" (SOME smallcfg) state_invoked;

val Body = atomic("Body" -: 5);
 
local
  open BG.BgBDNF
  val glob = BG.Interface.glob
  val Wir = BG.BgVal.Wir (BG.Info.noinfo)
  val state_invoked_b = make state_invoked;
  val {wirxid, D}        = unmkB state_invoked_b
  val {ren, Ps, perm}    = unmkD D
  val {id_Z, Y, s, X, N} = unmkP (hd Ps)
  val {absnames, G}      = unmkN N
  val {idxmerge, Ss}     = unmkG G
  val caller   = List.nth (Ss, 0)
  val proc     = List.nth (Ss, 1)
  val instance = List.nth (Ss, 2)
  val wir_X = glob (BG.BgVal.innerface wirxid)
  val intro = Wir (BG.Wiring.introduce wir_X)
in
  val state_invoked_caller   = wirxid o (unmk caller || intro)
  val state_invoked_proc     = unmk (List.nth (Ss, 1))
  val state_invoked_instance = wirxid o (unmk instance || intro)
end;
val _ =
  outputsvgdoc_v
    "state_invoked_caller.svg" (SOME smallcfg) state_invoked_caller;
val _ =
  outputsvgdoc_v
    "state_invoked_instance.svg" (SOME smallcfg) state_invoked_instance;
val _ =
  outputtikz_v
    "state_invoked_caller.tex" (SOME 0.017) (SOME smallcfg) state_invoked_caller;
val _ =
  outputtikz_v
    "state_invoked_instance.tex" (SOME 0.017) (SOME smallcfg) state_invoked_instance;
*)
*)
