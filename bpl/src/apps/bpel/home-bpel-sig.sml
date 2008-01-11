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
 *   #1 should be connected to the instance identifier of the instance
 *)
val TopRunning    = atomic   (TopRunning    -:       1);
val SubTransition = atomic   (SubTransition -:       1);

(* To keep top-level instances related to the associated TopRunning/
 * SubTransition node, we encapsulate them in a TopInstance node.
 *)
val TopInstance   = active0  (TopInstance             );

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
val SubLink      = passive  (SubLink     -:       2);
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
val FrozenSupLink = atomic  (FrozenSupLink -:     2);

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

(* Since it is possible to copy partner-links, an instance can be
 * invoked by previously unknown partners. When invoked, it therefor
 * needs a way to "remember" whom to reply to -- this is accomplished
 * by placing a ReplyTo node in the relevant partner-link.
 *
 * The free ports of a ReplyTo node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the instance identifier of the instance to reply to.
 *)
val ReplyTo      = atomic   (ReplyTo     -:       2);

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
 * The free ports of an Freeze node should be connected
 *
 *   #1 to the name of the sub-link connected to the instance which
 *        should be frozen.
 *   #2 to the same scope port as the sub-link.
 *   #3 to the name of the variable where the frozen sub-instance should
 *        be stored.
 *   #4 to the same scope port as the above variable.
 *   #5 to the instance identifier of the enclosing instance.
 *)
val Freeze       = atomic   (Freeze      -:       5);

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
 * The free ports of a Thaw node should be connected
 *
 *   #1 to the name of the sub-link to connect to the instance which
 *        is thawed.
 *   #2 to the same scope port as the sub-link.
 *   #3 to the name of the variable where the frozen sub-instance is
 *        stored.
 *   #4 to the same scope port as the above variable.
 *   #5 to the instance identifier of the enclosing instance.
 *)
val Thaw         = atomic   (Thaw         -:       5);
