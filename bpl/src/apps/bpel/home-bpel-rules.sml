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
                  `|` Then o Sequence[inst_id] o (
                               `[]` 
                         `|` Next o
                               While[inst_id]
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

   ActiveScope[scope, active_scopes]
   o (    Variables    o `[]`
      `|` PartnerLinks o `[]`
      `|` SubLinks     o `[]`
      `|` Instances o <->)
|| Running[inst_id, active_scopes, inst_id_top]
|| TopRunning[inst_id_top]
  ----|>
   <-> || scope//[]
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
   o (    Variables    o `[]`
      `|` PartnerLinks o `[]`
      `|` SubLinks     o `[]`
      `|` Instances    o <->
      `|` Running[inst_id, active_scopes, inst_id_top])
|| TopRunning[inst_id_top]

  ----|>

   <-> || proc_name//[] || inst_id//[]
       || active_scopes_sup//[] || active_scopes//[]
|| TopRunning[inst_id_top];


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
   o (Stopped[inst_id, active_scopes, inst_id_top] `|` `[]`)
|| SubTransition[inst_id_top]

  ----|>

   <-> || proc_name//[] || inst_id//[]
       || active_scopes_sup//[] || active_scopes//[]
|| TopRunning[inst_id_top];

(* If a top-level instance has completed or has been stopped, we
 * garbage-collect it. *)
val rule_top_instance_completed = "top instance completed" :::

TopInstance o (-/inst_id_top o TopRunning[inst_id_top])
  ----|>
<->;




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
   || (    Process[proc_name][[scope]]
           o (    PartnerLinks
                  o (    PartnerLink[partner_link_invoked, scope]
                         o (CreateInstance[oper] `|` `[]`)
                     `|` scope//[scope1] o `[scope1]`)
              `|` scope//[scope2] o `[scope2]`)
       `|` TopInstance
           o (    SubTransition[inst_id_invoked]
              `|` -/active_scopes_sup
                  o Instance[proc_name, inst_id_invoked, active_scopes_sup]
                    o (    PartnerLinks
                           o (    PartnerLink[partner_link_invoked, inst_id_invoked]
                                  o (    Link[inst_id_invoker]
                                     `|` Message[oper] o `[]`
                                     `|` ReplyTo[oper, inst_id_invoker])
                              `|` inst_id_invoked//[inst_id_invoked1]
                                  o `[inst_id_invoked1]`)
                       `|` -/active_scopes
                           o Invoked[inst_id_invoked, active_scopes, inst_id_invoked]
                       `|` inst_id_invoked//[inst_id_invoked2]
                           o `[inst_id_invoked2]`))));


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
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked, active_scopes_invoked, inst_id_top_invoked]
|| TopRunning[inst_id_top_invoked]

  --[3 |-> 1]--|>

   GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker, active_scopes_invoker, inst_id_top_invoker]
|| TopRunning[inst_id_top_invoker]
|| <->
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked]
   o (`[]` `|` ReplyTo[oper, inst_id_invoker])
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
   o (ReplyTo[oper, inst_id_invoker] `|` `[]`)
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
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
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
 * Freeze activity.
 *
 * FIXME give tactic describing the intended use of the freezing rules
 *)
val rule_freeze_sub = "freeze sub" :::

   Freeze[sub_link, sub_link_scope, var, var_scope, inst_id_sup]
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

   Thaw[sub_link, sub_link_scope, var, var_scope, inst_id_sup]
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
             rule_variable_reference,
             rule_assign_copy_var2var,
             rule_assign_copy_var2plink,
             rule_assign_copy_plink2var,
             rule_assign_copy_plink2plink,
             rule_invoke,
             rule_receive,
             rule_invoke_instance, rule_reply,
             rule_invoke_sub, rule_reply_sup,
             rule_invoke_sup, rule_reply_sub,
             rule_freeze_sub, rule_freeze_scope,
             rule_freeze_sub_instance, rule_freeze_sub_instance2,
             rule_freeze_complete, rule_thaw_sub,
             rule_thaw_sub_instance, 
             rule_exit_stop_inst, rule_exit_remove_inst,
             rule_inst_completed, rule_top_instance_completed];
