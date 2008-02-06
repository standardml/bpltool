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

   Flow[instanceId == inst_id] o <->
|| Running[instanceId == inst_id]
  ----|>
   <->
|| Running[instanceId == inst_id];


val rule_sequence_completed = "sequence completed" :::

   Sequence[instanceId == inst_id] o Next o `[]`
|| Running[instanceId == inst_id]
  --[0 |-> 0]--|>
   `[]`
|| Running[instanceId == inst_id];

(* The rules for evaluating an if-then-else statement is as expected. If
 * the condition is True we execute the then-branch, otherwise we execute
 * the else-branch.
 *)
val rule_if_true = "if true" :::

   If[instanceId == inst_id] o (    Condition o True
                                `|` Then o `[]`
                                `|` Else o `[]`)
|| Running[instanceId == inst_id]
  --[0 |-> 0]--|>
   `[]`
|| Running[instanceId == inst_id];


val rule_if_false = "if false" :::

   If[instanceId == inst_id] o (    Condition o False
                                `|` Then o `[]`
                                `|` Else o `[]`)
|| Running[instanceId == inst_id]
  --[0 |-> 1]--|>
   `[]`
|| Running[instanceId == inst_id];

(* We give semantics to a while-loop in the traditional manner, by
 * unfolding the loop once and using an if-then-else construct with the
 * loop condition.
 *)
val rule_while_unfold = "while unfold" :::

   While[instanceId == inst_id] o (Condition o `[]` `|` `[]`)
|| Running[instanceId == inst_id]

  --[0 |-> 0, 1 |-> 1, 2 |-> 0, 3 |-> 1]--|>

   If[instanceId == inst_id] o (    Condition o `[]`
                                `|` Then o Sequence[instanceId == inst_id] o (
                                      `[]` 
                                `|` Next o
                                      While[instanceId == inst_id]
                                      o (Condition o `[]` `|` `[]`))
                                `|` Else o <->)
|| Running[instanceId == inst_id];


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

   VariableRef[variable == var, variableScope == var_scope, instanceId == inst_id]
|| Variable[name == var, scope == var_scope] o `[]`
|| Running[instanceId == inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   `[]`
|| Variable[name == var, scope == var_scope] o `[]`
|| Running[instanceId == inst_id];


(* Assignment *)
(* As for expressions, the semantics of assignment only supports one
 * type of assignment, but more can be supported by adding rules
 * describing their semantics.
 *)

(* The assign copy activity copies the content inside a Variable/PartnerLink
 * to a Variable/PartnerLink.
 *)
val rule_assign_copy_var2var = "assign copy var2var" :::

   Assign[instanceId == inst_id]
   o Copy o (    From[variable == f, scope == scope1]
             `|` To[variable == t, scope == scope2])
|| Variable[name == f, scope == scope1] o `[]`
|| Variable[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   <->
|| Variable[name == f, scope == scope1] o `[]`
|| Variable[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id];

val rule_assign_copy_var2plink = "assign copy var2plink" :::

   Assign[instanceId == inst_id]
   o Copy o (    From[variable == f, scope == scope1]
             `|` ToPLink[partnerLink == t, scope == scope2])
|| Variable[name == f, scope == scope1] o `[]`
|| PartnerLink[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   <->
|| Variable[name == f, scope == scope1] o `[]`
|| PartnerLink[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id];

val rule_assign_copy_plink2var = "assign copy plink2var" :::

   Assign[instanceId == inst_id]
   o Copy o (    FromPLink[partnerLink == f, scope == scope1]
             `|` To[variable == t, scope == scope2])
|| PartnerLink[name == f, scope == scope1] o `[]`
|| Variable[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   <->
|| PartnerLink[name == f, scope == scope1] o `[]`
|| Variable[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id];

val rule_assign_copy_plink2plink = "assign copy plink2plink" :::

   Assign[instanceId == inst_id]
   o Copy o (    FromPLink[partnerLink == f, scope == scope1]
             `|` ToPLink[partnerLink == t, scope == scope2])
|| PartnerLink[name == f, scope == scope1] o `[]`
|| PartnerLink[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   <->
|| PartnerLink[name == f, scope == scope1] o `[]`
|| PartnerLink[name == t, scope == scope2] o `[]`
|| Running[instanceId == inst_id];



(* Scope *)

(* When we ``execute'' a scope we replace the passive Scope node with an
 * active node of control ActiveScope and we replace the binding port
 * with an edge (which we call scope).
 *)
val rule_scope_activation = "scope activation" :::

   Scope[instanceId == inst_id][scope == scope] o `[scope]`
|| Running[instanceId == inst_id]
  --[0 |-> 0]--|>
   -//[scope] o (ActiveScope[instanceId == inst_id, scope == scope] o `[scope]`)
|| Running[instanceId == inst_id];

(* When we are finished executing the body of the scope we remove the
 * scope, including its variables, partner links, and its associated
 * "scope"-edge. *)
val rule_scope_completed = "scope completed" :::

   ActiveScope[instanceId == inst_id, scope == scope]
   o (Variables o `[]`  `|`  PartnerLinks o `[]`)
|| Running[instanceId == inst_id]
  ----|>
   <-> || scope//[]
|| Running[instanceId == inst_id];




(* Process termination *)
(* Processes can terminate in two ways:
 * 1) normally, i.e. when no more activities remain, or 2) abnormally by
 * executing an Exit activity.
 *)

(* In the first case, we simply remove the instance in the same way as
 * for scopes.
 *)
val rule_inst_completed = "inst completed" :::

Instance[name == name, id == inst_id]
o (Variables o `[]`  `|`  PartnerLinks o `[]`  `|`  Running[instanceId == inst_id])
  ----|>
<-> || name//[] || inst_id//[];

(* In the case of an Exit activity we change the status of the instance
 * from running to stopped by replacing the Running node inside the
 * instance with a Stopped node. This prevents other rules from being
 * used, except for the rule "exit remove instance" below, effectively
 * stopping any other activity from proceeding.
 *)
val rule_exit_stop_inst = "exit stop inst" :::

   Exit[instanceId == inst_id]
|| Running[instanceId == inst_id]
  ----|>
   <->
|| Stopped[instanceId == inst_id];

(* Once an Instance node contains a Stopped node we garbage collect the
 * instance together with all its remaining content.
 *)
val rule_exit_remove_inst = "exit remove inst" :::

Instance[name == proc_name, id == inst_id]
o (Stopped[instanceId == inst_id] `|` `[]`)
  ----|>
<-> || proc_name//[] || inst_id//[];



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

   Invoke[partnerLink == partner_link_invoker,
          partnerLinkScope == partner_link_scope_invoker, operation == oper,
          inputVariable == invar, inputVariableScope == invar_scope,
          outputVariable == outvar, outputVariableScope == outvar_scope,
          instanceId == inst_id_invoker]
|| PartnerLink[name == partner_link_invoker,
               scope == partner_link_scope_invoker] o <->
|| Variable[name == invar, scope == invar_scope] o `[]`
|| Running[instanceId == inst_id_invoker]
|| Process[name == proc_name][scope == scope]
   o (    PartnerLinks
          o (    PartnerLink[name == partner_link, scope == scope]
                 o (CreateInstance[operation == oper] `|` `[]`)
             `|` scope//[scope1] o `[scope1]`)
      `|` scope//[scope2] o `[scope2]`)

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 3,
     4 |-> 0, 5&[inst_id_invoked1] |--> 2&[scope1],
     6&[inst_id_invoked2] |--> 3&[scope2]]--|>

-//[inst_id_invoked]
o (   GetReply[partnerLink == partner_link_invoker,
               partnerLinkScope == partner_link_scope_invoker,
               operation == oper,
               outputVariable == outvar, outputVariableScope == outvar_scope,
               instanceId == inst_id_invoker]
   || PartnerLink[name == partner_link_invoker, scope == partner_link_scope_invoker]
      o Link[partnerId == inst_id_invoked]
   || Variable[name == invar, scope == invar_scope] o `[]`
   || Running[instanceId == inst_id_invoker]
   || (Process[name == proc_name][scope == scope]
       o (    PartnerLinks
              o (    PartnerLink[name == partner_link, scope == scope]
                     o (CreateInstance[operation == oper] `|` `[]`)
                 `|` scope//[scope1] o `[scope1]`)
          `|` scope//[scope2] o `[scope2]`)
       `|` Instance[name == proc_name, id == inst_id_invoked]
           o (    PartnerLinks
                  o (    PartnerLink[name == partner_link,
                                     scope == inst_id_invoked]
                         o (    Link[partnerId == inst_id_invoker]
                            `|` Message[operation == oper] o `[]`
                            `|` ReplyTo[operation == oper,
                                        partnerId == inst_id_invoker])
                     `|` inst_id_invoked//[inst_id_invoked1]
                         o `[inst_id_invoked1]`)
              `|` Invoked[instanceId == inst_id_invoked]
              `|` inst_id_invoked//[inst_id_invoked2]
                  o `[inst_id_invoked2]`)));


(* The receive rule takes care of activating the instance, by removing a
 * receive node associated to the partner link and the operation
 * (indicated by the link of the Message), copying the content of the
 * Message in the PartnerLink to the proper input variable, and changing
 * the Invoked node to a Running node.
 *)
val rule_receive = "receive" :::

   Receive[partnerLink == partner_link, partnerLinkScope == partner_link_scope,
           operation == oper, variable == var, variableScope == var_scope,
           instanceId == inst_id]
|| PartnerLink[name == partner_link, scope == partner_link_scope]
   o (`[]` `|` Message[operation == oper] o `[]`)
|| Variable[name == var, scope == var_scope] o `[]`
|| Invoked[instanceId == inst_id]

  --[0 |-> 0, 1 |-> 1]--|>

   <-> || oper//[]
|| PartnerLink[name == partner_link, scope == partner_link_scope]
   o `[]`
|| Variable[name == var, scope == var_scope] o `[]`
|| Running[instanceId == inst_id];


(* The invoke instance rule executes an Invoke activity in one instance
 * simultaneously with a corresponding Receive activity in another
 * instance, replacing the Invoke with a GetReply activity and removing
 * the Receive. The invoking process' PartnerLink is used to identify
 * the receiving instance. The content of the output variable is copied
 * to the appropriate variable of the receiving instance.
 *)
val rule_invoke_instance = "invoke_instance" :::

   Invoke[partnerLink == partner_link_invoker,
          partnerLinkScope == partner_link_scope_invoker,
          operation == oper,
          inputVariable == invar, inputVariableScope == invar_scope,
          outputVariable == outvar, outputVariableScope == outvar_scope,
          instanceId == inst_id_invoker]
|| PartnerLink[name == partner_link_invoker, scope == partner_link_scope_invoker]
   o (Link[partnerId == inst_id_invoked] `|` `[]`)
|| Variable[name == invar, scope == invar_scope] o `[]`
|| Running[instanceId == inst_id_invoker]
|| Receive[partnerLink == partner_link_invoked,
           partnerLinkScope == partner_link_scope_invoked,
           operation == oper,
           variable == var, variableScope == var_scope,
           instanceId == inst_id_invoked]
|| PartnerLink[name == partner_link_invoked,
               scope == partner_link_scope_invoked] o `[]`
|| Variable[name == var, scope == var_scope] o `[]`
|| Running[instanceId == inst_id_invoked]

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>

   GetReply[partnerLink == partner_link_invoker,
            partnerLinkScope == partner_link_scope_invoker,
            operation == oper,
            outputVariable == outvar,
            outputVariableScope == outvar_scope,
            instanceId == inst_id_invoker]
|| PartnerLink[name == partner_link_invoker, scope == partner_link_scope_invoker]
   o (Link[partnerId == inst_id_invoked] `|` `[]`)
|| Variable[name == invar, scope == invar_scope] o `[]`
|| Running[instanceId == inst_id_invoker]
|| <->
|| PartnerLink[name == partner_link_invoked, scope == partner_link_scope_invoked]
   o (`[]` `|` ReplyTo[operation == oper, partnerId == inst_id_invoker])
|| Variable[name == var, scope == var_scope] o `[]`
|| Running[instanceId == inst_id_invoked] handle e => explain e;


(* The Reply activity inside one instance can synchronize together with
 * a GetReply activity inside another instance, thereby copying the
 * content from variable var to variable outvar.
 *)
val rule_reply = "reply" :::

   Reply[partnerLink == partner_link_invoked,
         partnerLinkScope == partner_link_scope_invoked,
         operation == oper,
         variable == var, variableScope == var_scope,
         instanceId == inst_id_invoked]
|| PartnerLink[name == partner_link_invoked, scope == partner_link_scope_invoked]
   o (ReplyTo[operation == oper, partnerId == inst_id_invoker] `|` `[]`)
|| Variable[name == var, scope == var_scope] o `[]`
|| Running[instanceId == inst_id_invoked]
|| GetReply[partnerLink == partner_link_invoker,
            partnerLinkScope == partner_link_scope_invoker,
            operation == oper,
            outputVariable == outvar, outputVariableScope == outvar_scope,
            instanceId == inst_id_invoker]
|| Variable[name == outvar, scope == outvar_scope] o `[]`
|| Running[instanceId == inst_id_invoker]

  --[0 |-> 0, 1 |-> 1, 2 |-> 1]--|>

   <-> || oper//[]
|| PartnerLink[name == partner_link_invoked,
               scope == partner_link_scope_invoked] o `[]`
|| Variable[name == var, scope == var_scope] o `[]`
|| Running[instanceId == inst_id_invoked]
|| <-> || partner_link_invoker//[] || partner_link_scope_invoker//[]
|| Variable[name == outvar, scope == outvar_scope] o `[]`
|| Running[instanceId == inst_id_invoker];




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
             rule_exit_stop_inst, rule_exit_remove_inst,
             rule_inst_completed];
