(*******************************)
(*        Reaction rules       *)
(*******************************)

(* Structural activities *)

(* We garbage collect a Sequence when the current instruction is
 * completed.  We then make the following instruction the next to
 * be executed.
 *)
val rule_sequence_completed = "sequence completed" :::

   Sequence[inst_id] o (`[]` `|` -//[p] o Next[p] o `[]`)
|| Running[inst_id]
  --[0 |-> 0, 1 |-> 1]--|>
   `[]` `|` `[]`
|| Running[inst_id];

(* The rules for evaluating an if-then-else statement is as expected. If
 * the condition is True we execute the then-branch, otherwise we execute
 * the else-branch.
 *)
val rule_if_true = "if true" :::

   If[inst_id, s] o (    Condition o True
                     `|` Then o `[]`
                     `|` Else o `[]`)
|| Running[inst_id]
  --[0 |-> 0]--|>
   s//[] `|` `[]`
|| Running[inst_id];


val rule_if_false = "if false" :::

   If[inst_id, s] o (    Condition o False
                  `|` Then o `[]`
                  `|` Else o `[]`)
|| Running[inst_id]
  --[0 |-> 1]--|>
   s//[] `|` `[]`
|| Running[inst_id];

(* We give semantics to a while-loop in the traditional manner, by
 * unfolding the loop once and using an if-then-else construct with the
 * loop condition.
 *)
val rule_while_unfold = "while unfold" :::

   While[inst_id, s1][[s2]] o (Condition o `[]` `|` `[s2]`)
|| Running[inst_id]

  --[0 |-> 0, 1&[s3] |--> 1&[s2], 2 |-> 0, 3&[s2] |--> 1&[s2]]--|>

   If[inst_id, s1] o (    Condition o `[]`
                      `|` Then o Sequence[inst_id] o ( -//[s3] o (
                                 `[s3]` 
                             `|` Next[s3] o
                                   While[inst_id, s1][[s2]]
                                   o (Condition o `[]` `|` `[s2]`)))
                      `|` Else o <->)
|| Running[inst_id];


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
|| Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id];


(* Assignment *)
(* As for expressions, the semantics of assignment only supports one
 * type of assignment, but more can be supported by adding rules
 * describing their semantics.
 *)

(* The assign copy activity copies the content inside a Variable/PartnerLink
 * to a Variable/PartnerLink.
 *)
val rule_assign_copy_var2var = "assign copy var2var" :::

   Assign[inst_id, s] o Copy o (    From[f, scope1]
                                `|` To[t, scope2])
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   s//[] `|` <->
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id];

val rule_assign_copy_var2plink = "assign copy var2plink" :::

   Assign[inst_id, s] o Copy o (    From[f, scope1]
                                `|` ToPLink[t, scope2])
|| Variable[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   s//[] `|` <->
|| Variable[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id];

val rule_assign_copy_plink2var = "assign copy plink2var" :::

   Assign[inst_id, s] o Copy o (    FromPLink[f, scope1]
                                `|` To[t, scope2])
|| PartnerLink[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   s//[] `|` <->
|| PartnerLink[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id];

val rule_assign_copy_plink2plink = "assign copy plink2plink" :::

   Assign[inst_id, s] o Copy o (    FromPLink[f, scope1]
                                `|` ToPLink[t, scope2])
|| PartnerLink[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

   s//[] `|` <->
|| PartnerLink[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id];



(* Scope *)

(* When we ``execute'' a scope we replace the passive Scope node with an
 * active node of control ActiveScope and we replace the binding port
 * with an edge (which we call scope).
 *)
val rule_scope_activation = "scope activation" :::

   Scope[inst_id][[scope]] o `[scope]`
|| Running[inst_id]
  --[0 |-> 0]--|>
   -//[scope] o (ActiveScope[inst_id, scope] o `[scope]`)
|| Running[inst_id];
(* Scopes in newly created instances are also allowed to be activated. *)
val rule_scope_activation2 = "scope activation 2" :::

   Scope[inst_id][[scope]] o `[scope]`
|| Invoked[inst_id]
  --[0 |-> 0]--|>
   -//[scope] o (ActiveScope[inst_id, scope] o `[scope]`)
|| Invoked[inst_id];


(* Process termination *)
(* Processes can terminate in two ways:
 * 1) normally, i.e. when no more activities remain, or 2) abnormally by
 * executing an Exit activity.
 *)

(* In the case of an Exit activity we change the status of the instance
 * from running to stopped by replacing the Running node inside the
 * instance with a Stopped node. This prevents other rules from being
 * used, except for the rule "exit remove instance" below, effectively
 * stopping any other activity from proceeding.
 *)
val rule_exit_stop_inst = "exit stop inst" :::

   Exit[inst_id, s]
|| Running[inst_id]
  ----|>
   s//[] `|` <->
|| Stopped[inst_id];

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
          invar, invar_scope, outvar, outvar_scope, inst_id_invoker, s]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker] o <->
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker]
|| Process[proc_name][[scope]]
   o (    PartnerLinks
          o (    PartnerLink[partner_link, scope]
                 o (CreateInstance[oper] `|` `[]`)
             `|` scope//[scope1] o `[scope1]`)
      `|` scope//[scope2] o `[scope2]`)

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 3,
     4 |-> 0, 5&[inst_id_invoked1] |--> 2&[scope1],
     6&[inst_id_invoked2] |--> 3&[scope2]]--|>

-//[inst_id_invoked]
o (   GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
               outvar, outvar_scope, inst_id_invoker, s]
   || PartnerLink[partner_link_invoker, partner_link_scope_invoker]
      o Link[inst_id_invoked]
   || Variable[invar, invar_scope] o `[]`
   || Running[inst_id_invoker]
   || Process[proc_name][[scope]]
      o (    PartnerLinks
             o (    PartnerLink[partner_link, scope]
                    o (CreateInstance[oper] `|` `[]`)
                `|` scope//[scope1] o `[scope1]`)
         `|` scope//[scope2] o `[scope2]`)
  `|` Instance[proc_name, inst_id_invoked]
      o (    PartnerLinks
             o (    PartnerLink[partner_link, inst_id_invoked]
                    o (    Link[inst_id_invoker]
                       `|` Message[oper] o `[]`
                       `|` ReplyTo[oper, inst_id_invoker])
                `|` inst_id_invoked//[inst_id_invoked1]
                    o `[inst_id_invoked1]`)
         `|` Invoked[inst_id_invoked]
         `|` inst_id_invoked//[inst_id_invoked2]
             o `[inst_id_invoked2]`));


(* The receive rule takes care of activating the instance, by removing a
 * receive node associated to the partner link and the operation
 * (indicated by the link of the Message), copying the content of the
 * Message in the PartnerLink to the proper input variable, and changing
 * the Invoked node to a Running node.
 *)
val rule_receive = "receive" :::

   Receive[partner_link, partner_link_scope, oper, var, var_scope, inst_id, s]
|| PartnerLink[partner_link, partner_link_scope]
   o (`[]` `|` Message[oper] o `[]`)
|| Variable[var, var_scope] o `[]`
|| Invoked[inst_id]

  --[0 |-> 0, 1 |-> 1]--|>

   oper//[] `|` s//[] `|` <->
|| PartnerLink[partner_link, partner_link_scope]
   o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id];


(* The invoke instance rule executes an Invoke activity in one instance
 * simultaneously with a corresponding Receive activity in another
 * instance, replacing the Invoke with a GetReply activity and removing
 * the Receive. The invoking process' PartnerLink is used to identify
 * the receiving instance. The content of the output variable is copied
 * to the appropriate variable of the receiving instance.
 *)
val rule_invoke_instance = "invoke_instance" :::

   Invoke[partner_link_invoker, partner_link_scope_invoker, oper,
          invar, invar_scope, outvar, outvar_scope, inst_id_invoker, s1]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker]
|| Receive[partner_link_invoked, partner_link_scope_invoked, oper,
           var, var_scope, inst_id_invoked, s2]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>

   GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker, s1]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker]
|| s2//[] `|` <->
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked]
   o (`[]` `|` ReplyTo[oper, inst_id_invoker])
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked];


(* The Reply activity inside one instance can synchronize together with
 * a GetReply activity inside another instance, thereby copying the
 * content from variable var to variable outvar.
 *)
val rule_reply = "reply" :::

   Reply[partner_link_invoked, partner_link_scope_invoked, oper,
         var, var_scope, inst_id_invoked, s1]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked]
   o (ReplyTo[oper, inst_id_invoker] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]
|| GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker, s2]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker]

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>

   <-> || oper//[] || s1//[]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]
|| s2//[] `|` <->
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker];



val rulelist = [rule_scope_activation, rule_scope_activation2,
                rule_sequence_completed,
                rule_if_true, rule_if_false, rule_while_unfold,
                rule_variable_reference,
                rule_assign_copy_var2var,
                rule_assign_copy_var2plink,
                rule_assign_copy_plink2var,
                rule_assign_copy_plink2plink,
                rule_invoke,
                rule_receive,
                rule_invoke_instance, rule_reply,
                rule_exit_stop_inst];
val rules = mkrules rulelist; 
