(*******************************)
(*        Reaction rules       *)
(*******************************)

(* Structural activities *)

(* Mikkel: New version. First gc Next when there is no predecessors *)
val rule_sequence_completed = "sequence completed" :::

    -//[p] o Next[p, id] o `[]` `|` Run[id] 
--[0 |-> 0]--|>
    `[]` `|` Run[id];

(* The rules for evaluating an if-then-else statement is as expected. If
 * the condition is True we execute the then-branch, otherwise we execute
 * the else-branch.
 *)
val rule_if_true = "if true" :::

    If[id, s] o ( Cond o True `|` Then o `[]` `|` Else o `[]`) `|` Run[id]
  --[0 |-> 0]--|>
    `[]` `|` s//[] `|` Run[id];


val rule_if_false = "if false" :::

    If[id, s] o (    Cond o False `|` Then o `[]` `|` Else o `[]`) `|` Run[id]
  --[0 |-> 1]--|>
   `[]` `|` s//[]
`|` Run[id];

(* We give semantics to a while-loop in the traditional manner, by
 * unfolding the loop once and using an if-then-else construct with the
 * loop condition.
 *)
(* Mikkel: Just a test, I am not sure it works *)
(* The local name pred should refer to the last activities in the while-loop *)
val rule_while_unfold = "while unfold" :::

    While[id, s][[p]] o (Cond o `[]` `|` `[p]`) `|` Run[id]

  --[0 |-> 0, 1&[p1] |--> 1&[p], 2 |-> 0, 3&[p2] |--> 1&[p]]--|>
(*  --[0 |-> 0, 1 |-> 1, 2 |-> 0, 3 |-> 1]--|> *)

    If[id, s] o (    Cond o `[]` `|` Then o ( -//[p1] o ( `[p1]` `|` Next[p1, id] o
                                    While[id, s][[p2]]o (Cond o `[]` `|` `[p2]`)))`|` Else o <->)
`|` Run[id];


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

    Ref[n, sc, id]  `|` Var[n, sc] o `[]` `|` Run[id]
  --[0 |-> 0, 1 |-> 0]--|>
    `[]` `|` Var[n, sc] o `[]` `|` Run[id];


(* Assignment *)
(* As for expressions, the semantics of assignment only supports one
 * type of assignment, but more can be supported by adding rules
 * describing their semantics.
 *)

(* The assign copy activity copies the content inside a Variable/PartnerLink
 * to a Variable/PartnerLink.
 *)
val rule_assign_copy_var2var = "assign copy var2var" :::

    Ass[id, s, f, scf, t, sct] `|` Var[f, scf] o `[]` `|` Var[t, sct] o `[]`
`|` Run[id]

  --[0 |-> 0, 1 |-> 0]--|>

    <-> `|` s//[] `|` Var[f, scf] o `[]` `|` Var[t, sct] o `[]` `|` Run[id];

(* Thomas: The following variations are not needed when we collaps links and vars *)
val rule_assign_copy_var2plink = "assign copy var2plink" :::

    Assign[inst_id, succ] o Copy o (    From[f, scope1]
                                    `|` ToPLink[t, scope2])
`|` Variable[f, scope1] o `[]`
`|` PartnerLink[t, scope2] o `[]`
`|` Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

    <-> `|` succ//[]
`|` Variable[f, scope1] o `[]`
`|` PartnerLink[t, scope2] o `[]`
`|` Running[inst_id];

val rule_assign_copy_plink2var = "assign copy plink2var" :::

    Assign[inst_id, succ] o Copy o (    FromPLink[f, scope1]
                                    `|` To[t, scope2])
`|` PartnerLink[f, scope1] o `[]`
`|` Variable[t, scope2] o `[]`
`|` Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

    <-> `|` succ//[]
`|` PartnerLink[f, scope1] o `[]`
`|` Variable[t, scope2] o `[]`
`|` Running[inst_id];

val rule_assign_copy_plink2plink = "assign copy plink2plink" :::

    Assign[inst_id, succ] o Copy o (    FromPLink[f, scope1]
                                    `|` ToPLink[t, scope2])
`|` PartnerLink[f, scope1] o `[]`
`|` PartnerLink[t, scope2] o `[]`
`|` Running[inst_id]

  --[0 |-> 0, 1 |-> 0]--|>

    <-> `|` succ//[]
`|` PartnerLink[f, scope1] o `[]`
`|` PartnerLink[t, scope2] o `[]`
`|` Running[inst_id];



(* Scope *)

(* When we ``execute'' a scope we replace the passive Scope node with an
 * active node of control ActiveScope and we replace the binding port
 * with an edge (which we call scope).
 *)
(* ActiveScope is unnecessary *)
val rule_scope_activation = "scope activation" :::

    Scope[id][[sc]] o `[sc]`
`|` Run[id]
  --[0 |-> 0]--|>
    -//[sc] o `[sc]`
`|` Run[id];

(* When we are finished executing the body of the scope we remove the
 * scope, including its variables, partner links, and its associated
 * "scope"-edge. *)
(* Hmm, can we make garbage collection work when ActiveScope etc. is removed? *)
(* Mikkel: As far as I can see we can garbage collect a variable when it *)
(* is the only entity refering a scope + name. *)

(*val rule_scope_completed = "scope completed" :::

   ActiveScope[inst_id, scope]
   o (Variables o `[]`  `|`  PartnerLinks o `[]`)
|| Running[inst_id]
  ----|>
   <-> || scope//[]
|| Running[inst_id];
*)



(* Process termination *)
(* Processes can terminate in two ways:
 * 1) normally, i.e. when no more activities remain, or 2) abnormally by
 * executing an Exit activity.
 *)

(* In the first case, we simply remove the instance in the same way as
 * for scopes.
 *)
(* Hmm, can we make garbage collection work when Instance etc. is removed? *)
(*val rule_inst_completed = "inst completed" :::

Instance[proc_name, inst_id]
o (Variables o `[]`  `|`  PartnerLinks o `[]`  `|`  Running[inst_id])
  ----|>
<-> || proc_name//[] || inst_id//[];
*)

(* In the case of an Exit activity we change the status of the instance
 * from running to stopped by replacing the Running node inside the
 * instance with a Stopped node. This prevents other rules from being
 * used, except for the rule "exit remove instance" below, effectively
 * stopping any other activity from proceeding.
 *)
val rule_exit_stop_inst = "exit stop inst" :::

    Exit[inst_id, succ]
`|` Run[inst_id]
  ----|>
    <-> `|` succ//[]
`|` Stopped[inst_id];

(* Once an Instance node contains a Stopped node we garbage collect the
 * instance together with all its remaining content.
 *)
(* Hmm, what should this be replaced by? *)
(*val rule_exit_remove_inst = "exit remove inst" :::

Instance[proc_name, inst_id]
o (Stopped[inst_id] `|` `[]`)
  ----|>
<-> || proc_name//[] || inst_id//[];
*)
(* Maybe garbage collect nodes connected to a Stopped node? E.g. *)
(* val rule_gc_reply = "gc reply" ::: *)
(*    Reply[partner_link, partner_link_scope, operation, *)
(*          variable, variable_scope, inst_id] *)
(* || Stopped[inst_id] *)
(*   ----|> *)
(*    <-> *)
(* || Stopped[inst_id]; *)
(* ... *)
(* and then at the end gc Stopped? *)
(* val rule_gc_stopped = "gc stopped" ::: *)
(*    -//[inst_id] o Stopped[inst_id] *)
(*   ----|> *)
(*    <->; *)

(* Thomas: Why don't we exit and garbage collect all in one go ? *)
val rule_gc_all_at_once = "gc all atonce" :::
   -//[id] o (Run[id] `|` Exit[id,s] `|` `[id]`)
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

    Send[l1, lsc, op, v, vsc, id1, s] 
`|` Var[l1, lsc] o <->
`|` Var[v, vsc] o `[(*0*)]`
`|` Run[id1]
`|` Process[n][[sc]]
   o (    Var[l2, sc]
                 o (CreateInstance[oper] `|` `[(*1*)]`)
      `|` `[(*2*)scope]`)

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 0,
     4&[id2] |--> 2&[scope]]--|>

-//[inst_id_invoked]
o (    GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
               outvar, outvar_scope, inst_id_invoker, succ]
   `|` PartnerLink[partner_link_invoker, partner_link_scope_invoker]
       o Link[inst_id_invoked]
   `|` Variable[invar, invar_scope] o `[(*0*)]`
   `|` Running[inst_id_invoker]
   `|` (Process[proc_name][[scope]]
       o (    PartnerLink[partner_link, scope]
                     o (CreateInstance[oper] `|` `[(*1*)]`)
          `|` `[(*2*)scope]`)
       `|`    PartnerLink[partner_link, inst_id_invoked]
                  o (    Link[inst_id_invoker]
                     `|` Message[oper] o `[(*3*)]`
                     `|` ReplyTo[oper, inst_id_invoker])
              `|` `[(*4*)inst_id_invoked]`
              `|` Invoked[inst_id_invoked]));


(* The receive rule takes care of activating the instance, by removing a
 * receive node associated to the partner link and the operation
 * (indicated by the link of the Message), copying the content of the
 * Message in the PartnerLink to the proper input variable, and changing
 * the Invoked node to a Running node.
 *)
val rule_receive = "receive" :::

    Receive[partner_link, partner_link_scope, oper, var, var_scope, inst_id, succ]
`|` PartnerLink[partner_link, partner_link_scope]
    o (`[]` `|` Message[oper] o `[]`)
`|` Variable[var, var_scope] o `[]`
`|` Invoked[inst_id]

  --[0 |-> 0, 1 |-> 1]--|>

    <-> `|` oper//[] `|` succ//[]
`|` PartnerLink[partner_link, partner_link_scope]
    o `[]`
`|` Variable[var, var_scope] o `[]`
`|` Running[inst_id];


(* The invoke instance rule executes an Invoke activity in one instance
 * simultaneously with a corresponding Receive activity in another
 * instance, replacing the Invoke with a GetReply activity and removing
 * the Receive. The invoking process' PartnerLink is used to identify
 * the receiving instance. The content of the output variable is copied
 * to the appropriate variable of the receiving instance.
 *)
val rule_invoke_instance = "invoke_instance" :::

    Invoke[partner_link_invoker, partner_link_scope_invoker, oper,
           invar, invar_scope, outvar, outvar_scope, inst_id_invoker, succ1]
`|` PartnerLink[partner_link_invoker, partner_link_scope_invoker]
    o (Link[inst_id_invoked] `|` `[]`)
`|` Variable[invar, invar_scope] o `[]`
`|` Running[inst_id_invoker]
`|` Receive[partner_link_invoked, partner_link_scope_invoked, oper,
            var, var_scope, inst_id_invoked, succ2]
`|` PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
`|` Variable[var, var_scope] o `[]`
`|` Running[inst_id_invoked]

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>

    GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
             outvar, outvar_scope, inst_id_invoker, succ1]
`|` PartnerLink[partner_link_invoker, partner_link_scope_invoker]
    o (Link[inst_id_invoked] `|` `[]`)
`|` Variable[invar, invar_scope] o `[]`
`|` Running[inst_id_invoker]
`|` <-> `|` succ2//[]
`|` PartnerLink[partner_link_invoked, partner_link_scope_invoked]
    o (`[]` `|` ReplyTo[oper, inst_id_invoker])
`|` Variable[var, var_scope] o `[]`
`|` Running[inst_id_invoked];


(* The Reply activity inside one instance can synchronize together with
 * a GetReply activity inside another instance, thereby copying the
 * content from variable var to variable outvar.
 *)
val rule_reply = "reply" :::

    Reply[partner_link_invoked, partner_link_scope_invoked, oper,
          var, var_scope, inst_id_invoked, succ1]
`|` PartnerLink[partner_link_invoked, partner_link_scope_invoked]
    o (ReplyTo[oper, inst_id_invoker] `|` `[]`)
`|` Variable[var, var_scope] o `[]`
`|` Running[inst_id_invoked]
`|` GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker, succ2]
`|` Variable[outvar, outvar_scope] o `[]`
`|` Running[inst_id_invoker]

  --[0 |-> 0, 1 |-> 1, 2 |-> 1]--|>

    <-> `|` oper//[] `|` succ1//[]
`|` PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
`|` Variable[var, var_scope] o `[]`
`|` Running[inst_id_invoked]
`|` <-> `|` partner_link_invoker//[] `|` partner_link_scope_invoker//[]
`|` succ2//[]
`|` Variable[outvar, outvar_scope] o `[]`
`|` Running[inst_id_invoker];


val rules =
    mkrules [rule_scope_activation, rule_sequence_completed,
             rule_if_true, rule_if_false, rule_while_unfold,
             rule_variable_reference,
             rule_assign_copy_var2var,
             rule_assign_copy_var2plink,
             rule_assign_copy_plink2var,
             rule_assign_copy_plink2plink,
             rule_invoke,
             rule_receive,
             rule_invoke_instance, rule_reply,
             rule_exit_stop_inst (*, rule_exit_remove_inst, *)
             (* rule_inst_completed *)];
