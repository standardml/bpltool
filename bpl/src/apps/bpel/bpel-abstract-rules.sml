(*******************************)
(*        Reaction rules       *)
(*******************************)

(* Structural activities *)

val rule_sequence_completed = "sequence completed" :::

    -//[p] o Next[p, id] o `[]` `|` Run[id] 
  --[0 |-> 0]--|>
    `[]` `|` Run[id];

(* The rules for evaluating an if-then-else statement is as expected. If
 * the condition is True we execute the then-branch, otherwise we execute
 * the else-branch.
 *)
val rule_if_true = "if true" :::

    If[id, s] o (Cond o True `|` Then o `[]` `|` Else o `[]`) `|` Run[id]
  --[0 |-> 0]--|>
    `[]` `|` s//[] `|` Run[id];

val rule_if_false = "if false" :::

    If[id, s] o (Cond o False `|` Then o `[]` `|` Else o `[]`) `|` Run[id]
  --[0 |-> 1]--|>
   `[]` `|` s//[] `|` Run[id];

(* We give semantics to a while-loop in the traditional manner, by
 * unfolding the loop once and using an if-then-else construct with the
 * loop condition.
 *)
val rule_while_unfold = "while unfold" :::

    While[id, s1][[s2]] o (Cond o `[]` `|` `[s2]`) `|` Run[id]

  --[0 |-> 0, 1&[s3] |--> 1&[s2], 2 |-> 0, 3&[s2] |--> 1&[s2]]--|>

    If[id, s1] o (Cond o `[]` `|` Then o (-//[s3] o (`[s3]` `|` Next[s3, id] o
    While[id, s1][[s2]] o (Cond o `[]` `|` `[s2]`))) `|` Else o <->) `|` Run[id];


(* Expression evaluation *)

(* Our current formalization only supports one type of expressions,
 * namely variable references. But one can easily extend the semantics
 * to more expression types, simply by adding rules describing how to
 * evaluate them -- without having to alter the current rules.
 *)

val rule_variable_reference = "variable reference" :::

    Ref[n, sc, id]  `|` Var[n, sc] o `[]` `|` Run[id]
  --[0 |-> 0, 1 |-> 0]--|>
    `[]` `|` Var[n, sc] o `[]` `|` Run[id];


(* Assignment *)
(* As for expressions, the semantics of assignment only supports one
 * type of assignment (copy) , but more can be supported by adding rules
 * describing their semantics.
 *)

val rule_assign = "assign copy" :::

    Ass[id, s, f, scf, t, sct] 
`|` Var[f, scf] o `[]` `|` Var[t, sct] o `[]` `|` Run[id]

  --[0 |-> 0, 1 |-> 0]--|>

    s//[] `|` Var[f, scf] o `[]` `|` Var[t, sct] o `[]` `|` Run[id];

(* Scope *)
(* Removing the scope control and inserting a fresh closed sc link instead 
 *of the bound link
 *)

val rule_scope_activation = "scope activation" :::

    Scope[id][[sc]] o `[sc]` `|` Run[id]
  --[0 |-> 0]--|>
    -//[sc] o `[sc]` `|` Run[id];

(* Process termination *)
(* Processes can terminate in two ways:
 * 1) normally, i.e. when no more activities remain, or 2) abnormally by
 * executing an Exit activity.
 *)

(* In the first case we do nothing. In the case of an Exit activity we
 * change the status of the instance
 * from running to stopped by replacing the Run node 
 * with a Stop node. This prevents other rules from being used
 *)
val rule_exit_stop_inst = "exit stop inst" :::

    Exit[id, s] `|` Run[id]
  ----|>
    s//[] `|` Stop[id];

(* we could garbage collect elements connected to Stop as Espen suggests
 * but let us leave it out for now
*)

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
 * create new instances. The reactum 1) removes the Inv activity in
 * the calling instance (the semantics ensure it is followed by a Reply
 * activity, which is used to
 * represent that the instance is waiting for the reply), and 2) creates
 * a new instance with the body of the process definition and the value
 * of the input variable in a Message node within the relevant
 * PartnerLink node. The partner links are updated to reflect the
 * connection between the two instances: A Link node is inserted into
 * the PartnerLink Var nodes of the instances, with a connection to the
 * scope link of the other instance.
 *)

val rule_invoke = "invoke" :::

    Inv[l1, lsc, oper, v, vsc, id1, s] 
`|` Var[l1, lsc] o <-> `|` Var[v, vsc] o `[(*0*)]` `|` Run[id1]
`|` Process[n][[sc]] o
    (Var[l2, sc] o (CrInst[oper] `|` `[(*1*)]`) `|` `[(*2*) sc]`)

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 0, 4&[id2] |--> 2&[sc]]--|>

 -//[id2] o ( s//[] 
`|` Var[l1, lsc] o Link[id2] `|` Var[v, vsc] o `[(*0*)]` `|` Run[id1]
`|` (Process[n][[sc]] o
    (Var[l2, sc] o (CrInst[oper] `|` `[(*1*)]`) `|` `[(*2*) sc]`)
`|` Var[l2, id2] o ( Link[id1] `|` Mess[oper] o `[(*3*)]` `|` Reply[oper, id1])
   `|` `[(*4*)id2]` `|` Invoked[id2]));


(* The receive rule takes care of activating the instance, by removing a
 * receive node associated to the partner link and the operation
 * (indicated by the link of the Message in Mess), copying the content of the
 * Message in the PartnerLink to the proper input variable, and changing
 * the Invoked node to a Run node.
 *)
val rule_receive = "receive" :::

    Rec[l, lsc, oper, v, vsc, id, s] `|` Var[l, lsc] o (`[]` `|` Mess[oper] o `[]`)
`|` Var[v, vsc] o `[]` `|` Invoked[id]

  --[0 |-> 0, 1 |-> 1]--|>

    oper//[] `|` s//[] `|` Var[l, lsc] o `[]`
`|` Var[v, vsc] o `[]` `|` Run[id];


(* The invoke instance rule executes an Invoke activity in one instance
 * simultaneously with a corresponding Receive activity in another
 * instance, replacing the Invoke with a GetReply activity and removing
 * the Receive. The invoking process' PartnerLink is used to identify
 * the receiving instance. The content of the output variable is copied
 * to the appropriate variable of the receiving instance.
 *)
val rule_invoke_instance = "invoke_instance" :::

    Inv[l1, l1sc, oper, v1 , v1sc, id1, s1]
`|` Var[l1, l1sc] o (Link[id2] `|` `[]`) `|` Var[v1, v1sc] o `[]` `|` Run[id1]
`|` Rec[l2, l2sc, oper, v2, v2sc, id2, s2] 
`|` Var[l2, l2sc] o `[]` `|` Var[v2, v2sc] o `[]` `|` Run[id2]

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>

   s1//[]
`|` Var[l1, l1sc] o (Link[id2] `|` `[]`) `|` Var[v1, v1sc] o `[]` `|` Run[id1]
`|` s2//[]
`|` Var[l2, l2sc] o (`[]` `|` Reply[oper, id1]) `|` Var[v2, v2sc] o `[]` `|` Run[id2];


(* The Rep activity inside one instance can synchronize together with
 * a GetRep activity inside another instance, thereby copying the
 * content from variable var to variable outvar.
 *)
val rule_reply = "reply" :::

    Rep[l1, l1sc, oper, v1, v1sc, id1, s1]
`|` Var[l1, l1sc] o (Reply[oper, id2] `|` `[]`) `|` Var[v1, v1sc] o `[]` `|` Run[id1]
`|` GetRep[l2, l2sc, oper, v2, v2sc, id2, s2]
`|` Var[v2, v2sc] o `[]` `|` Run[id2]

  --[0 |-> 0, 1 |-> 1, 2 |-> 1]--|>

    oper//[] `|` s1//[]
`|` Var[l1, l1sc] o `[]` `|` Var[v1, v1sc] o `[]` `|` Run[id1]
`|` l2//[] 
`|` l2sc//[] `|` s2//[] `|` Var[v2, v2sc] o `[]` `|` Run[id2];

val rulelist = [rule_scope_activation, rule_sequence_completed,
             rule_if_true, rule_if_false, rule_while_unfold,
             rule_variable_reference,
             rule_assign,
             rule_invoke,
             rule_receive,
             rule_invoke_instance, rule_reply,
             rule_exit_stop_inst];
val rules = mkrules rulelist;
