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

    If[id, s, g] o (Cond o True `|` Then o `[]` `|` Else o `[]`) `|` Run[id]
  --[0 |-> 0]--|>
    s//[] `|` g//[] `|` `[]` `|` Run[id];

val rule_if_false = "if false" :::

    If[id, s, g] o (Cond o False `|` Then o `[]` `|` Else o `[]`) `|` Run[id]
  --[0 |-> 1]--|>
    s//[] `|` g//[] `|` `[]` `|` Run[id];

(* We give semantics to a while-loop in the traditional manner, by
 * unfolding the loop once and using an if-then-else construct with the
 * loop condition.
 *)
val rule_while_unfold = "while unfold" :::

    While[id, s1, g][[s2]] o (Cond o `[]` `|` `[s2]`) `|` Run[id]

  --[0 |-> 0, 1&[s3] |--> 1&[s2], 2 |-> 0, 3&[s2] |--> 1&[s2]]--|>

    If[id, s1, g] o (Cond o `[]` `|` Then o (-//[s3] o (`[s3]` `|` Next[s3, id] o
    While[id, s1, g][[s2]] o (Cond o `[]` `|` `[s2]`))) `|` Else o <->) `|` Run[id];


(* Expression evaluation *)

(* Our current formalization only supports one type of expressions,
 * namely variable references. But one can easily extend the semantics
 * to more expression types, simply by adding rules describing how to
 * evaluate them -- without having to alter the current rules.
 *)

val rule_variable_reference = "variable reference" :::

    Ref[n, sc, id]  `|` Var[n, sc, g, id] o `[]` `|` Run[id]
  --[0 |-> 0, 1 |-> 0]--|>
    `[]` `|` Var[n, sc, g, id] o `[]` `|` Run[id];


(* Assignment *)
(* As for expressions, the semantics of assignment only supports one
 * type of assignment (copy) , but more can be supported by adding rules
 * describing their semantics.
 *)

val rule_assign = "assign copy" :::

    Ass[id, s, f, scf, t, sct, g] 
`|` Var[f, scf, g1, id] o `[]` `|` Var[t, sct, g2, id] o `[]` `|` Run[id]

  --[0 |-> 0, 1 |-> 0]--|>

    s//[] `|` g//[]
`|` Var[f, scf, g1, id] o `[]` `|` Var[t, sct, g2, id] o `[]` `|` Run[id];

(* Scope *)
(* Removing the scope control and inserting a fresh closed sc link instead 
 *of the bound link
 *)
(* !!!!!!!!!!! *)
(* Scope skal have 2 bindende porte mere: variables + activites, + 1 free til parent *)
val rule_scope_activation = "scope activation" :::

    Scope[id, g][[sc], [vars], [acts]] o (`[sc, vars]` `|` `[sc, acts]`) `|` Run[id]
  --[0 |-> 0, 1 |-> 1]--|>
    -//[sc,vars,acts] o (
        ActScope[vars, acts, g, id] `|` `[sc, vars]` `|` `[sc, acts]`) `|` Run[id];

(* Scopes can also be activated when an instance has just been created, since
 * the initial receive might be nested within scopes. *)
val rule_scope_activation2 = "scope activation2" :::

    Scope[id, g][[sc], [vars], [acts]] o (`[sc, vars]` `|` `[sc, acts]`) `|` Invoked[id]
  --[0 |-> 0, 1 |-> 1]--|>
    -//[sc,vars,acts] o (
        ActScope[vars, acts, g, id] `|` `[sc, vars]` `|` `[sc, acts]`) `|` Invoked[id];

(* Remove one variable (no more actions) *)
val rule_scope_completed = "scope completed" :::

    (-//[acts] o ActScope[vars, acts, g, id]) `|` Var[f, sc, vars, id]
  ----|>
    (-//[acts] o ActScope[vars, acts, g, id]) `|` f//[] `|` sc//[];

(* Remove ActScope (no more actions and variables *) 
val rule_scope_completed2 = "scope completed2" :::

    (-//[acts, vars] o ActScope[vars, acts, g, id])
  ----|>
    <-> `|` g//[] `|` id//[];


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

    Exit[id, s, g] `|` Run[id]
  ----|>
    s//[] `|` g//[] `|` Stop[id];


val rule_gc_scope = "gc scope" :::

    Scope[id, g][[sc], [vars], [acts]] o `[vars, sc, acts]` `|` Stop[id]
  ----|>
    g//[] `|` Stop[id];

val rule_gc_actscope = "gc ActScope" :::

    ActScope[vars, acts, g, id] `|` Stop[id]
  ----|>
    vars//[] `|` acts//[] `|` g//[] `|` Stop[id];

(* GC of Run and Invoked nodes cannot happen *)

val rule_gc_var = "gc var" :::

    Var[n, sc, g, id] `|` Stop[id]
  ----|>
    n//[] `|` sc//[] `|` g//[] `|` Stop[id];


val rule_gc_ref = "gc ref" :::

    Ref[n, sc, id] `|` Stop[id]
  ----|>
    n//[] `|` sc//[] `|` Stop[id];

val rule_gc_next = "gc next" :::
    Next[s, id] `|` Stop[id]
  ----|>
    s//[] `|` Stop[id];

val rule_gc_while = "gc while" :::
    While[id, s1, g][[s2]] o `[s2]` `|` Stop[id]
  ----|>
    s1//[] `|` g//[] `|` Stop[id];

val rule_gc_if = "gc if" :::
    If[id, s1, g] o `[]` `|` Stop[id]
  ----|>
    s1//[] `|` g//[] `|` Stop[id];

val rule_gc_ass = "gc ass" :::
    Ass[id, s, f, scf, t, sct, g] `|` Stop[id]
  ----|>
    s//[] `|` f//[] `|` scf//[] `|` t//[] `|` sct//[] `|` g//[] `|` Stop[id];

val rule_gc_inv = "gc inv" :::
    Inv[l, lsc, oper, v, vsc, id, s, g] `|` Stop[id]
  ----|>
    l//[] `|` lsc//[] `|` oper//[] `|` v//[] `|` vsc//[] `|` s//[] `|` g//[] `|` Stop[id];

val rule_gc_rec = "gc rec" :::
    Rec[l, lsc, oper, v, vsc, id, s, g] `|` Stop[id]
  ----|>
    l//[] `|` lsc//[] `|` oper//[] `|` v//[] `|` vsc//[] `|` s//[] `|` g//[] `|` Stop[id];

val rule_gc_rep = "gc rep" :::
    Rep[l1, l1sc, oper, v1, v1sc, id, s1, g1] `|` Stop[id]
  ----|>
    l1//[] `|` l1sc//[] `|` oper//[] `|` v1//[] `|` v1sc//[] `|` s1//[] `|` g1//[] `|` Stop[id];

val rule_gc_getrep = "gc getrep" :::
    GetRep[l2, l2sc, oper, v2, v2sc, id, s2, g4] `|` Stop[id]
  ----|>
    l2//[] `|` l2sc//[] `|` oper//[] `|` v2//[] `|` v2sc//[] `|` s2//[] `|` g4//[] `|` Stop[id];

val rule_gc_exit = "gc exit" :::
    Exit[id, s, g] `|` Stop[id]
  ----|>
    s//[] `|`  g//[] `|` Stop[id];

(* FIXME this will not work as the id might be connected to the partnerlink of
 * another instance*)
val rule_gc_stop = "gc stop" :::
    -//[id] o Stop[id]
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

(* !!!!!! UPDATE PROCESS WITH MORE PORTS SIMILAR TO A SCOPE !!!! *)
val rule_invoke = "invoke" :::

    Inv[l1, l1sc, oper, v, vsc, id1, s, g] 
`|` Var[l1, l1sc, g1] o <-> `|` Var[v, vsc, g2] o `[(*0*)]` `|` Run[id1]
`|` Process[n][[sc]] o
    (Var[l2, sc, g3] o (CrInst[oper] `|` `[(*1*)]`) `|` `[(*2*) sc]`)

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 0, 4&[id2] |--> 2&[sc]]--|>

    s//[] `|` -//[id2] o (
    Var[l1, l1sc] o Link[id2] `|` Var[v, vsc] o `[(*0*)]` `|` Run[id1]
`|` Process[n][[sc]] o
    (Var[l2, sc] o (CrInst[oper] `|` `[(*1*)]`) `|` `[(*2*) sc]`)
`|` Var[l2, id2] o (Link[id1] `|` Mess[oper] o `[(*3*)]` `|` Reply[oper, id1])
`|` `[(*4*)id2]` `|` Invoked[id2]);



(* The receive rule takes care of activating the instance, by removing a
 * receive node associated to the partner link and the operation
 * (indicated by the link of the Message in Mess), copying the content of the
 * Message in the PartnerLink to the proper input variable, and changing
 * the Invoked node to a Run node.
 *)
val rule_receive = "receive" :::

    Rec[l, lsc, oper, v, vsc, id, s, g]
`|` Var[l, lsc, g1, id] o (`[(*0*)]` `|` Mess[oper] o `[(*1*)]`)
`|` Var[v, vsc, g2, id] o `[(*2*)]` `|` Invoked[id]

  --[0 |-> 0, 1 |-> 1]--|>

    oper//[] `|` s//[] `|` g//[] `|` Var[l, lsc, g1, id] o `[(*0*)]`
`|` Var[v, vsc, g2, id] o `[(*1*)]` `|` Run[id];


(* The invoke instance rule executes an Invoke activity in one instance
 * simultaneously with a corresponding Receive activity in another
 * instance, replacing the Invoke with a GetReply activity and removing
 * the Receive. The invoking process' PartnerLink is used to identify
 * the receiving instance. The content of the output variable is copied
 * to the appropriate variable of the receiving instance.
 *)
val rule_invoke_instance = "invoke_instance" :::

    Inv[l1, l1sc, oper, v1, v1sc, id1, s1, g1]
`|` Var[l1, l1sc, g2, id1] o (Link[id2] `|` `[]`) `|` Var[v1, v1sc, g3, id1] o `[]` `|` Run[id1]
`|` Rec[l2, l2sc, oper, v2, v2sc, id2, s2, g4] 
`|` Var[l2, l2sc, g5, id2] o `[]` `|` Var[v2, v2sc, g6, id2] o `[]` `|` Run[id2]

  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>

    s1//[] `|` g1//[]
`|` Var[l1, l1sc, g2, id1] o (Link[id2] `|` `[]`) `|` Var[v1, v1sc, g3, id1] o `[]` `|` Run[id1]
`|` s2//[] `|` g4//[]
`|` Var[l2, l2sc, g5, id2] o (`[]` `|` Reply[oper, id1]) `|` Var[v2, v2sc, g6, id2] o `[]` `|` Run[id2];


(* The Rep activity inside one instance can synchronize together with
 * a GetRep activity inside another instance, thereby copying the
 * content from variable var to variable outvar.
 *)
(* !!!!!! There seeems to be something wrong with the instantiation !!!! *)
val rule_reply = "reply" :::

    Rep[l1, l1sc, oper, v1, v1sc, id1, s1, g1]
`|` Var[l1, l1sc, g2, id1] o (Reply[oper, id2] `|` `[]`) `|` Var[v1, v1sc, g3, id1] o `[]` `|` Run[id1]
`|` GetRep[l2, l2sc, oper, v2, v2sc, id2, s2, g4]
`|` Var[l2, l2sc, g5, id2] o (Link[id1] `|` `[]`) `|` Var[v2, v2sc, g6, id2] o `[]` `|` Run[id2]

  --[0 |-> 0, 1 |-> 1, 2 |-> 1]--|>

    oper//[] `|` s1//[] `|` g1//[]
`|` Var[l1, l1sc, g2, id1] o `[]` `|` Var[v1, v1sc, g3, id1] o `[]` `|` Run[id1]
`|` s2//[] `|` g4//[]
`|` Var[l2, l2sc, g5, id2] o (Link[id1] `|` `[]`) `|` Var[v2, v2sc, g6, id2] o `[]` `|` Run[id2];

(* !!!!! TO BE UPDATED *)
val rulelist = [rule_scope_activation, rule_sequence_completed,
             rule_if_true, rule_if_false, rule_while_unfold,
             rule_variable_reference,
             rule_assign,
             (* rule_invoke, *)
             rule_receive,
             rule_invoke_instance, rule_reply,
             rule_exit_stop_inst];
val rules = mkrules rulelist;
