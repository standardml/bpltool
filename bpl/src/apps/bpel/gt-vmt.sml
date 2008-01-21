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
val ToPLink        = "ToPLink"
val FromPLink      = "FromPLink"
val Invoke         = "Invoke"
val PartnerLinks   = "PartnerLinks"    
val PartnerLink    = "PartnerLink"
val Message        = "Message"
val Link           = "Link"
val CreateInstance = "CreateInstance"
val Receive        = "Receive"
val Reply          = "Reply"
val ReplyTo        = "ReplyTo"
val GetReply       = "GetReply"
val Exit           = "Exit"

(*    For names                *)
val inst_id                    = "inst_id"
val inst_id1                   = "inst_id1"
val inst_id2                   = "inst_id2"
val inst_id_invoker            = "inst_id_invoker"
val inst_id_invoked            = "inst_id_invoked"
val inst_id_invoked1           = "inst_id_invoked1"
val inst_id_invoked2           = "inst_id_invoked2"
val scope                      = "scope"
val scope1                     = "scope1"
val scope2                     = "scope2"
val f                          = "f"
val t                          = "t"
val proc_name                  = "proc_name"
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

val PartnerLinks = active0  (PartnerLinks          );
(* The free ports of a PartnerLink node should be connected:
 *
 *   #1 to the name of the partner link
 *   #2 to the scope port of the node delimiting its scope
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
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation
 *   #4 to the name of the variable
 *   #5 to the same scope port as the variable
 *   #6 to the instance identifier
 *)
val Receive      = atomic   (Receive     -:       6);
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

(* The free port should be connected to the instance identifier of the
 * enclosing instance. *)
val Exit         = atomic   (Exit        -:       1);


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

(* The rules for evaluating an if-then-else statement is as expected. If
 * the condition is True we execute the then-branch, otherwise we execute
 * the else-branch.
 *)
val rule_if_true = "if true" :::

   If[inst_id] o (    Condition o True
                  `|` Then o `[]`
                  `|` Else o `[]`)
|| Running[inst_id]
  ----|>
   `[]`
|| Running[inst_id];


val rule_if_false = "if false" :::

   If[inst_id] o (    Condition o False
                  `|` Then o `[]`
                  `|` Else o `[]`)
|| Running[inst_id]
  --[0 |-> 1]--|>
   `[]`
|| Running[inst_id];

(* We give semantics to a while-loop in the traditional manner, by
 * unfolding the loop once and using an if-then-else construct with the
 * loop condition.
 *)
val rule_while_unfold = "while unfold" :::

   While[inst_id] o (Condition o `[]` `|` `[]`)
|| Running[inst_id]

  --[2 |-> 0, 3 |-> 1]--|>

   If[inst_id] o (    Condition o `[]`
                  `|` Then o Sequence[inst_id]
                             o (`[]` 
                                `|` Next
                                    o While[inst_id]
                                      o (Condition o `[]` `|` `[]`))
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

  --[1 |-> 0]--|>

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

   Assign[inst_id] o Copy o (    From[f, scope1]
                             `|` To[t, scope2])
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id]

  --[1 |-> 0]--|>

   <->
|| Variable[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id];

val rule_assign_copy_var2plink = "assign copy var2plink" :::

   Assign[inst_id] o Copy o (    From[f, scope1]
                             `|` ToPLink[t, scope2])
|| Variable[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id]

  --[1 |-> 0]--|>

   <->
|| Variable[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id];

val rule_assign_copy_plink2var = "assign copy plink2var" :::

   Assign[inst_id] o Copy o (    FromPLink[f, scope1]
                             `|` To[t, scope2])
|| PartnerLink[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id]

  --[1 |-> 0]--|>

   <->
|| PartnerLink[f, scope1] o `[]`
|| Variable[t, scope2] o `[]`
|| Running[inst_id];

val rule_assign_copy_plink2plink = "assign copy plink2plink" :::

   Assign[inst_id] o Copy o (    FromPLink[f, scope1]
                             `|` ToPLink[t, scope2])
|| PartnerLink[f, scope1] o `[]`
|| PartnerLink[t, scope2] o `[]`
|| Running[inst_id]

  --[1 |-> 0]--|>

   <->
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
  ----|>
   -/scope o (ActiveScope[scope, inst_id] o `[scope]`)
|| Running[inst_id];

(* When we are finished executing the body of the scope we remove the
 * scope, including its variables, partner links, and its associated
 * "scope"-edge. *)
val rule_scope_completed = "scope completed" :::

   -/scope
   o (ActiveScope[scope, inst_id]
      o (    Variables    o (scope//[scope1] o `[scope1]`)
         `|` PartnerLinks o (scope//[scope2] o `[scope2]`)))
|| Running[inst_id]
  ----|>
   <->
|| Running[inst_id];




(* Process termination *)
(* Processes can terminate in two ways:
 * 1) normally, i.e. when no more activities remain, or 2) abnormally by
 * executing an Exit activity.
 *)

(* In the first case, we simply remove the instance in the same way as
 * for scopes.
 *)
val rule_inst_completed = "inst completed" :::

-/inst_id
o (Instance[proc_name, inst_id]
   o (    Variables    o (inst_id//[inst_id1] o `[inst_id1]`)
      `|` PartnerLinks o (inst_id//[inst_id2] o `[inst_id2]`)
      `|` Running[inst_id]))
  ----|>
<-> || proc_name//[];

(* In the case of an Exit activity we change the status of the instance
 * from running to stopped by replacing the Running node inside the
 * instance with a Stopped node. This prevents other rules from being
 * used, except for the rule "exit remove instance" below, effectively
 * stopping any other activity from proceeding.
 *)
val rule_exit_stop_inst = "exit stop inst" :::

   Exit[inst_id]
|| Running[inst_id]
  ----|>
   <->
|| Stopped[inst_id];

(* Once an Instance node contains a Stopped node we garbage collect the
 * instance together with all its remaining content.
 *)
val rule_exit_remove_inst = "exit remove inst" :::

-/inst_id
o (Instance[proc_name, inst_id]
   o (    inst_id//[inst_id1] o Stopped[inst_id1]
      `|` inst_id//[inst_id2] o `[inst_id2]`))
  ----|>
<-> || proc_name//[];



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
|| Running[inst_id_invoker]
|| Process[proc_name][[scope]]
   o (    PartnerLinks
          o (    PartnerLink[partner_link, scope]
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
   || Running[inst_id_invoker]
   || (Process[proc_name][[scope]]
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
                  o `[inst_id_invoked2]`)));
(* FIXME: Out of sync!
(* A specialized version of the above rule, where the partner link and
 * variable must be at the top-level scope. *)
val rule_invoke_specialized = "invoke_specialized" :::
   (    PartnerLinks
         o (PartnerLink[partner_link_invoker, inst_id_invoker] o <-> `|` `[]`)
    `|` Variables o (Variable[invar, invar_scope] o `[]` `|` `[]`)
    `|` Running[inst_id_invoker])
|| Invoke[partner_link_invoker, inst_id_invoker, oper,
          invar, invar_scope, outvar, outvar_scope, inst_id_invoker]
|| Process[proc_name][[scope]]
   o (PartnerLinks
      o (PartnerLink[partner_link, scope] o (CreateInstance[oper] `|` `[]`)
         `|` `[]`)
      `|` `[scope]`)

  --[6 |-> 1, 7 |-> 4, 8&[inst_id_invoked] |--> 5&[scope]]--|>

-/inst_id_invoked
o ((   PartnerLinks
        o (PartnerLink[partner_link_invoker, inst_id_invoker]
            o Link[inst_id_invoked] `|` `[]`)
    `|` Variables o (Variable[invar, invar_scope] o `[]` `|` `[]`)
    `|` Running[inst_id_invoker])
   || GetReply[partner_link_invoker, inst_id_invoker, oper,
               outvar, outvar_scope, inst_id_invoker]
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
*)


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
|| Invoked[inst_id]

  ----|>

   <-> || oper//[]
|| PartnerLink[partner_link, partner_link_scope]
   o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id];

(* FIXME Out of sync!
(* A specialized version of the above rule, where the partner link and
 * variable must be at the top-level scope. *)
val rule_receive_specialized = "receive_specialized" :::

   (    PartnerLinks o (
          `[]` `|`
          PartnerLink[partner_link, inst_id] o (
            `[]` `|` Message[oper] o `[]`))
    `|` Invoked[inst_id]
    `|` Variables o (
          `[]` `|`
          Variable[var, var_scope] o `[]`))
|| Receive[partner_link, inst_id, oper, var, var_scope, inst_id]
  --[2 |-> 3, 3 |-> 2]--|>
   (    PartnerLinks o (
          `[]` `|`
          PartnerLink[partner_link, inst_id] o `[]`)
    `|` Running[inst_id]
    `|` Variables o (
          `[]` `|`
          Variable[var, var_scope] o `[]`))
|| <-> || oper//[];
*)

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
|| Running[inst_id_invoker]
|| Receive[partner_link_invoked, partner_link_scope_invoked, oper,
           var, var_scope, inst_id_invoked]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]

  --[3 |-> 1]--|>

   GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker]
|| PartnerLink[partner_link_invoker, partner_link_scope_invoker]
   o (Link[inst_id_invoked] `|` `[]`)
|| Variable[invar, invar_scope] o `[]`
|| Running[inst_id_invoker]
|| <->
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
         var, var_scope, inst_id_invoked]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked]
   o (ReplyTo[oper, inst_id_invoker] `|` `[]`)
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]
|| GetReply[partner_link_invoker, partner_link_scope_invoker, oper,
            outvar, outvar_scope, inst_id_invoker]
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker]

  --[2 |-> 1]--|>

   <-> || oper//[]
|| PartnerLink[partner_link_invoked, partner_link_scope_invoked] o `[]`
|| Variable[var, var_scope] o `[]`
|| Running[inst_id_invoked]
|| <-> || partner_link_invoker//[] || partner_link_scope_invoker//[]
|| Variable[outvar, outvar_scope] o `[]`
|| Running[inst_id_invoker];




val rules =
    mkrules [rule_scope_activation, rule_scope_completed,
             rule_flow_completed, rule_sequence_completed,
             rule_if_true, rule_if_false, rule_while_unfold,
             rule_assign_copy_var2var,
             rule_assign_copy_var2plink,
             rule_assign_copy_plink2var,
             rule_assign_copy_plink2plink,
             rule_invoke, (*rule_invoke_specialized,*)
             rule_receive, (*rule_receive_specialized,*)
             rule_invoke_instance, rule_reply,
             rule_exit_stop_inst, rule_exit_remove_inst,
             rule_inst_completed];

val tactic = roundrobin;

(*******************************)
(*       Example processes     *)
(*******************************)

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
-/caller_id
o (Instance[caller, caller_id]
   o (    Running[caller_id]
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
-/caller_id
o (Instance[caller, caller_id]
   o (    Running[caller_id]
      `|` PartnerLinks o PartnerLink[echo_service, caller_id] o <->
      `|` Variables
          o (    Variable[z, caller_id] o True
             `|` Variable[v, caller_id] o False)
      `|` Flow[caller_id]
          o (    Sequence[caller_id]
                 o (    Invoke[echo_service, caller_id, echo,
                               z, caller_id, v, caller_id, caller_id]
                    `|` Next o Invoke[echo_service, caller_id, echo_value,
                                      v, caller_id, v, caller_id, caller_id])
             `|` Exit[caller_id])));


val _ = use_shorthands on;

(*
val mz1 = matches (mkrules [rule_reply]) (caller_inst1 `|` echo_process1);
val mz2 = matches (mkrules [rule_invoke_specialized]) (caller_inst1 `|` echo_process1);
val mz3 = matches (mkrules [rule_invoke]) (caller_inst1 `|` echo_process1);
print_mv mz2;*)

(*val final_state = run rules tactic (echo_process || caller_inst);*)
(*val final_state = run rules (react_rule "invoke_specialized") (caller_inst1 `|` echo_process1);*)

(*
val state1_0 = caller_inst1 `|` echo_process1
val state2_0 = caller_inst2 `|` echo_process2;
val state_0 = caller_inst2 `|` echo_process2_emptyloop;

val tac_invoke =
  react_rule "invoke" ++
  react_rule "receive" ++
  react_rule "sequence completed"

val state1_invokedz = stepz rules tac_invoke state1_0;
val state2_invokedz = stepz rules tac_invoke state2_0;
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