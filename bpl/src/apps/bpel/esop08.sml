(* Formalization of a subset of BPEL using Binding Bigraphical Reactive
 * Systems in BPLtool.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

val cur_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir cur_dir;


(*******************************)
(*          Signature          *)
(*******************************)
(* The binding port of a Process is used to delimit the scope of
 * variables within the process to the process itself.
 * The free port should be connected to name of the process. *)
val Process     = passive  ("Process"     =: 1 --> 1);

(* The first free port should be connected to name of the process.
 * The second free port of an instance is the instance identifier
 * which (among other things) is used to determine the scope of
 * variables within the instance to the instance itself. *)
val Instance    = active   ("Instance"    -:       2);

(* The binding port of a Scope is used to delimit the scope of
 * variables within the scope to the scope itself. *)
val Scope       = passive  ("Scope"       =: 1 --> 1);
(* Scopes have to be initialized before they can be used. An ActiveScope
 * is a scope that has has been initialized.
 * The first free port of an ActiveScope is used to delimit the scope of
 * variables within the scope to the scope itself.
 * The second free port should be connected to the instance identifier.
 *)
val ActiveScope = active   ("ActiveScope" -:       2);

(* The free port of a 'running' or 'stopped' node should be connected to
 * the scope port of the parent process/instance.
 *)
val Running     = atomic   ("Running"     -:       1);
val Stopped     = atomic   ("Stopped"     -:       1);

val Variables   = passive0 ("Variables"             );
(* The first free port of a variable should be connected to its name,
 * and the second should be connected to the scope port of the node
 * delimiting its scope. *)
val Variable    = passive  ("Variable"    -:       2);

val Sequence    = active   ("Sequence"    -:       1);
val Next        = passive0 ("Next"                  );

val Flow        = active   ("Flow"        -:       1);

val While       = passive  ("While"       -:       1);

val If          = passive  ("If"          -:       1);
val Condition   = passive0 ("Condition"             );
val Then        = passive0 ("Then"                  );
val Else        = passive0 ("Else"                  );
val True        = atomic0  ("True"                  );
val False       = atomic0  ("False"                 );

val Assign      = passive  ("Assign"      -:       1);
val Copy        = passive0 ("Copy"                  );
(* The first free port of a To or From node should be connected to a
 * variable name, and the second should be connected to the scope port of
 * the node delimiting its scope. *)
val To          = atomic   ("To"          -:       2);
val From        = atomic   ("From"        -:       2);

(* The free ports of an Invoke node should be connected:
 * 
 *   #1 to the name of the operation to be invoked
 *   #2 to the name of the input variable
 *   #3 to the same scope port as the input variable
 *   #4 to the name of the output variable
 *   #5 to the same scope port as the output variable
 *   #6 to the instance identifier
 *)
val Invoke      = atomic   ("Invoke"      -:       6);

val Proxies     = passive0 ("Proxies"               );
(* The free ports of a RecProxy node should be connected:
 *
 *   #1 to the name of the operation
 *   #2 to the instance identifier
 *)
val RecProxy    = passive  ("RecProxy"    -:       2);
val Parameter   = passive0 ("Parameter"             );
(* The free ports of a Receive node should be connected:
 * 
 *   #1 to the name of the operation
 *   #2 to the instance identifier
 *   #3 to the name of the variable
 *   #4 to the same scope port as the variable
 *)
val Receive     = atomic   ("Receive"     -:       4);
(* The free ports of a Reply node should be connected:
 * 
 *   #1 to the name of the variable
 *   #2 to the same scope port as the variable
 *   #3 to the instance identifier of its enclosing instance
 *)
val Reply       = atomic   ("Reply"       -:       3);
(* The free ports of a GetReply node should be connected:
 * 
 *   #1 to the name of the output variable
 *   #2 to the same scope port as the output variable
 *   #3 to the instance identifier of its enclosing instance
 *   #4 to the instance identifier of the replying instance
 *)
val GetReply    = atomic   ("GetReply"    -:       4);

(* The free port should be connected to the instance identifier of the
 * enclosing instance. *)
val Exit        = atomic   ("Exit"        -:       1);


(*******************************)
(*        Reaction rules       *)
(*******************************)
(* Scope *)
val rule_scope_activation   =
    "scope activation"   ::: Scope["inst_id"][["scope"]]
                             || Running["inst_id"]
                           ----|>
                             (idw["inst_id"] * -/"scope" * `[]`)
                             o (ActiveScope["scope", "inst_id"]
                                o `["scope"]`)
                             || Running["inst_id"];
val rule_scope_completed    =
    "scope completed"    ::: ActiveScope["scope", "inst_id"] o <->
                             || Running["inst_id"]
                           ----|>
                             ("scope"//[] * <->)
                             || Running["inst_id"];

(* Structural activities *)
val rule_flow_completed     =
    "flow completed"     ::: Flow["inst_id"] o <->
                             || Running["inst_id"]
                           ----|>
                             <->
                             || Running["inst_id"];
val rule_sequence_completed =
    "sequence completed" ::: Sequence["inst_id"] o Next o `[]`
                             || Running["inst_id"]
                           ----|>
                             `[]`
                             || Running["inst_id"];
val rule_if_true            =
    "if true"            ::: (If["inst_id"]
                              o (Condition o True
                                 `|` (Then o `[]`) `|` (Else o `[]`)))
                             || Running["inst_id"]
                           ----|>
                             `[]`
                             || Running["inst_id"];
val rule_if_false           =
    "if false"           ::: (If["inst_id"]
                              o (Condition o False
                                 `|` (Then o `[]`) `|` (Else o `[]`)))
                              || Running["inst_id"]
                           --[0 |-> 1]--|>
                             `[]`
                             || Running["inst_id"];
val rule_while_unfold       =
    "while unfold"       ::: (While["inst_id"] o (Condition `|` `[]`))
                             || Running["inst_id"]
                           --[2 |-> 0, 3 |-> 1]--|>
                             (If["inst_id"]
                              o (Condition
                                 `|` Then o Sequence["inst_id"]
                                     o (`[]` 
                                        `|` Next
                                            o While["inst_id"]
                                              o (Condition `|` `[]`))
                                 `|` Else o <->))
                             || Running["inst_id"];

(* Variable assignment *)
val rule_variable_copy      =
    "variable copy"      ::: (Assign["inst_id"] o Copy
                              o (From["f", "scope"]
                                 `|` To["t", "scope'"]))
                             || Variable["f", "scope"]
                             || Variable["t", "scope'"]
                             || Running["inst_id"]
                           --[1 |-> 0]--|>
                             <->
                             || Variable["f", "scope"]
                             || Variable["t", "scope'"]
                             || Running["inst_id"];

(* Process communication *)
val rule_invoke             =
    "invoke"             ::: Invoke["op", "invar", "invar_scope",
				    "outvar", "outvar_scope",
				    "inst_id_invoker"]
			     || Variable["invar", "invar_scope"]
                             || Running["inst_id_invoker"]
			     || Process["proc_name"][["scope"]]
                                o (<["scope"]>
				   ((Proxies
                                     o (RecProxy["op", "scope"] o <->
                                        `|` `[]`))
                                    `|` `["scope"]`))
                           --[3 |-> 0, 4 |-> 1, 5&["inst_id_invoked"] |--> 2&["scope"]]--|>
			     (-/"inst_id_invoked"
			      * idw["op", "invar", "invar_scope", "outvar",
				    "outvar_scope", "inst_id_invoker",
				    "proc_name"]
			      * idp(4))
                             o (GetReply["outvar", "outvar_scope",
					 "inst_id_invoker", "inst_id_invoked"]
				|| Variable["invar", "invar_scope"]
				|| Running["inst_id_invoker"]
				|| ((Process["proc_name"][["scope"]]
                                     o (<["scope"]>
				        ((Proxies
					  o (RecProxy["op", "scope"] o <->
				             `|` `[]`))
					 `|` `["scope"]`)))
				    `|` (Instance["proc_name", "inst_id_invoked"]
					 o ((Proxies
					     o ((RecProxy["op", "inst_id_invoked"]
						 o Parameter o `[]`)
					        `|` `[]`))
				            `|` Running["inst_id_invoked"]
					    `|` `["inst_id_invoked"]`))));

(* NB! invoke step 2 + reply missing*)

(* Process cancellation *)
val rule_exit_stop_inst     =
    "exit stop inst"     ::: Exit["inst_id"]
                             || Running["inst_id"]
                           ----|>
                             <->
                             || Stopped["inst_id"];
val rule_exit_remove_inst   =
    "exit remove inst"   ::: Instance["name", "inst_id"]
                             o (Stopped["inst_id"] `|` `[]`)
                           ----|>
                             "name"//[] * "inst_id"//[] * <->;
                             

(*******************************)
(*       Example processes     *)
(*******************************)

