(* FIXME *)
(* Mikkel: All the activities: *)
(* Exit, GetReply, Reply, Receive, Invoke, Assign, If, While *)
(* have been assigned an additional port, but documentation  *)
(* have not been updated *)

(* !!!!!! *)
(* And now we add yet another port to the activities *)
(* Do we need to add a port to Next ? *)

(*******************************)
(*          Signature          *)
(*******************************)
(* The binding port of a Process is used to delimit the scope of
 * variables within the process to the process itself.
 * The free port should be connected to name of the process. *)
val Process      = passive  (Process     =: 2 --> 2);

(* The first free port should be connected to name of the process.
 * The second free port of an instance is the instance identifier
 * which (among other things) is used to determine the scope of
 * variables within the instance to the instance itself. *)
(*val Instance     = active   (Instance    -:       2);*)

(* The binding port of a Scope is used to delimit the scope of
 * variables within the scope to the scope itself.
 * The free port should be connected to the instance identifier. *)
val Scope        = passive  (Scope       =: 2 --> 2);
(* Scopes have to be initialized before they can be used. An ActiveScope
 * is a scope that has has been initialized.
 * The first free port should be connected to the instance identifier.
 * The second free port of an ActiveScope is used to delimit the scope of
 * variables within the scope to the scope itself.
 *)
(* Ports to: scope, actions, parent, and id *)
val ActScope  = atomic      (ActScope    -:       4);

(* The free port of a 'running', 'invoked', or 'stopped' node should
 * be connected to the scope port of the parent process/instance.
 *)
val Run          = atomic   (Run         -:       1);
val Invoked      = atomic   (Invoked     -:       1);
val Stop         = atomic   (Stop        -:       1);

(*val Variables    = active0  (Variables             );*)
(* The free ports of a variable should be connected
 *
 *   #1 to its name
 *   #2 to the scope port of the node delimiting its scope
 *   #3 to instance id
 *)
val Var          = passive  (Var         -:       3);
(* The free ports of a variable reference should be connected
 *
 *   #1 to the variable name
 *   #2 to the scope port of the node delimiting the variables scope
 *   #3 to the instance identifier
 *)
val Ref          = atomic   (Ref         -:       3);

(*val Sequence     = active   (Sequence    -:       1);*)

(* The free ports of a Next node should be connected
 *
 *   #1 to the predecessor edge
 *   #2 to the instance identifier
 *)
val Next         = passive  (Next        -:       2);

(*val Flow         = active   (Flow        -:       1);*)

val While        = passive  (While       =: 1 --> 3);

val If           = active   (If          -:       3);
val Cond         = active0  (Cond                  );
val Then         = passive0 (Then                  );
val Else         = passive0 (Else                  );
val True         = atomic0  (True                  );
val False        = atomic0  (False                 );

(* MIKKEL: CHANGE ARITY *)

val Ass          = passive  (Ass         -:       7);
(* val Copy         = passive0 (Copy                  ); *)
(* The first free port of a To or From node should be connected to a
 * variable name, and the second should be connected to the scope port of
 * the node delimiting its scope. *)
(* val To           = atomic   (To          -:       2); *)
(* val From         = atomic   (From        -:       2); *)

(* The free ports of a ToPLink or FromPLink node should be connected
 *
 *   #1 to a partner link name.
 *   #2 to the scope port of the node delimiting the partner link's scope.
 *)
(* val ToPLink      = atomic   (ToPLink     -:       2); *)
(* val FromPLink    = atomic   (FromPLink   -:       2); *)

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
(* ARITY CHANGED *)
val Inv          = atomic   (Inv         -:       8);

(*val PartnerLinks = active0  (PartnerLinks          );*)
(* The free ports of a PartnerLink node should be connected:
 *
 *   #1 to the name of the partner link
 *   #2 to the scope port of the node delimiting its scope
 *)
(* val PartnerLink  = passive  (PartnerLink -:       2); *)
(* The free port of a Message should be connected:
 *
 *   #1 to the name of the operation the message pertains to
 *)
val Mess         = passive  (Mess        -:       1);
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
val CrInst       = atomic (CrInst        -:    1);
(* The free ports of a Receive node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation
 *   #4 to the name of the variable
 *   #5 to the same scope port as the variable
 *   #6 to the instance identifier
 *)
val Rec          = atomic   (Rec         -:       8);
(* The free ports of a Reply node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation
 *   #4 to the name of the variable
 *   #5 to the same scope port as the variable
 *   #6 to the instance identifier of its enclosing instance
 *)
val Rep          = atomic   (Rep         -:       8);

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
val Reply        = atomic   (Reply       -:       2);

(* The free ports of a GetReply node should be connected:
 * 
 *   #1 to the name of the partner link
 *   #2 to the same scope port as the partner link
 *   #3 to the name of the operation
 *   #4 to the name of the output variable
 *   #5 to the same scope port as the output variable
 *   #6 to the instance identifier of its enclosing instance
 *)
val GetRep     = atomic   (GetRep         -:       8);

(* The free port should be connected to the instance identifier of the
 * enclosing instance. *)
val Exit         = atomic   (Exit        -:       3);
