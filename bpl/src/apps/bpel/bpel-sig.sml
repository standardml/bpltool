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
