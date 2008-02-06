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
Process[name == echo_process][scope == echo_id]
o (    PartnerLinks o PartnerLink[name == echo_client, scope == echo_id] o CreateInstance[operation == echo]
   `|` Variables o Variable[name == x, scope == echo_id] o <->
   `|` Sequence[instanceId == echo_id] o (
         Receive[partnerLink == echo_client, partnerLinkScope == echo_id, operation == echo, variable == x, variableScope == echo_id, instanceId == echo_id]
   `|` Next o Sequence[instanceId == echo_id] o (
         Reply[partnerLink == echo_client, partnerLinkScope == echo_id, operation == echo, variable == x, variableScope == echo_id, instanceId == echo_id]
   `|` Next o
         Exit[instanceId == echo_id])));

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
-//[caller_id]
o (Instance[name == caller, id == caller_id]
   o (    Running[instanceId == caller_id]
      `|` PartnerLinks o PartnerLink[name == echo_service, scope == caller_id] o <->
      `|` Variables
           o (    Variable[name == y, scope == caller_id] o True
              `|` Variable[name == z, scope == caller_id] o False)
          `|` Invoke[partnerLink == echo_service, partnerLinkScope == caller_id, operation == echo, inputVariable == y, inputVariableScope == caller_id, outputVariable == z, outputVariableScope == caller_id, instanceId == caller_id]));


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
While[instanceId == echo_id]
o (    Condition o VariableRef[variable == x, variableScope == echo_id, instanceId == echo_id]
   `|` Scope[instanceId == echo_id][scope == scope]
       o (    PartnerLinks o <->
          `|` Variables    o Variable[name == y, scope == scope] o <->
          `|` Sequence[instanceId == echo_id] o (
                Receive[partnerLink == echo_client, partnerLinkScope == echo_id, operation == echo_value, variable == y, variableScope == scope, instanceId == echo_id]
          `|` Next o Sequence[instanceId == echo_id] o (
                Reply[partnerLink == echo_client, partnerLinkScope == echo_id, operation == echo_value, variable == y, variableScope == scope, instanceId == echo_id]
          `|` Next o
                Assign[instanceId == echo_id] o Copy o (    From[variable == y, scope == scope]
                                          `|` To[variable == x, scope == echo_id])))));
val echo_process2_context = 
Process[name == echo_process][scope == echo_id]
o (    PartnerLinks o PartnerLink[name == echo_client, scope == echo_id] o CreateInstance[operation == echo]
   `|` Variables    o Variable[name == x, scope == echo_id] o <->
   `|` Sequence[instanceId == echo_id] o (
         Receive[partnerLink == echo_client, partnerLinkScope == echo_id, operation == echo, variable == x, variableScope == echo_id, instanceId == echo_id]
   `|` Next o Sequence[instanceId == echo_id] o (
         Reply[partnerLink == echo_client, partnerLinkScope == echo_id, operation == echo, variable == x, variableScope == echo_id, instanceId == echo_id]
   `|` Next o `[echo_id]`)));

val echo_process2 = echo_process2_context o while_loop;
val echo_process2_emptyloop = echo_process2_context o While[instanceId == echo_id] o <->;

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
-//[caller_id]
o (Instance[name == caller, id == caller_id]
   o (    Running[instanceId == caller_id]
      `|` PartnerLinks o PartnerLink[name == echo_service, scope == caller_id] o <->
      `|` Variables
          o (    Variable[name == z, scope == caller_id] o True
             `|` Variable[name == v, scope == caller_id] o False)
      `|` Flow[instanceId == caller_id]
          o (    Sequence[instanceId == caller_id] o (
                   Invoke[partnerLink == echo_service, partnerLinkScope == caller_id, operation == echo, inputVariable == z, inputVariableScope == caller_id, outputVariable == v, outputVariableScope == caller_id, instanceId == caller_id]
             `|` Next o
                   Invoke[partnerLink == echo_service, partnerLinkScope == caller_id, operation == echo_value, inputVariable == v, inputVariableScope == caller_id, outputVariable == v, outputVariableScope == caller_id, instanceId == caller_id])
             `|` Exit[instanceId == caller_id])));


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