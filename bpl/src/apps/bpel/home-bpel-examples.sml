(*******************************)
(*       Example processes     *)
(*******************************)

(* A simple engine process: it is started by the operation
 * "start_engine" after which it repeatedly provides the operation "run"
 * which receives a process and executes it.
 * It replies True to the sender in response to both operations, since
 * replies are required.
 *
 * <process name="engine">
 *   <partnerLinks>
 *     <partnerLink name="engine_client" />
 *   </partnerLinks>
 *   <subLinks>
 *     <subLink name="subinsts" />
 *   </subLinks>
 *   <variables>
 *     <variable name="in" />
 *     <variable name="out"><from>true()</from></variable>
 *   </variables>
 *   <sequence>
 *     <receive partnerLink="engine_client" operation="start_engine"
 *              createInstance="yes" variable="in" />
 *     <reply   partnerLink="engine_client" operation="start_engine"
 *              variable="out" />
 *     <while>
 *       <condition>$out</condition>
 *       <sequence>
 *         <receive partnerLink="engine_client" operation="run" variable="in" />
 *         <thaw variable="in" subLink="subinsts" />
 *         <reply   partnerLink="engine_client" operation="run" variable="out" />
 *       </sequence>
 *     </while>
 *   </sequence>
 * </process>
 *)
val engine_while_loop =
While[engine_id] o (

    Condition o VariableRef[out, engine_id, engine_id]

`|` Sequence[engine_id] o (
      Receive[engine_client, engine_id, run, invar, engine_id, engine_id]
`|` Next o Sequence[engine_id] o (
      Thaw[subinsts, engine_id, invar, engine_id, engine_id]
`|` Next o
      Reply[engine_client, engine_id, run, out, engine_id, engine_id]
)));

val engine_process = 
Process[engine][[engine_id]] o (

    PartnerLinks o
        PartnerLink[engine_client, engine_id] o CreateInstance[start_engine]
`|` SubLinks o SubLink[subinsts, engine_id] o <->
`|` Variables o (
        Variable[invar, engine_id] o <->
    `|` Variable[out, engine_id] o True)
`|` Instances o <->

`|` Sequence[engine_id] o (
      Receive[engine_client, engine_id, start_engine, invar, engine_id, engine_id]
`|` Next o Sequence[engine_id] o (
      Reply[engine_client, engine_id, start_engine, out, engine_id, engine_id]
`|` Next o
      engine_while_loop
    ))
);

(* An engine instance:

<instance name="engine" id="1">
  <partnerLinks>
    <partnerLink name="engine_client"><link ref="2" /></partnerLink>
  </partnerLinks>
  <subLinks>
    <subLink name="subinsts"><link ref="3" /><link ref="4" /></subLink>
  </subLinks>
  <variables>
    <variable name="in"><process name="treatment">...</process></variable>
    <variable name="out"><true /></variable>
  </variables>
  <instances>
    <instance name="medication" id="3">...</instance>
    <instance name="treatment" id="4">...</instance>
  </instances>
  <sequence>
     <receive partnerLink="engine_client" operation="run" variable="in" />
     <thaw variable="in" subLink="subinsts" />
     <reply   partnerLink="engine_client" operation="run" variable="out" />
     <while>[the same as in the process definition]</while>
  </sequence>
</instance>
 *)
val engine_instance =
TopInstance o (
-//[engine_id, parent_active_scopes, active_scopes, medication_id, treatment_id] o (
    TopRunning[engine_id]
`|` Instance[engine, engine_id, parent_active_scopes] o (
        Running[engine_id, active_scopes, engine_id]
    `|` PartnerLinks o
            PartnerLink[engine_client, engine_id] o Link[doctor_id]
    `|` SubLinks o SubLink[subinsts, engine_id] o (
                       Link[medication_id] `|` Link[treatment_id])
    `|` Variables o (
            Variable[invar, engine_id] o
                Process[treatment][[treatment_scope]] o <->  (* ... *)
        `|` Variable[out, engine_id] o True)
    `|` Instances o (
            Instance[medication, medication_id, active_scopes] o <->  (* ... *)
        `|` Instance[treatment, treatment_id, active_scopes]   o <->) (* ... *)

    `|` Sequence[engine_id] o (
          Receive[engine_client, engine_id, run, invar, engine_id, engine_id]
    `|` Next o Sequence[engine_id] o (
          Thaw[subinsts, engine_id, invar, engine_id, engine_id]
    `|` Next o Sequence[engine_id] o (
          Reply[engine_client, engine_id, run, out, engine_id, engine_id]
    `|` Next o
          engine_while_loop
        )))
    )
));


(* A doctor process: 
 *
<process name="doctor">
  <partnerLinks>
    <partnerLink name="hospital" />
    <partnerLink name="patient" />
  </partnerLinks>
  <subLinks>
    <subLink name="treatment" />
  </subLinks>
  <variables>
    <variable name="in" />
    <variable name="out"><from>true()</from></variable>
  </variables>
  <sequence>
    <receive partnerLink="hospital" operation="doctor_hired"
             createInstance="yes" variable="in" />
    <reply   partnerLink="hospital" operation="doctor_hired" variable="out" />
    <while>
      <condition>true()</condition>
      <sequence>
        <receive partnerLink="hospital" operation="patient" variable="in" />
        <reply   partnerLink="hospital" operation="patient" variable="out" />
        <assign><copy>
            <from variable="in" />
            <to   partnerLink="patient" />
        </copy></assign>
        <receive partnerLink="hospital" operation="treatment"
                 variable="in" />
        <reply   partnerLink="hospital" operation="treatment"
                 variable="out" />
        <thawSub   subLink="treatment" variable="in" />
        <invokeSub subLink="treatment" operation="perform_treatment"
                   inputVariable="out" outputVariable="out" />
        <freezeSub subLink="treatment" variable="in" />
        <invoke partnerLink="patient" operation="run"
                inputVariable="in" outputVariable="out" />
      </sequence>
    </while>
  </sequence>
</process>
 *)
(* FIXME some of the names should probably be closed ? *)
(* I have only used doctor_id since I got confused making the 
   doctor_instance, see below :D *)

val doctor_process = 
Process[doctor][[doctor_id]] o (

    PartnerLinks o (
        PartnerLink[hospital, doctor_id] o CreateInstance[doctor_hired]
    `|` PartnerLink[patient, doctor_id]  o <->) 
`|` SubLinks o SubLink[treatment, doctor_id] o <->
`|` Variables o (
        Variable[invar, doctor_id] o <->
    `|` Variable[out, doctor_id] o True) 
`|` Instances o <->

`|` Sequence[doctor_id] o (
      Receive[hospital, doctor_id, doctor_hired, invar, doctor_id, doctor_id]   
`|` Next o Sequence[doctor_id] o (
      Reply[hospital, doctor_id, doctor_hired, out, doctor_id, doctor_id]
`|` Next o
      While[doctor_id] o (
          Condition o True

      `|` Sequence[doctor_id] o (
            Receive[hospital, doctor_id, patient, invar, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Reply[hospital, doctor_id, patient, out, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Assign[doctor_id] o Copy o (
                From[invar, doctor_id] 
            `|` ToPLink[patient, doctor_id])
      `|` Next o Sequence[doctor_id] o (
    (* So there is a sublink called treatment and an operation on a  *)
    (* partnerlink called treatment ???? yep :-) /Espen *)
            Receive[hospital, doctor_id, treatment, invar, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Reply[hospital, doctor_id, treatment, out, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Thaw[treatment, doctor_id, invar, doctor_id, doctor_id] 
      `|` Next o Sequence[doctor_id] o (
            InvokeSub[treatment, doctor_id, perform_treatment, 
                      out, doctor_id, out, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Freeze[treatment, doctor_id, invar, doctor_id, doctor_id]
      `|` Next o Invoke[patient, doctor_id, run, invar, doctor_id,
                         out, doctor_id, doctor_id]
          ))))))))
      )
    ))
);

(* A doctor instance:

<instance name="doctor" id="2">
  <partnerLinks>
    <partnerLink name="hospital"><link ref="5" /></partnerLink>
    <partnerLink name="patient"><link ref="1" /></partnerLink>
  </partnerLinks>
  <subLinks>
    <subLink name="treatment"><link ref="6" /></subLink>
  </subLinks>
  <variables>
    <variable name="in"><process name="surgery">...</process></variable>
    <variable name="out"><true /></variable>
  </variables>
  <instances>
    <instance name="surgery" id="6">...</instance>
  </instances>
  <sequence>
    <invokeSub subLink="treatment" operation="perform_treatment"
               inputVariable="out" outputVariable="out" />
    <freezeSub subLink="treatment" variable="in" />
    <invoke partnerLink="patient" operation="run"
            inputVariable="in" outputVariable="out" />
    <while>[the same as in the process definition]</while>
  </sequence>
</instance>
*)
(* FIXME *)
(* insert while-loop ??? *)
val doctor_instance = 
TopInstance o (
-//[doctor_id, parent_active_scopes, active_scopes, surgery_id] o (
    TopRunning[doctor_id] 
`|` Instance[doctor, doctor_id, parent_active_scopes] o (
        Running[doctor_id, active_scopes, doctor_id] 
    `|` PartnerLinks o (
            PartnerLink[hospital, doctor_id] o Link[hospital_id]
        `|` PartnerLink[patient, doctor_id] o Link[engine_id])
    `|` SubLinks o SubLink[treatment, doctor_id] o Link[surgery_id]
    `|` Variables o (
            Variable[invar, doctor_id] o
                Process[surgery][[surgery_scope]] o <->  (* ... *)
        `|` Variable[out, doctor_id] o True)
    `|` Instances o Instance[surgery, surgery_id, active_scopes] o <-> (* ... *)
   
    `|` Sequence[doctor_id] o (
          InvokeSub[treatment, doctor_id, perform_treatment, out, doctor_id, 
                    out, doctor_id, doctor_id]
    `|` Next o Sequence[doctor_id] o (
          Freeze[treatment, doctor_id, invar, doctor_id, doctor_id]
    `|` Next o Sequence[doctor_id] o (
          Invoke[patient, doctor_id, run, invar, doctor_id,
                 out, doctor_id, doctor_id] 
    `|` Next o While[doctor_id] o <-> (* ... *)
        )))
    )
));
