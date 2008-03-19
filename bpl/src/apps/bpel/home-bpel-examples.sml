(*******************************)
(*       Example processes     *)
(*******************************)

(* A simple patient process: it is started by the operation
 * "start" after which it repeatedly provides the operation "run"
 * which receives a process and executes it.
 * It replies True to the sender in response to both operations, since
 * replies are required.
 *
<process name="patient">
  <partnerLinks>
    <partnerLink name="patient_client" />
    <partnerLink name="task_list_UI" />
  </partnerLinks>
  <subLinks>
    <subLink name="subinsts" />
  </subLinks>
  <variables>
    <variable name="x" />
    <variable name="y"><from>true()</from></variable>
  </variables>
  <sequence>
    <receive partnerLink="patient_client" operation="start"
             createInstance="yes" variable="x" />
    <invoke  partnerLink="task_list_UI" operation="init_UI"
             input_variable="x" output_variable="y" />
    <reply   partnerLink="patient_client" operation="start"
             variable="y" />
    <flow>
      <!-- Continually receive and execute subinstances -->
      <while>
        <condition>$y</condition>
        <sequence>
          <receive   partnerLink="patient_client" operation="run" variable="x" />
          <thaw      subLink="subinsts" variable="x" />
          <invokeSub subLink="subinsts" operation="resume" variable="y" />
          <reply     partnerLink="patient_client" operation="run" variable="y" />
        </sequence>
      </while>
      <!-- Continually receive tasks from subinstances and pass them
           on to the UI service -->
      <while>
        <condition>$y</condition>
        <scope>
          <variables>
            <variable name="task" />
            <variable name="reply" />
          </variables>
          <sequence>
            <receiveSub subLink="subinsts" operation="task" variable="task" />
            <invoke     partnerLink="task_list_UI" operation="add_task"
                        input_variable="task" output_variable="reply" />
            <replySub   subLink="subinsts" operation="task" variable="reply" />
          </sequence>
        </scope>
      </while>
    </flow>
  </sequence>
</process>

visualize "the old loop" instead of the whole process
 *)

val thaw_loop_body =
    Sequence[patient_id] o (
      Receive[patient_client, patient_id, run, x, patient_id, patient_id]
`|` Next o Sequence[patient_id] o (
      Thaw[subinsts, patient_id, x, patient_id, patient_id]
`|` Next o
      Reply[patient_client, patient_id, run, y, patient_id, patient_id]
    ));

val thaw_loop =
While[patient_id] o (

    Condition o VariableRef[y, patient_id, patient_id]
`|` thaw_loop_body
);

val task_loop =
While[patient_id] o (

    Condition o VariableRef[y, patient_id, patient_id]

`|` Scope[patient_id][[scope]] o (
        PartnerLinks o <->
    `|` SubLinks     o <->
    `|` Instances    o <->
    `|` Variables o (
            Variable[task, scope]
        `|` Variable[reply, scope]
        )
    `|` Sequence[patient_id] o (
          ReceiveSub[subinsts, patient_id, task, task, scope, patient_id]
    `|` Next o Sequence[patient_id] o (
          Invoke[task_list_UI, patient_id, add_task,
                 task, scope, reply, scope, patient_id]
    `|` Next o (
          ReplySub[subinsts, patient_id, task, reply, scope, patient_id]
        )))
    )
);

val patient_body =
Flow[patient_id] o (
    thaw_loop
`|` task_loop
);

val patient_process = 
Process[patient][[patient_id]] o (

    PartnerLinks o (
        PartnerLink[patient_client, patient_id] o CreateInstance[start]
    `|` PartnerLink[task_list_UI, patient_id]  o <->
    )
`|` SubLinks o SubLink[subinsts, patient_id] o <->
`|` Variables o (
        Variable[x, patient_id] o <->
    `|` Variable[y, patient_id] o True)
`|` Instances o <->

`|` Sequence[patient_id] o (
      Receive[patient_client, patient_id, start, x, patient_id, patient_id]
`|` Next o Sequence[patient_id] o (
      Reply[patient_client, patient_id, start, y, patient_id, patient_id]
`|` Next o
      patient_body
    ))
);

(* An patient instance:
 * FIXME: update instance to reflect updated process

<instance name="engine" id="1">
  <partnerLinks>
    <partnerLink name="engine_client"><link ref="2" /></partnerLink>
  </partnerLinks>
  <subLinks>
    <subLink name="subinsts"><link ref="3" /><link ref="4" /></subLink>
  </subLinks>
  <variables>
    <variable name="x"><process name="treatment">...</process></variable>
    <variable name="y"><true /></variable>
  </variables>
  <instances>
    <instance name="medication" id="3">...</instance>
    <instance name="treatment" id="4">...</instance>
  </instances>
  <sequence>
     <receive partnerLink="engine_client" operation="run" variable="x" />
     <thaw variable="x" subLink="subinsts" />
     <reply   partnerLink="engine_client" operation="run" variable="y" />
     <while>[the same as in the process definition]</while>
  </sequence>
</instance>
 *)
(*
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
            Variable[x, engine_id] o
                Process[treatment][[treatment_scope]] o <->  (* ... *)
        `|` Variable[y, engine_id] o True)
    `|` Instances o (
            Instance[medication, medication_id, active_scopes] o <->  (* ... *)
        `|` Instance[treatment, treatment_id, active_scopes]   o <->) (* ... *)

    `|` Sequence[engine_id] o (
          Receive[engine_client, engine_id, run, x, engine_id, engine_id]
    `|` Next o Sequence[engine_id] o (
          Thaw[subinsts, engine_id, x, engine_id, engine_id]
    `|` Next o Sequence[engine_id] o (
          Reply[engine_client, engine_id, run, y, engine_id, engine_id]
    `|` Next o
          engine_while_loop
        )))
    )
));
*)

(* A doctor process: 
 *
<process name="doctor">
  <partnerLinks>
    <partnerLink name="hospital" />
    <partnerLink name="patient" />
    <partnerLink name="task_list_UI" />
  </partnerLinks>
  <subLinks>
    <subLink name="treatment" />
  </subLinks>
  <variables>
    <variable name="guideline">
      <process name="guideline">...</process>
    </variable>
    <variable name="x" />
    <variable name="y"><from>true()</from></variable>
  </variables>
  <sequence>
    <receive partnerLink="hospital" operation="doctor_hired"
             createInstance="yes" variable="x" />
    <invoke  partnerLink="task_list_UI" operation="init_UI"
             input_variable="x" output_variable="y" />
    <reply   partnerLink="hospital" operation="doctor_hired" variable="y" />
    <flow>
      <while>
        <condition>true()</condition>
        <sequence>
          <receive partnerLink="hospital" operation="patient" variable="x" />
          <reply   partnerLink="hospital" operation="patient" variable="y" />
          <assign><copy>
              <from variable="x" />
              <to   partnerLink="patient" />
          </copy></assign>
          <thaw      subLink="treatment" variable="guideline" />
          <invokeSub subLink="treatment" operation="consultation"
                     inputVariable="y" outputVariable="y" />
          <freeze subLink="treatment" variable="x" />
          <invoke partnerLink="patient" operation="run"
                  inputVariable="x" outputVariable="y" />
        </sequence>
      </while>
      <while>
        <condition>true</condition>
        <scope>
          <variables>
            <variable name="task" />
            <variable name="reply" />
          </variables>
          <sequence>
            <receiveSub subLink="subinsts" operation="task" variable="task" />
            <invoke     partnerLink="task_list_UI" operation="add_task"
                        input_variable="task" output_variable="reply" />
            <replySub   subLink="subinsts" operation="task" variable="reply" />
          </sequence>
        </scope>
      </while>
    </flow>
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
        Variable[x, doctor_id] o <->
    `|` Variable[y, doctor_id] o True) 
`|` Instances o <->

`|` Sequence[doctor_id] o (
      Receive[hospital, doctor_id, doctor_hired, x, doctor_id, doctor_id]   
`|` Next o Sequence[doctor_id] o (
      Reply[hospital, doctor_id, doctor_hired, y, doctor_id, doctor_id]
`|` Next o
      While[doctor_id] o (
          Condition o True

      `|` Sequence[doctor_id] o (
            Receive[hospital, doctor_id, patient, x, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Reply[hospital, doctor_id, patient, y, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Assign[doctor_id] o Copy o (
                From[x, doctor_id] 
            `|` ToPLink[patient, doctor_id])
      `|` Next o Sequence[doctor_id] o (
    (* So there is a sublink called treatment and an operation on a  *)
    (* partnerlink called treatment ???? yep :-) /Espen *)
            Receive[hospital, doctor_id, treatment, x, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Reply[hospital, doctor_id, treatment, y, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Thaw[treatment, doctor_id, x, doctor_id, doctor_id] 
      `|` Next o Sequence[doctor_id] o (
            InvokeSub[treatment, doctor_id, perform_treatment, 
                      y, doctor_id, y, doctor_id, doctor_id]
      `|` Next o Sequence[doctor_id] o (
            Freeze[treatment, doctor_id, x, doctor_id, doctor_id]
      `|` Next o Invoke[patient, doctor_id, run, x, doctor_id,
                         y, doctor_id, doctor_id]
          ))))))))
      )
    ))
);

(* FIXME: update to match process:
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
    <variable name="x"><process name="surgery">...</process></variable>
    <variable name="y"><true /></variable>
  </variables>
  <instances>
    <instance name="surgery" id="6">...</instance>
  </instances>
  <sequence>
    <invokeSub subLink="treatment" operation="perform_treatment"
               inputVariable="y" outputVariable="y" />
    <freeze subLink="treatment" variable="x" />
    <invoke partnerLink="patient" operation="run"
            inputVariable="x" outputVariable="y" />
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
            Variable[x, doctor_id] o
                Process[surgery][[surgery_scope]] o <->  (* ... *)
        `|` Variable[y, doctor_id] o True)
    `|` Instances o Instance[surgery, surgery_id, active_scopes] o <-> (* ... *)
   
    `|` Sequence[doctor_id] o (
          InvokeSub[treatment, doctor_id, perform_treatment, y, doctor_id, 
                    y, doctor_id, doctor_id]
    `|` Next o Sequence[doctor_id] o (
          Freeze[treatment, doctor_id, x, doctor_id, doctor_id]
    `|` Next o Sequence[doctor_id] o (
          Invoke[patient, doctor_id, run, x, doctor_id,
                 y, doctor_id, doctor_id] 
    `|` Next o While[doctor_id] o <-> (* ... *)
        )))
    )
));
*)


(* An example guideline process

<process name="guideline">
  <variables>
    <variable name="x" />
    <variable name="y" />
  </variables>
  <sequence>
    <!-- Doctor initializes treatment -->
    <receiveSup operation="consultation" variable="x" />
    <invokeSup  operation="task" input_variable="x" output_variable="y" />
    <replySup   operation="consultation" variable="x" />
    <!-- Wait to be moved and resumed -->
    <receiveSup operation="resume" variable="x" />
    <replySup   operation="resume" variable="x" />
    <!-- Tell the patient what to do -->
    <invokeSup  operation="task" input_variable="x" output_variable="y" />
  </sequence>
</process>

* FIXME: write BPL term
*)
val guideline =
Process[guideline][[guideline_id]] o (
    PartnerLinks o <->
`|` SubLinks     o <->
`|` Instances    o <->
`|` Variables o (
        Variable[x, guideline_id] o <->
    `|` Variable[y, guideline_id] o <->)

`|` Sequence[guideline_id] o (
      ReceiveSup[consultation, x, guideline_id, guideline_id]
`|` Next o Sequence[guideline_id] o (
      InvokeSup[task, x, guideline_id, y, guideline_id, guideline_id]
`|` Next o Sequence[guideline_id] o (
      ReplySup[consultation, x, guideline_id, guideline_id]
`|` Next o Sequence[guideline_id] o (
      ReceiveSup[resume, x, guideline_id, guideline_id]
`|` Next o Sequence[guideline_id] o (
      ReplySup[resume, x, guideline_id, guideline_id]
`|` Next o (
      InvokeSup[task, x, guideline_id, y, guideline_id, guideline_id]
    ))))))
);
