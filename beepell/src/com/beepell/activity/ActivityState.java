package com.beepell.activity;

/**
 * Enumeration over activity states.
 * 
 * @author Tim Hallwyl
 */
public enum ActivityState {

    /**
     * From the time the activity object is constructed until it is ready to be
     * executed, it is in INITIALIZING state. In this state configuration is
     * loaded and perpetrations are made.
     */
    INITIALIZING,

    /**
     * Waiting for incoming links to be determined.
     */
    WAITING,

    /**
     * When an activity object is done with initialization, it is ready to be
     * executed. In this period, while waiting to be executed, the activity is
     * in READY state.
     */
    READY,

    /**
     * From the moment the activity starts executing, until it is finished,
     * failed or terminated, it is in RUNNING state. This is when the activity
     * performs it tasks.
     */
    RUNNING,

    /**
     * When the activity success has completed its tasks, it reaches COMPLETED
     * state.
     */
    COMPLETED,

    /**
     * When the join condition of the activity fails or evaluates false AND
     * suppress join failure is "yes" - then the activity is silently skipped.
     * When working with scopes we need to distinguish COMPLETED from SKIPPED
     * because compensation applies only in COMPLETED state.
     */
    SKIPPED,

    /**
     * A Scope specific state, when the fault handler has been invoked, it goes
     * into FAILED state on completion of the fault handler.
     */
    FAILING,

    /**
     * If the activity throws a failure, it goes into FAILED state, and never
     * reaches FINISHED state.
     */
    FAILED,

    /**
     * An activity may be asked to terminate prematurely. Termination may take
     * some time, for example when waiting for enclosed activities to terminate.
     * During this time the activity is in TERMINATING state.
     */
    TERMINATING,

    /**
     * When an activity has completed a premature termination it goes into
     * TERMINATED state, and never reaches FINISHED state.
     */
    TERMINATED,

    /**
     * A Scope specific state, when the compensation handler has been invoked
     * but not yet completed.
     */
    COMPENSATING,

    /**
     * A Scope specific state, when the compensation handler has successfully
     * completed.
     */
    COMPENSATED
}
