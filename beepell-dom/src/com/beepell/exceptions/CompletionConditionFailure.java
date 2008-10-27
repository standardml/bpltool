package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: completionConditionFailure
 * </p>
 * <p>
 * Thrown if upon completion of a directly enclosed &lt;scope&gt; activity
 * within &lt;forEach&gt; activity it can be determined that the completion
 * condition can never be true.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class CompletionConditionFailure extends BPELStandardFault {

    private static final long serialVersionUID = 1L;

    private static final QName name = new QName(namespace, "completionConditionFailure");

    /**
     * Create a completionConditionFailure.
     */
    public CompletionConditionFailure() {
        super(name);
    }

    /**
     * Create a completionConditionFailure with a message.
     * 
     * @param message
     */
    public CompletionConditionFailure(String message) {
        super(name, message);
    }

}
