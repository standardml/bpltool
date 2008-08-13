package com.beepell.broker;

import javax.xml.namespace.QName;

import com.beepell.exceptions.BPELFault;

/**
 * A non-standard fault. 
 * @author Tim Hallwyl
 *
 */
public class MessagingFault extends BPELFault {

    private static final long serialVersionUID = 1L;

    private static final QName name = new QName("http://beepell.com/faults/", "messagingFault");
    
    /**
     * Create a messaging fault.
     * 
     * @param description Short text describing the fault.
     */
    public MessagingFault(String description) {
        super(name, description);
    }
    
    /**
     * Create a messaging fault.
     * 
     * @param description Short text describing the fault.
     * @param cause The Java Throwable that caused the fault.
     */
    public MessagingFault(String description, Throwable cause) {
        super(name, description, cause);
    }

    /**
     * Create a messaging fault.
     * 
     * @param cause The Java Throwable that caused the fault.
     */
    public MessagingFault(Throwable cause) {
        super(name, cause);
    }

}
