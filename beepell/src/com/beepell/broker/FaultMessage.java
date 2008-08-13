package com.beepell.broker;

import javax.xml.namespace.QName;

import com.beepell.exceptions.BPELFault;
import com.beepell.variable.MessageVariable;

/**
 * Used to throw fault messages received from an invoke
 * TODO: we not certain on this is sufficient, or a message variable should be used.
 * @author Tim Hallwyl
 *
 */
public class FaultMessage extends BPELFault {

    private static final long serialVersionUID = 1L;
    /**
     * 
     * @param name 
     * @param data 
     */
    public FaultMessage(QName name, MessageVariable data) {
        super(name, data);
        
    }
}
