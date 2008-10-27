package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: uninitializedPartnerRole
 * </p>
 * <p>
 * Thrown when an &lt;invoke&gt; or &lt;assign&gt; activity references a partner
 * link whose partnerRole endpoint reference is not initialized.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class UninitializedPartnerRole extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "uninitializedPartnerRole");

    /**
     * 
     */
    public UninitializedPartnerRole() {
        super(name);
    }

    /**
     * @param message
     */
    public UninitializedPartnerRole(String message) {
        super(name, message);
    }
 
}
