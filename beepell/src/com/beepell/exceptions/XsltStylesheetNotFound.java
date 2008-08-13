package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: xsltStylesheetNotFound
 * </p>
 * <p>
 * Thrown when the named style sheet in a bpel:doXslTransform function call was
 * not found.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class XsltStylesheetNotFound extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "xsltStylesheetNotFound");

    /**
     * 
     */
    public XsltStylesheetNotFound() {
        super(name);
    }

    /**
     * @param message
     */
    public XsltStylesheetNotFound(String message) {
        super(name, message);
    }

}
