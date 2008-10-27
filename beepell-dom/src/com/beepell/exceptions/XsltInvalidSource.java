package com.beepell.exceptions;

import javax.xml.namespace.QName;

/**
 * <p>
 * WS-BPEL Fault: xsltInvalidSource
 * </p>
 * <p>
 * Thrown when the transformation source provided in a bpel:doXslTransform
 * function call was not legal (i.e., not an EII).
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class XsltInvalidSource extends BPELStandardFault {

    private static final long serialVersionUID = 1L;
    private static final QName name = new QName(namespace, "xsltInvalidSource");

    /**
     * 
     */
    public XsltInvalidSource() {
        super(name);
    }

    /**
     * @param message
     */
    public XsltInvalidSource(String message) {
        super(name, message);
    }


}
