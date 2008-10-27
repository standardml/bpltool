package com.beepell.exceptions;

import javax.xml.namespace.QName;

import org.w3c.dom.Node;

/**
 * @author Tim Hallwyl
 */
public abstract class BPELFault extends Exception {

    private final QName name;

    private final Node data;

    /*
     * Data thrown with a fault can be a WSDL message type or a XML Schema
     * element.
     */


    /**
     * @param name the BPEL Faults qualified name.
     */
    public BPELFault(QName name) {
        this.name = name;
        this.data = null;
    }

    /**
     * @param name the BPEL Faults qualified name.
     * @param data the fault message or element data
     */
    public BPELFault(QName name, Node data) {
        this.name = name;
        this.data = data;
    }

    /**
     * @param name the BPEL Faults qualified name.
     * @param message
     */
    public BPELFault(QName name, String message) {
        super(message);
        this.name = name;
        this.data = null;
    }

    /**
     * @param name the BPEL Faults qualified name.
     * @param cause
     */
    public BPELFault(QName name, Throwable cause) {
        super(cause);
        this.name = name;
        this.data = null;
    }

    /**
     * @param name the BPEL Faults qualified name.
     * @param message
     * @param cause
     */
    public BPELFault(QName name, String message, Throwable cause) {
        super(message, cause);
        this.name = name;
        this.data = null;
    }

    /**
     * Gets the BPEL Faults qualified name.
     * 
     * @return the BPEL Faults qualified name.
     */
    public QName getName() {
        return this.name;
    }

    /**
     * Gets the fault data.
     * @return the fault message or element data
     */
    public Node getData() {
        return this.data;
    }
}
