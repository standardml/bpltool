package com.beepell.repository.bpel;

import javax.wsdl.PortType;

/**
 * Partner Link Role Class.
 * <p>
 * This is a WS-BPEL 2.0 specific extension.
 * 
 * @author Tim Hallwyl
 * 
 */
public class Role {

    private String name;
    private PortType portType;

    /**
     * Create a Partner Link Role associated with a WSDL Port Type.
     * 
     * @param name
     * @param portType
     */
    public Role(String name, PortType portType) {
        this.name = name;
        this.portType = portType;
    }

    @Override
    public String toString() {

        return this.name + ": " + this.portType.toString();

    }

    /**
     * Gets the name of the Role
     * 
     * @return the name of the Role
     */
    public String getName() {
        return this.name;
    }

    /**
     * Gets the WSDL Port Type associated with this Role.
     * 
     * @return WSDL Port Type
     */
    public PortType getPortType() {
        return this.portType;
    }

}
