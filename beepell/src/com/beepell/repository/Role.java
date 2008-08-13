package com.beepell.repository;

import javax.wsdl.PortType;

/**
 * Partner Link Role Class.
 * @author Tim Hallwyl
 *
 */
public class Role {
    
    private String name;
    private PortType portType;

    /**
     * Create a Partner Link Role associated with a WSDL Port Type.
     * @param name
     * @param portType
     */
    public Role(String name, PortType portType) {
        this.name = name;
        this.portType = portType;
    }
    
    

    @Override
    public String toString() {
        
        return name + ": " + portType.toString();
        
    }



    /**
     * Gets the name of the Role
     * @return the name of the Role
     */    
    public String getName() {
        return name;
    }

    /**
     * Gets the WSDL Port Type associated with this Role.
     * @return WSDL Port Type
     */
    public PortType getPortType() {
        return portType;
    }
    
}
