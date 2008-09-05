package com.beepell.repository.bpel;

import javax.xml.namespace.QName;

/**
 * Property class.
 * <p>
 * This is a WS-BPEL 2.0 specific extension.
 * 
 * @author Tim Hallwyl
 * 
 */
public class Property {

    protected QName name;

    /**
     * 
     * @return the name of the property.
     */
    public QName getName() {
        return this.name;
    }

}
