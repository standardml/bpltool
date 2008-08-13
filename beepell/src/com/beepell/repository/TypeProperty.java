package com.beepell.repository;

import javax.xml.namespace.QName;

/**
 * Type Property.
 * @author Tim Hallwyl
 *
 */
public class TypeProperty extends Property {

    private QName type;
    
    /**
     * Creates a type based Property
     * @param name
     * @param type
     */
    public TypeProperty(QName name, QName type) {
        this.name = name;
        this.type = type;
    }

    /**
     * Gets the type.
     * @return the type.
     */
    public QName getType() {
        return type;
    }
}
