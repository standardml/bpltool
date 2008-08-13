package com.beepell.repository;

import javax.xml.namespace.QName;

/**
 * Element based Property class.
 * @author Tim Hallwyl
 *
 */
public class ElementProperty extends Property {
    
    private QName element;
    
    /**
     * 
     * @param name
     * @param element
     */
    public ElementProperty(QName name, QName element) {
        this.name = name;
        this.element = element;
    }
    
    /**
     * Gets the QName of the element.
     * @return the QName of the element.
     */
    public QName getElement() {
        return element;
    }
    
}
