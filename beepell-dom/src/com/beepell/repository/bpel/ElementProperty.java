package com.beepell.repository.bpel;

import javax.xml.namespace.QName;


/**
 * Element based Property class.
 * <p>
 * This is a WS-BPEL 2.0 specific extension.
 * 
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
     * 
     * @return the QName of the element.
     */
    public QName getElement() {
        return this.element;
    }

}
