package com.beepell.variable;

import java.io.IOException;

import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;

/**
 * A complex type variable.
 */
public class ElementVariable extends Variable {

    /**
     * Creates a complex type variable.
     * 
     * @param type the type of this variable
     * @param name the name used to access this variable
     * @param repository Repository of schema definitions used for validation.
     * @throws ParserConfigurationException if it is unable to create the
     *             variable document.
     */
    public ElementVariable(QName type, String name, SchemaRepository repository) throws ParserConfigurationException {
        super(type, name, repository);

    }

    @Override
    public Element getValue() throws UninitializedVariable {
        if (!initialized)
            throw new UninitializedVariable("Variable '" + name + "' is not initialized.");
        else
            return document.getDocumentElement();
    }

    @Override
    public void validate() throws InvalidVariables, UninitializedVariable {

        if (!this.isInitialized())
            throw new UninitializedVariable("Element variable '" + name + "' is not initialized.");

        try {

            repository.getSchema(type).newValidator().validate(new DOMSource(document.getDocumentElement()));

        } catch (SAXException exception) {
            throw new InvalidVariables("Variable '" + name + "' is invalid: " + exception.getLocalizedMessage());
        } catch (IOException exception) {
            throw new IllegalStateException("Variable '" + name + "' could not be validated: " + exception.getLocalizedMessage());
        }

    }

    @Override
    public void initialize() {
        Element documentElement = this.document.createElementNS(type.getNamespaceURI(), type.getLocalPart());
        this.document.appendChild(documentElement);
        this.initialized = true;
    }

    @Override
    public ElementVariable clone() {
        try {
            ElementVariable copy = new ElementVariable(this.getType(), this.getName(), this.repository);
            copy(this, copy);
            return copy;
        } catch (ParserConfigurationException exception) {
            /* Should not happen */
            exception.printStackTrace();
            return null;
        }
    }

    @Override
    public void copyOf(Variable source) throws IllegalArgumentException {        
        copy(source, this);
    }

}
