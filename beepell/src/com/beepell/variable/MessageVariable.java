package com.beepell.variable;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.sun.xml.xsom.XSType;

/**
 * A Message Variable.
 * 
 * @author Tim Hallwyl
 */
public class MessageVariable extends Variable {

    Hashtable<String, Variable> parts;

    ServiceRepository services = null;

    /**
     * Creates a Message Variable.
     * 
     * @param type the type of this variable
     * @param name the name used to access this variable
     * @param schemas Repository of schema definitions used for validation and
     *            creation of parts.
     * @param services Repository of message definitions used for validation and
     *            creation of parts.
     * @throws ParserConfigurationException
     */
    public MessageVariable(QName type, String name, SchemaRepository schemas, ServiceRepository services) throws ParserConfigurationException {
        super(type, name, schemas);
        this.services = services;
    }

    @Override
    public Node getValue() throws UninitializedVariable {
        return null;
    }

    /**
     * Gets the variable value of the specified part.
     * 
     * @param part
     * @return the variable value node of the specified part.
     * @throws UninitializedVariable
     */
    public Node getValue(String part) throws UninitializedVariable {
        if (!isInitialized())
            throw new UninitializedVariable("Variable '" + name + "' is not initialized");

        Variable partVariable = parts.get(part);
        if (partVariable == null)
            throw new UninitializedVariable("No part '" + part + "' in '" + type + "'.");

        return partVariable.getValue();
    }

    /**
     * Gets the variable type of the specified part. For element parts, this is
     * the namespace URI and local name.
     * 
     * @param part
     * @return qualified name of the variable type.
     */
    public QName getType(String part) {
        return parts.get(part).getType();
    }

    @Override
    public void initialize() {
        try {
            Message message = services.getMessage(type);
            Map map = message.getParts();
            this.parts = new Hashtable<String, Variable>(map.size());

            String key;
            Variable variable;
            Iterator keys = map.keySet().iterator();
            while (keys.hasNext()) {
                key = (String) keys.next();
                Part part = (Part) map.get(key);
                variable = create(part);
                variable.initialize();        
                variable.getValue().getOwnerDocument().setUserData("variable", this, null);
                variable.getValue().getOwnerDocument().setUserData("part", part.getName(), null);
                this.parts.put(part.getName(), variable);
            }

            this.initialized = true;
        } catch (Exception exception) {
            throw new IllegalStateException("Unexpected exception in variable initialization.", exception);
        }
    }

    /**
     * @param part name of the message part to initialize
     */
    public void initialize(String part) {
        try {
            if (parts == null) {
                Message message = services.getMessage(type);
                Map map = message.getParts();
                this.parts = new Hashtable<String, Variable>(map.size());
            }

            // Get the part variable, if it exists,
            // or create it
            Variable variable = parts.get(part);
            if (variable == null) {
                Part p = services.getMessage(this.type).getPart(part);
                if (p == null)
                    throw new IllegalArgumentException("Part '" + part + "' not found in " + this.type);
                else
                    variable = create(p);
            }

            // If the part variable is not initialized, do it.
            if (!variable.isInitialized()) {
                variable.initialize();
                variable.getValue().getOwnerDocument().setUserData("variable", this, null);
                variable.getValue().getOwnerDocument().setUserData("part", part, null);
            }
            parts.put(part, variable);

            this.initialized = true;
        } catch (Exception exception) {
            throw new IllegalStateException("Unexpected exception in variable initialization.", exception);
        }

    }

    /**
     * Gets the initilized state on a message part.
     * @param part
     * @return true if the part is initilized.
     */
    public boolean isInitialized(String part) {
        
        if (parts == null)
            return false;
        
        Variable variable = parts.get(part);
        if (variable == null)
            return false;
        
        return variable.isInitialized();
        
    }
    
    /**
     * Uninitialize a part of the varaible. This is used by assignment when
     * copying an uninitialized part.
     * 
     * @param part
     */
    public void uninitialize(String part) {
        try {
            Part p = services.getMessage(this.type).getPart(part);
            Variable variable;

            if (p == null)
                throw new IllegalArgumentException("Part '" + part + "' not found in " + this.type);
            else
                variable = create(p);

            parts.put(part, variable);
            
            
        } catch (Exception exception) {
            throw new IllegalStateException("Unexpected exception in variable initialization.", exception);
        }

    }

    /**
     * Gets the variable's initialized state. This is true if at least one of
     * the message parts is initialized.
     * 
     * @return false if all parts are uninitialized, otherwise true.
     */
    public boolean isInitialized() {
        if (parts == null)
            return false;

        for (Variable part : parts.values()) {
            if (part.isInitialized()) {
                this.initialized = true;
                return true;
            }
        }
        
        this.initialized = false;
        return false;
    }

    @Override
    public void validate() throws InvalidVariables, UninitializedVariable {
        if (!this.isInitialized())
            throw new UninitializedVariable("Message variable '" + name + "' is not initialized.");

        Variable variable;
        List<Part> parts = getParts();
        for (Part part : parts) {
            variable = this.parts.get(part.getName());
            if (variable == null)
                throw new UninitializedVariable("Message variable '" + name + "', part '"+ part.getName() +"' is not initialized.");
            else
                variable.validate();
        }

    }
    
    /**
     * Validate a single part.
     * @param part
     * @throws InvalidVariables
     * @throws UninitializedVariable 
     */
    public void validate(String part) throws InvalidVariables, UninitializedVariable {
        parts.get(part).validate();
    }

    /**
     * Creates an uninitialized variable from a Part definition.
     * 
     * @param part
     * @return Variable of same name and type as the Part.
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    private Variable create(Part part) throws ParserConfigurationException, SAXException {
        Variable variable = null;

        if (part.getElementName() != null) {
            variable = new ElementVariable(part.getElementName(), part.getName(), repository);
        }

        if (part.getTypeName() != null) {
            XSType type = repository.getType(part.getTypeName());

            if (type.isSimpleType()) {
                variable = new SimpleTypeVariable(part.getTypeName(), part.getName(), repository);
            }

            if (type.isComplexType()) {
                variable = new ComplexTypeVariable(part.getTypeName(), part.getName(), repository);
            }

        }

        return variable;
    }

    /**
     * Gets the ordered list of part definitions.
     * 
     * @return the ordered list of part definitions.
     */
    @SuppressWarnings("unchecked")
    public List<Part> getParts() {
        return services.getMessage(type).getOrderedParts(null);

    }
    
    @Override
    public MessageVariable clone() {
        try {
            MessageVariable copy = new MessageVariable(this.getType(), this.getName(), this.repository, this.services);
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
        if (!(source instanceof MessageVariable))
            throw new IllegalArgumentException("Attempt to copy of from a non-message variable.");
        
        copy((MessageVariable) source, this);
    }
    
    private void copy(MessageVariable from, MessageVariable to) {
        if (!from.getType().equals(to.getType()))
            throw new IllegalArgumentException("To and from variables must be of same type.");

        List<Part> parts = from.getParts();
        String name;
        for (Part part : parts) {
            name = part.getName();

            if (from.isInitialized(name) && !to.isInitialized(name)) {
                to.initialize(name);
            }

            if (!from.isInitialized(name) && to.isInitialized(name)) {
                to.uninitialize(name);
            }

            if (from.isInitialized(name)) {

                try {
                    copy(from.getValue(name), to.getValue(name));
                } catch (UninitializedVariable exception) {
                    // Should not happen
                    exception.printStackTrace();
                    System.exit(1);
                }

            }

        }

    }
}
