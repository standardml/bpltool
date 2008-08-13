package com.beepell.variable;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;

/**
 * Abstract super class for Variables.
 * 
 * @author Tim Hallwyl
 */
public abstract class Variable {

    protected Document document;

    protected final QName type;

    protected final String name;

    protected final SchemaRepository repository;

    protected boolean initialized = false;

    protected Variable(QName type, String name, SchemaRepository repository) throws ParserConfigurationException {
        this.type = type;
        this.name = name;
        this.repository = repository;

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        this.document = builder.newDocument();
        this.document.setUserData("variable", this, null);

    }

    /**
     * Gets the variable type. For element variables, this is the namespace URI
     * and local name.
     * 
     * @return qualified name of the variable type.
     */
    public QName getType() {
        return this.type;
    }

    /**
     * Gets the variable name.
     * 
     * @return name of the variable.
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the variable value.
     * 
     * @return the variable value.
     * @throws UninitializedVariable If the variable is not initialized when
     *             read.
     */
    public abstract Node getValue() throws UninitializedVariable;

    /**
     * Validates the variable against its type.
     * 
     * @throws InvalidVariables if the variable value does not validate against
     *             its type.
     * @throws UninitializedVariable if the variable is not initilized.
     */
    abstract public void validate() throws InvalidVariables, UninitializedVariable;

    /**
     * Initialize the variable.
     */
    abstract public void initialize();

    /**
     * Gets the variables initialized state.
     * 
     * @return true, if the variable has been initialized
     */
    public boolean isInitialized() {
        return initialized;
    }

    /**
     * Creates a clone of this Variable. 
     * 
     * @return A clone of this variable.
     */
    public abstract Variable clone();

    /**
     * Copy the value and initialized state from a variable of same type.
     * 
     * @param source a variable of same type.
     * @throws IllegalArgumentException if the source variable is not of the same type as this.
     */
    public abstract void copyOf(Variable source) throws IllegalArgumentException;

    protected void copy(Variable from, Variable to) {
        if (!from.getType().equals(to.getType()))
            throw new IllegalArgumentException("To and from variables must be of same type.");

        if (from instanceof MessageVariable) {
            copy((MessageVariable) from, (MessageVariable) to);
            return;
        }

        if (from.isInitialized()) {

            if (!to.isInitialized())
                to.initialize();

            try {
                copy(from.getValue(), to.getValue());
            } catch (UninitializedVariable exception) {
                // Should not happen
                exception.printStackTrace();
                System.exit(1);
            }
        }
    }

    protected void copy(Node from, Node to) {
        Node newChild = to.getOwnerDocument().importNode(from, true);
        to.getParentNode().replaceChild(newChild, to);
    }
}
