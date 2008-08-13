package com.beepell.expression;

import javax.xml.namespace.QName;
import org.w3c.dom.Node;

import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;
import com.sun.xml.xsom.XSType;

/**
 * @author Tim Hallwyl
 */
public interface ExpressionContext {

    /**
     * Gets the variable property type.
     * 
     * @param property
     * @return qualified name of the property type.
     */
    public QName getVariablePropertyType(QName property);

    /**
     * Gets the variable property value.
     * 
     * @param variable The variable to query.
     * @param property The property to query.
     * @return the property value of the variable.
     * @throws UninitializedVariable 
     * @throws InvalidExpressionValue 
     * @throws SubLanguageExecutionFault 
     */
    public Node getVariablePropertyValue(String variable, QName property) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable;

    /**
     * Gets the variable type.
     * 
     * @param variable
     * @return qualified name of the variable type.
     */
    public QName getVariableType(String variable);

    /**
     * Gets the variable value.
     * 
     * @param variable
     * @return the variable value.
     * @throws UninitializedVariable if the variable has not been initialized
     */
    public Node getVariableValue(String variable) throws UninitializedVariable;

    /**
     * Gets the variable part type.
     * 
     * @param variable
     * @param part
     * @return qualified name of the variable type.
     */
    public QName getVariableType(String variable, String part);

    /**
     * Gets the message type variable part value.
     * 
     * @param variable
     * @param part
     * @return the message type variable part value
     * @throws UninitializedVariable if the variable has not been initialized
     */
    public Node getVariableValue(String variable, String part) throws UninitializedVariable;

    /**
     * Gets the XSType object for a qualified named type.
     * 
     * @param qName
     * @return XSType object.
     */
    public XSType getType(QName qName);

    /**
     * Gets the link state as a Boolean object. This method returns null if the
     * link state is unset, otherwise true or false.
     * 
     * @param link name of the link.
     * @return null if the link state is unset, otherwise a true or false
     *         Boolean.
     */
    public Boolean getLinkState(String link);


}
