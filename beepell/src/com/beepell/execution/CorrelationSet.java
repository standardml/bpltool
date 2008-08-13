package com.beepell.execution;

import java.io.ByteArrayInputStream;
import java.util.Hashtable;
import java.util.List;

import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.Settings;
import com.beepell.exceptions.CorrelationViolation;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.SelectionFailure;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.expression.PropertyAliasQuery;
import com.beepell.repository.ElementPropertyAlias;
import com.beepell.repository.MessagePropertyAlias;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.repository.TypeProperty;
import com.beepell.variable.MessageVariable;

/**
 * @author Tim Hallwyl
 */
public class CorrelationSet {
    private final boolean normalizeCorelationValues = Boolean.parseBoolean(Settings.getInstance().getSetting("execution.correlationset.normalizepropertyvalues", "true"));
    
    private final ServiceRepository repository;

    /*
     * The name of a correlationSet MUST be unique among the names of all
     * correlationSet defined within the same immediately enclosing scope.
     * [SA00044]
     */
    private final String name;

    private final List<QName> properties;

    /*
     * Properties used in a correlationSet MUST be defined using XML Schema
     * simple types. Once initiated, the values are stored under the property
     * QName.
     */
    private final Hashtable<QName, String> values;

    /*
     * A process' correlationSet is in an uninitiated state at the beginning of
     * a process. A scope's correlationSet is in an uninitiated state at the
     * start of the scope to which it belongs. [9.1]
     */
    /*
     * A correlationSet can be initiated only once during the lifetime of the
     * scope to which it belongs. Once initiated, the correlationSet MUST retain
     * its values, regardless of any variable updates. [9.1]
     */
    private boolean isInitiated = false;

    /**
     * @param repository
     * @param name
     * @param properties
     */
    public CorrelationSet(final ServiceRepository repository, final String name, final List<QName> properties) {

        this.repository = repository;
        this.name = name;
        this.properties = properties;
        this.values = new Hashtable<QName, String>(properties.size());
    }

    /**
     * <p>
     * If the correlation set is already initiated, and join is false, a
     * CorrelationViolationException is thrown.
     * <p>
     * If join is true and the correlation set is already initiated and the
     * correlation consistency constraint is violated, a
     * CorrelationViolationException is thrown.
     * <p>
     * In the case in which the application of the property alias results in a
     * response that contains anything other than exactly one information item
     * and/or a collection of Character Information Items then a
     * SelectionException is thrown.
     * 
     * @param join
     * @param variable
     * @param context
     * @throws CorrelationViolation
     * @throws SelectionFailure
     * @throws InvalidExpressionValue
     * @throws SubLanguageExecutionFault
     * @throws UninitializedVariable 
     */
    public void initiate(boolean join, String variable, ExecutionContext context) throws CorrelationViolation, SelectionFailure, SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        MessageVariable message = (MessageVariable) context.getVariable(variable);
        if (isInitiated && !join)
            throw new CorrelationViolation("Correlation set '" + name + "' is already initialized.");

        if (!isInitiated) {

            PropertyAliasQuery query;
            for (QName property : properties) {
                query = getPropertyAliasQuery(message.getType(), property);

                if (query == null)
                    throw new CorrelationViolation("No alias found for property '" + property +"'.");

                Node value = query.evaluate(message.getName(), context);
                if (value == null)
                    throw new CorrelationViolation("Could not initialize correlation set '" + name + "'; Query '"+ query.getExpression() +"' returned null on '"+ message.getName() +"."+ query.getPart() +"'.");
                
                
                String valueString = value.getTextContent();
                if (normalizeCorelationValues) {
                    QName type = ((TypeProperty) repository.getProperty(property)).getType();
                    valueString = normalize(valueString, type, context.getSchemaRepository());
                }
                
                values.put(property, valueString);
            }

            this.isInitiated = true;

        } else if (join) {
            // If we did not initiate, but join is true the check consistency.
            checkConsistency(variable, context);
        }
    }
    /**
     * 
     * @return true, if the correlation set has been initiated, otherwise false.
     */
    public boolean isInitiated() {
        return isInitiated;
    }

    /**
     * <p>
     * If the correlation set has not been previously initiated, a
     * correlationViolationException is thrown.
     * <p>
     * If the correlation set is already initiated and the correlation
     * consistency constraint is violated, a correlationViolationException is
     * thrown.
     * 
     * @param variable
     * @param context 
     * @throws CorrelationViolation
     * @throws InvalidExpressionValue
     * @throws SubLanguageExecutionFault
     * @throws UninitializedVariable 
     */
    public void checkConsistency(String variable, ExecutionContext context) throws CorrelationViolation, SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        MessageVariable message = (MessageVariable) context.getVariable(variable);
        checkConsistency(message, context);
    }
    
    /**
     * <p>
     * If the correlation set has not been previously initiated, a
     * correlationViolationException is thrown.
     * <p>
     * If the correlation set is already initiated and the correlation
     * consistency constraint is violated, a correlationViolationException is
     * thrown.
     * 
     * @param message
     * @param context 
     * @throws CorrelationViolation
     * @throws InvalidExpressionValue
     * @throws SubLanguageExecutionFault
     * @throws UninitializedVariable 
     */
    public void checkConsistency(MessageVariable message, ExecutionContext context) throws CorrelationViolation, SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        if (!isInitiated)
            throw new CorrelationViolation("Correlation set '" + name + "' has not been initialized.");
        
        PropertyAliasQuery query;
        for (QName property : properties) {
            query = getPropertyAliasQuery(message.getType(), property);

            if (query == null)
                throw new CorrelationViolation("Could not initialize correlation set '" + name + "' with a " + message.getType() + " message.");

            Node node = query.evaluate(message.getValue(query.getPart()));
            
            if (node == null)
              throw new CorrelationViolation("Property '"+ property.toString() +"' was not found in '"+ message.getName() +"."+ query.getPart() +"'.");

            String value = node.getTextContent();
            QName type = ((TypeProperty) repository.getProperty(property)).getType();
            
            if (normalizeCorelationValues)
                value = normalize(value, type, context.getSchemaRepository());            
            
            if (!values.get(property).equals(value))
                throw new CorrelationViolation("Correlation constraint violated: Property '" + property + " was " + value + " in " + message.getName() + ", but " + values.get(property) + " was expected.");

        }

    } 

    private static String normalize(String value, QName type, SchemaRepository repository) {

        try {
            String xml = "<typed xmlns:" + type.getPrefix() + "=\"" + type.getNamespaceURI() + "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"" + type.getPrefix() + ":" + type.getLocalPart() + "\">" + value + "</typed>";

            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            dbf.setSchema(repository.getSchema(type));
            Document document = dbf.newDocumentBuilder().parse(new ByteArrayInputStream(xml.getBytes()));
            document.normalizeDocument();
            return document.getDocumentElement().getTextContent();

        } catch (Exception exception) {
            exception.printStackTrace();
            return value;
        }
        
    }
    
    /**
     * Observe that in order to retrieve correlation values from a message, a
     * processor MUST find a matching property alias and apply it to the
     * message. A property alias is considered matching with a message if:
     * <ul>
     * <li>the messageType attribute value used in property alias definition
     * matches the QName of the WSDL message type associated with the message;
     * <li> the message is associated with a WSDL message type where the message
     * contains a single part defined by an element and the element attribute
     * value used in property alias definition matches the QName of the element
     * used to define the WSDL part.
     * </ul>
     */
    private PropertyAliasQuery getPropertyAliasQuery(QName message, QName property) {

        // Priority
        List<MessagePropertyAlias> messagePropertyAliases = repository.getMessagePropertyAliases(property);
        for (MessagePropertyAlias alias : messagePropertyAliases) {
            if (alias.getMessageType().equals(message)) {
                    return new PropertyAliasQuery(alias.getMessageType(), alias.getPart(), alias.getQuery(), alias.getNamespaceContext());
            }
        }

        // Secondary 1) contains a single part 2) defined by an element
        List parts = repository.getMessage(message).getOrderedParts(null);

        // 1) CHECK: contains a single part
        if (parts.size() != 1)
            return null;

        Part part = (Part) parts.get(0);

        // 2) CHECK: defined by an element
        if (part.getElementName() == null)
            return null;

        List<ElementPropertyAlias> elementPropertyAliases = repository.getElementPropertyAliases(property);
        for (ElementPropertyAlias alias : elementPropertyAliases) {
            if (alias.getElement().equals(part.getElementName())) {
                return new PropertyAliasQuery(alias.getElement(), alias.getQuery(), alias.getNamespaceContext());
            }
        }

        // No match found
        return null;

    }

    
    /**
     * @return the name
     */
    public String getName() {
        return name;
    }
    

    /**
     * Gets the initiated value of the property.
     * @param property
     * @return the values the correlation set was initated with.
     */
    public String getCorrelationValue(QName property) {
        return values.get(property);
    }

}
