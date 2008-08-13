package com.beepell.execution;

import java.util.List;

import javax.xml.namespace.QName;

import org.w3c.dom.Node;

import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.structured.Flow;
import com.beepell.activity.structured.Scope;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.expression.ExpressionContext;
import com.beepell.linkgraph.Link;
import com.beepell.model.Activity;
import com.beepell.model.OnMessage;
import com.beepell.model.Target;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.variable.Variable;
import com.sun.xml.xsom.XSType;

/**
 * @author Tim Hallwyl
 */
public interface ExecutionContext extends ExpressionContext {

    /**
     * Creates a new transaction for atomicity and isolation.
     * @return an ExpressionContext that implements Transaction
     */
    public ExecutionContext newTransaction();    
    
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
     */
    public Node getVariableValue(String variable, String part) throws UninitializedVariable;

    /**
     * Initialize a variable, if not initialized. If the variable is already
     * initialized then this has no effect. Used in assign-copy-to.
     * 
     * @param variable
     */
    public void initializeVariable(String variable);

    /**
     * Uninitialize a part of a message variable. Used in assignment when
     * copying a partially initialized message variable to an other message
     * variable.
     * 
     * @param variable
     * @param part
     */
    public void uninitializeVariable(String variable, String part);

    /**
     * Test if the variable is initialized.
     * 
     * @param variable
     * @return true if the variable is initialized.
     */
    public boolean isVariableInitialized(String variable);

    /**
     * Test if the variable part is initialized.
     * 
     * @param variable
     * @param part
     * @return true if the variable part is initialized.
     */
    public boolean isVariableInitialized(String variable, String part);

    /**
     * Initialize a part of a message variable. Used in assign-copy-to.
     * 
     * @param variable
     * @param part
     */
    public void initializeVariable(String variable, String part);

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
     * @param localPart
     * @return null if the link state is unset, otherwise a true or false
     *         Boolean.
     */
    public Boolean getLinkState(String link);

    /**
     * Permanently sets the link status value.
     * 
     * @param link name of the link.
     * @param value a true or false value.
     * @throws IllegalStateException if the link is already set.
     */
    public void setLinkState(String link, boolean value);

    /**
     * Returns true it the source and target is not declared in the same inner
     * scope.
     * 
     * @param source
     * @param link the link to test
     * @return false if the source and target is declared in the same inner
     *         scope.
     */
    public boolean isInterScopeLink(Activity source, String link);

    /**
     * Add a dependency rule to the flow context; When all dependencies are set,
     * the link is set false (dead link).
     * 
     * @param link
     * @param dependencies
     */
    public void addDeadLinkDependency(Link link, List<Link> dependencies);

    /**
     * Adds an object to the list of listeners, on the links specified.
     * 
     * @param listener
     * @param target
     */
    public void addLinkListener(LinkListener listener, List<Target> target);

    /**
     * Perform the "exit" for the Exit activity. If possible, move the
     * implementation of this method back to the Exit activity.
     */
    public void exit();

    /**
     * Validate the variable value against it type definition.
     * 
     * @param variableName variable to validate.
     * @throws InvalidVariables if the variable is invalid.
     * @throws UninitializedVariable 
     */
    public void validate(String variableName) throws InvalidVariables, UninitializedVariable;

    /**
     * Validate the message variable part value against it type definition.
     * 
     * @param messageVariableName variable to validate.
     * @param partName message part to validate.
     * @throws InvalidVariables if the variable is invalid.
     * @throws UninitializedVariable 
     */
    public void validate(String messageVariableName, String partName) throws InvalidVariables, UninitializedVariable;

    /**
     * Checks if partner link role is initialized.
     * 
     * @param partnerLink
     * @return true if the partner link role is initialized, otherwise false.
     */
    public boolean isPartnerRoleInitialized(String partnerLink);

    /**
     * TODO: Move to ProcessContext? This is static.
     * 
     * @return the static ServiceRepository for the process.
     */
    public ServiceRepository getServiceRepository();

    /**
     * TODO: Move to ProcessContext? This is static. Used by CorrelationSet
     * 
     * @return the static SchemaRepository for the process.
     */
    public SchemaRepository getSchemaRepository();

    /**
     * Gets the named partner link. PartnerLinks are declared in scopes.
     * 
     * @param name name of the partner link.
     * @return the named partner link
     */
    public PartnerLink getPartnerLink(String name);

    /**
     * Gets the Variable object. This is used in MessageExchange.
     * 
     * @param name
     * @return the Variable object
     */
    public Variable getVariable(String name);

    /**
     * Gets a live correlation set.
     * 
     * @param set
     * @return the correlation set.
     */
    public CorrelationSet getCorrelationSet(String set);

    /**
     * Gets the current Scope. Used for creation of new scopes, who needs to
     * know their parent.
     * 
     * @return the current Scope
     */
    public Scope getScope();
    
    /**
     * Gets the current (enclosing) flow, if any. Used for creation of new Flows, who needs to
     * know their parent.
     * @return the current flow, or null if none.
     */
    public Flow getFlow();

    /**
     * Create a sub-context using the specified Scope.
     * 
     * @param scope
     * @return a sub-context using the specified Scope.
     */
    public ExecutionContext create(Scope scope);

    /**
     * Create a sub-context using the specified Flow.
     * 
     * @param flow
     * @return a sub-context using the specified Flow.
     */
    public ExecutionContext create(Flow flow);

    /**
     * Returns true if the configuration is the initial starting activity of the
     * process instance.
     * 
     * @param configuration
     * @return true if the configuration is the initial starting activity.
     */
    public boolean isInitialStartingActivity(Activity configuration);

    /**
     * Gets the state of the process instance.
     * 
     * @return the state of the process instance.
     */
    public ProcessInstanceState getInstanceState();

    /**
     * This method is invoked by the initial starting activity after it has
     * registered with the message broker, but before it starts waiting for the
     * message. This will in turn make the inbound message handler deliver the
     * message which created the instance (it has already been received).
     * 
     * @param activity the initial starting activity itself.
     */
    public void setInitialStartingActivityReady(InboundMessageActivity activity);
    
    /**
     * Gets the initial OnMessage that the initial starting message is targeted to. 
     * @return the initial OnMessage
     */
    public OnMessage getInitialOnMessage();
    
    /**
     * This method is invoked by the initial starting activity when it has
     * completed. It will in turn make the process instance change state from
     * STARTING to RUNNING and allow other starting activities to be executed.
     * 
     * @param activity
     */
    public void setInitialStartingActivityComplete(com.beepell.activity.Activity activity);

    /**
     * This method is invoked by other starting activities than the initial
     * starting activity, to be notified then the process goes into RUNNING
     * state.
     * 
     * @param object
     */
    public void notifyOnRunningState(Object object);

    /**
     * Adds an open IMA to the scope.
     * @param ima
     */
    public void addOpenIMA(InboundMessageActivity ima);

    /**
     * Removes (closes) an open IMA in the scope. 
     * @param ima
     */
    public void removeOpenIMA(InboundMessageActivity ima);
    
    /**
     * Gets the matching open IMA.
     * @param partnerLink 
     * @param operation 
     * @param messageExchange 
     * @return the matching open IMA.
     */
    public InboundMessageActivity getOpenIMA(String partnerLink, String operation, String messageExchange);

    /**
     * Gets the name of the variable that the node belongs to.
     * @param node
     * @return null if no variable was found.
     */
    public String getVariable(Node node);

    /**
     * Gets the name of the partner link that the node belongs to (endpoint reference).
     * @param node
     * @return null if no partner link was found.
     */
    public String getPartnerLink(Node node);

    /**
     * Gets the process instance.
     * @return the process instance.
     */
    public ProcessInstance getInstance();
    
}
