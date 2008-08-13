package com.beepell.execution;

import java.util.List;

import javax.xml.namespace.QName;

import org.w3c.dom.Node;

import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.structured.Flow;
import com.beepell.activity.structured.Scope;
import com.beepell.deployment.ProcessContext;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.expression.PropertyAliasQuery;
import com.beepell.linkgraph.Link;
import com.beepell.model.Activity;
import com.beepell.model.OnMessage;
import com.beepell.model.Target;
import com.beepell.repository.ElementProperty;
import com.beepell.repository.ElementPropertyAlias;
import com.beepell.repository.MessagePropertyAlias;
import com.beepell.repository.Property;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.repository.TypeProperty;
import com.beepell.repository.TypePropertyAlias;
import com.beepell.variable.ComplexTypeVariable;
import com.beepell.variable.ElementVariable;
import com.beepell.variable.MessageVariable;
import com.beepell.variable.SimpleTypeVariable;
import com.beepell.variable.Variable;
import com.sun.xml.xsom.XSType;

/**
 * @author Tim Hallwyl
 */
public class ExecutionContextImpl implements ExecutionContext {

  
    protected final ProcessInstance instance;    
    protected final ProcessContext process;
    protected final Scope scope;
    protected final Flow flow;

    /**
     * 
     * @param instance 
     * @param initialStartActivity
     */
    public ExecutionContextImpl(ProcessInstance instance) {
        this.instance = instance;
        this.process = instance.getProcessContext();
        this.scope = null;
        this.flow = null;
    }
    
    /**
     * Create a sub context.
     * @param instance 
     * @param scope
     * @param flow
     */
    public ExecutionContextImpl(ProcessInstance instance, Scope scope, Flow flow) {
        this.instance = instance;
        this.process = instance.getProcessContext();
        this.scope = scope;
        this.flow = flow;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#addDeadLinkDependency(com.beepell.linkgraph.Link,
     *      java.util.List)
     */
    public void addDeadLinkDependency(Link link, List<Link> dependencies) {
        flow.addDeadLinkDependency(link, dependencies);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#addLinkListener(com.beepell.execution.LinkListener,
     *      java.util.List)
     */
    public void addLinkListener(LinkListener listener, List<Target> targets) {
        flow.addLinkListener(listener, targets);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#create(com.beepell.activity.structured.Scope)
     */
    public ExecutionContext create(Scope scope) {
        
        return new ExecutionContextImpl(this.instance, scope, this.flow);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#create(com.beepell.activity.structured.Flow)
     */
    public ExecutionContext create(Flow flow) {
        return new ExecutionContextImpl(this.instance, this.scope, flow);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#exit()
     */
    public void exit() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getCorrelationSet(java.lang.String)
     */
    public CorrelationSet getCorrelationSet(String set) {
        return scope.getCorrelationSet(set);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getLinkState(java.lang.String)
     */
    public Boolean getLinkState(String link) {
        return flow.getLinkState(link);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getPartnerLink(java.lang.String)
     */
    public PartnerLink getPartnerLink(String name) {
        return scope.getPartnerLink(name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getSchemaRepository()
     */
    public SchemaRepository getSchemaRepository() {
        return process.getSchemaRepository();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getScope()
     */
    public Scope getScope() {
        return scope;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getServiceRepository()
     */
    public ServiceRepository getServiceRepository() {
        return process.getServiceRepository();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getType(javax.xml.namespace.QName)
     */
    public XSType getType(QName qName) {
        try {
            return process.getSchemaRepository().getType(qName);
        } catch (Exception exception) {
            exception.printStackTrace();
            return null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getVariable(java.lang.String)
     */
    public Variable getVariable(String name) {
        return scope.getVariable(name);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getVariablePropertyType(javax.xml.namespace.QName)
     */
    public QName getVariablePropertyType(QName propertyName) {
        
        Property property =  process.getServiceRepository().getProperty(propertyName);
        if (property instanceof ElementProperty)
            return ((ElementProperty) property).getElement(); 
        else
            return ((TypeProperty) property).getType(); 
    }



    
    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getVariablePropertyValue(java.lang.String,
     *      javax.xml.namespace.QName)
     */
    public Node getVariablePropertyValue(String variableName, QName propertyName) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable {
        
        PropertyAliasQuery query = null;
        Variable variable = scope.getVariable(variableName);
        
        if (variable instanceof MessageVariable) {
            
            List<MessagePropertyAlias> aliases = process.getServiceRepository().getMessagePropertyAliases(propertyName);            
            for (MessagePropertyAlias alias : aliases) {
                
                if (variable.getType().equals(alias.getMessageType())) {
                    query = new PropertyAliasQuery(alias.getMessageType(), alias.getPart(), alias.getQuery(), alias.getNamespaceContext());
                    break;
                }
                
            }
            
        }
        if (variable instanceof ElementVariable) {
            List<ElementPropertyAlias> aliases = process.getServiceRepository().getElementPropertyAliases(propertyName);            
            for (ElementPropertyAlias alias : aliases) {
                
                if (variable.getType().equals(alias.getElement())) {
                    query = new PropertyAliasQuery(alias.getElement(), alias.getQuery(), alias.getNamespaceContext());
                    break;
                }
                
            }
        }
        
        if (variable instanceof ComplexTypeVariable || variable instanceof SimpleTypeVariable) {
            List<TypePropertyAlias> aliases = process.getServiceRepository().getTypePropertyAliases(propertyName);            
            for (TypePropertyAlias alias : aliases) {
                
                if (variable.getType().equals(alias.getType())) {
                    query = new PropertyAliasQuery(alias.getType(), alias.getQuery(), alias.getNamespaceContext());
                    break;
                }
                
            } 
        }
        
        if (query == null)
            throw new InvalidExpressionValue("Property '" + propertyName + "' was not found for variable '" + variableName + "'.");
        
        return query.evaluate(variableName, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getVariableType(java.lang.String)
     */
    public QName getVariableType(String variable) {
        return scope.getVariable(variable).getType();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getVariableType(java.lang.String,
     *      java.lang.String)
     */
    public QName getVariableType(String variableName, String part) {
        Variable variable = scope.getVariable(variableName);
        if (variable instanceof MessageVariable)
            return ((MessageVariable) variable).getType(part);
        else
            throw new IllegalArgumentException("Variable '" + variableName + "' is not a message type variable.");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getVariableValue(java.lang.String)
     */
    public Node getVariableValue(String variable) throws UninitializedVariable {
        return scope.getVariable(variable).getValue();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#getVariableValue(java.lang.String,
     *      java.lang.String)
     */
    public Node getVariableValue(String variableName, String part) throws UninitializedVariable {        
        Variable variable = scope.getVariable(variableName);
        if (variable instanceof MessageVariable)
            return ((MessageVariable) variable).getValue(part);
        else
            throw new IllegalArgumentException("Variable '" + variableName + "' is not a message type variable.");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#initializeVariable(java.lang.String)
     */
    public void initializeVariable(String variable) {
        scope.getVariable(variable).initialize();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#initializeVariable(java.lang.String,
     *      java.lang.String)
     */
    public void initializeVariable(String variableName, String part) {
        Variable variable = scope.getVariable(variableName);
        if (variable instanceof MessageVariable)
            ((MessageVariable) variable).initialize(part);
        else
            throw new IllegalArgumentException("Variable '" + variableName + "' is not a message type variable.");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#isInterScopeLink(java.lang.String)
     */
    public boolean isInterScopeLink(Activity source, String linkName) {        
        return process.getLinkGraph().isTargetInSameScope(source, linkName);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#isPartnerRoleInitialized(java.lang.String)
     */
    public boolean isPartnerRoleInitialized(String partnerLink) {
        return this.getPartnerLink(partnerLink).getPartnerEndpoint().isInitialized();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#isVariableInitialized(java.lang.String)
     */
    public boolean isVariableInitialized(String variable) {        
        return scope.getVariable(variable).isInitialized();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#isVariableInitialized(java.lang.String,
     *      java.lang.String)
     */
    public boolean isVariableInitialized(String variable, String part) {
        return ((MessageVariable) scope.getVariable(variable)).isInitialized(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#setLinkState(java.lang.String,
     *      boolean)
     */
    public void setLinkState(String link, boolean value) {
        flow.setLinkState(link, value);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#uninitializeVariable(java.lang.String,
     *      java.lang.String)
     */
    public void uninitializeVariable(String variable, String part) {
        ((MessageVariable) scope.getVariable(variable)).uninitialize(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#validate(java.lang.String)
     */
    public void validate(String variableName) throws InvalidVariables, UninitializedVariable {
        scope.getVariable(variableName).validate();

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContext#validate(java.lang.String,
     *      java.lang.String)
     */
    public void validate(String messageVariableName, String partName) throws InvalidVariables, UninitializedVariable {
        ((MessageVariable) scope.getVariable(messageVariableName)).validate(partName);

    }

    public ProcessInstanceState getInstanceState() {
        return instance.getState();
        
    }

    public boolean isInitialStartingActivity(Activity configuration) {        
        return instance.getInitialStartingActivity().equals(configuration);
    }

    public void notifyOnRunningState(Object object) {
        instance.notifyOnRunningState(object);        
    }

    public void setInitialStartingActivityComplete(com.beepell.activity.Activity activity) {
        instance.setInitialStartingActivityComplete();
    }

    public void setInitialStartingActivityReady(InboundMessageActivity activity) {
        instance.setInitialStartingActivity(activity);        
    }
    
    public OnMessage getInitialOnMessage() {
        return instance.getInitialOnMessage();        
    }
    
    public void addOpenIMA(InboundMessageActivity ima) {
        scope.addOpenIMA(ima);
        
    }

    public void removeOpenIMA(InboundMessageActivity ima) {
        scope.removeOpenIMA(ima);
        
    }

    public InboundMessageActivity getOpenIMA(String partnerLink, String operation, String messageExchange) {
        return scope.getOpenIMA(partnerLink, operation, messageExchange);
    }

    public Flow getFlow() {
        return flow;
    }

    public ExecutionContext newTransaction() {
        return new TransactionalExecutionContext(this.instance, this.scope, this.flow);
    }

    /**
     * Gets the name of the variable that the node belongs to.
     * @param node
     * @return null if no variable was found.
     */
    public String getVariable(Node node) {
        Variable variable = (Variable) node.getOwnerDocument().getUserData("variable");
        if (variable == null)
            return null;
        else
            return variable.getName();
    }
    
    /**
     * Gets the name of the partner link that the node belongs to (endpoint reference).
     * @param node
     * @return null if no partner link was found.
     */
    public String getPartnerLink(Node node) {
        PartnerLink partnerLink = (PartnerLink) node.getOwnerDocument().getUserData("partnerlink");
        if (partnerLink == null)
            return null;
        else
            return partnerLink.getName();
    }

    public ProcessInstance getInstance() {
        return instance;
    }
    
}
