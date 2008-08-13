package com.beepell.broker;

import java.util.Hashtable;
import java.util.List;

import javax.xml.namespace.QName;

import org.w3c.dom.Node;

import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.activity.structured.Flow;
import com.beepell.activity.structured.Scope;
import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.execution.CorrelationSet;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.LinkListener;
import com.beepell.execution.PartnerLink;
import com.beepell.execution.ProcessInstance;
import com.beepell.execution.ProcessInstanceState;
import com.beepell.linkgraph.Link;
import com.beepell.model.Activity;
import com.beepell.model.OnMessage;
import com.beepell.model.Target;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.variable.MessageVariable;
import com.beepell.variable.Variable;
import com.sun.xml.xsom.XSType;


/**
 * @author Tim Hallwyl
 *
 */
public class CorrelationContext implements ExecutionContext {
    
    private Hashtable<String, CorrelationSet> correlationSets = new Hashtable<String, CorrelationSet>();
    private Hashtable<String, Variable> variables = new Hashtable<String, Variable>();

    private final SchemaRepository schemas;
    private final ServiceRepository services;

    /**
     * @param schemas
     * @param services
     */
    public CorrelationContext(SchemaRepository schemas,ServiceRepository services) {
        this.schemas = schemas;
        this.services = services;
    }
    
    
    public void addDeadLinkDependency(Link link, List<Link> dependencies) {
        throw new UnsupportedOperationException();
    }

    public void addLinkListener(LinkListener listener, List<Target> target) {
        throw new UnsupportedOperationException();
    }

    public void exit() {
        throw new UnsupportedOperationException();
    }

    public CorrelationSet getCorrelationSet(String set) {
        return correlationSets.get(set);
    }
    
    /**
     * @param set 
     */
    public void addCorrelationSet(CorrelationSet set) {
        correlationSets.put(set.getName(), set);
    }

    public Boolean getLinkState(String link) {
        throw new UnsupportedOperationException();
    }

    public PartnerLink getPartnerLink(String name) {
        throw new UnsupportedOperationException();
    }

    public ServiceRepository getServiceRepository() {
        return services;
    }

    public XSType getType(QName qName) {
        try { return schemas.getType(qName); } catch (Exception exception) {throw new UnsupportedOperationException(); }
    }

    public Variable getVariable(String name) {
        return variables.get(name);
    }
    
    /**
     * @param variable
     */
    public void addVariable(Variable variable) {
        variables.put(variable.getName(), variable);
    }

    public QName getVariablePropertyType(QName property) {
        throw new UnsupportedOperationException();
    }

    public Node getVariablePropertyValue(String variable, QName property) {
        throw new UnsupportedOperationException();
    }

    public QName getVariableType(String variable, String part) {
        throw new UnsupportedOperationException();
    }

    public QName getVariableType(String variable) {
        return variables.get(variable).getType();
    }

    public Node getVariableValue(String variable, String part) throws UninitializedVariable {
        
        System.out.print("Context lookup variable '" + variable + "', part '" + part + "': ");
        MessageVariable mv = ((MessageVariable) variables.get(variable));
        
        if (mv == null)
            System.out.println("not found.");
        else
            System.out.println("found."); 
        
        return mv.getValue(part);
    }

    public Node getVariableValue(String variable) throws UninitializedVariable {
        System.out.print("Context lookup variable '" + variable + "': ");
        MessageVariable mv = ((MessageVariable) variables.get(variable));
        
        if (mv == null)
            System.out.println("not found.");
        else
            System.out.println("found."); 
        
        return mv.getValue();
    }



    public boolean isPartnerRoleInitialized(String partnerLink) {
        throw new UnsupportedOperationException();
    }

    public void setLinkState(String link, boolean value) {
        throw new UnsupportedOperationException();
    }

    public void validate(String messageVariableName, String partName) throws InvalidVariables {
        throw new UnsupportedOperationException();
    }

    public void validate(String variableName) throws InvalidVariables {
        throw new UnsupportedOperationException();
    }

    public SchemaRepository getSchemaRepository() {
        return schemas;
    }


    public Scope getScope() {
        // TODO Auto-generated method stub
        return null;
    }


    public void initializeVariable(String variable) {
        // TODO Auto-generated method stub
        
    }


    public void initializeVariable(String variable, String part) {
        // TODO Auto-generated method stub
        
    }


    public boolean isVariableInitialized(String variable) {
        // TODO Auto-generated method stub
        return false;
    }


    public boolean isVariableInitialized(String variable, String part) {
        // TODO Auto-generated method stub
        return false;
    }


    public void uninitializeVariable(String variable, String part) {
        // TODO Auto-generated method stub
        
    }


    public ExecutionContext create(Scope scope) {
        // TODO Auto-generated method stub
        return null;
    }


    public ExecutionContext create(Flow flow) {
        // TODO Auto-generated method stub
        return null;
    }


    public boolean isInterScopeLink(Activity source, String link) {
        // TODO Auto-generated method stub
        return false;
    }


    public ProcessInstanceState getInstanceState() {
        // TODO Auto-generated method stub
        return null;
    }


    public boolean isInitialStartingActivity(Activity configuration) {
        // TODO Auto-generated method stub
        return false;
    }


    public void notifyOnRunningState(Object object) {
        // TODO Auto-generated method stub
        
    }


    public void setInitialStartingActivityComplete(com.beepell.activity.Activity activity) {
        // TODO Auto-generated method stub
        
    }


    public void setInitialStartingActivityReady(InboundMessageActivity activity) {
        // TODO Auto-generated method stub
        
    }


    public void addOpenIMA(InboundMessageActivity ima) {
        // TODO Auto-generated method stub
        
    }


    public InboundMessageActivity getOpenIMA(String partnerLink, String operation, String messageExchange) {
        // TODO Auto-generated method stub
        return null;
    }


    public void removeOpenIMA(InboundMessageActivity ima) {
        // TODO Auto-generated method stub
        
    }


    public Flow getFlow() {
        // TODO Auto-generated method stub
        return null;
    }


    public String getPartnerLink(Node node) {
        // TODO Auto-generated method stub
        return null;
    }


    public String getVariable(Node node) {
        // TODO Auto-generated method stub
        return null;
    }


    public ExecutionContext newTransaction() {
        // TODO Auto-generated method stub
        return null;
    }


    public OnMessage getInitialOnMessage() {
        // TODO Auto-generated method stub
        return null;
    }


    public ProcessInstance getInstance() {
        // TODO Auto-generated method stub
        return null;
    }

 
}
