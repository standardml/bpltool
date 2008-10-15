package com.beepell.execution;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Node;

import com.beepell.Settings;
import com.beepell.activity.structured.Flow;
import com.beepell.activity.structured.Scope;
import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.execution.transaction.LockManager;
import com.beepell.execution.transaction.LockType;
import com.beepell.execution.transaction.Transaction;
import com.beepell.variable.MessageVariable;
import com.beepell.variable.Variable;

/**
 * @author Tim Hallwyl
 */
public class TransactionalExecutionContext extends ExecutionContextImpl implements Transaction {
    private static final Logger log = Settings.getLogger();

    private LockManager lockManager = LockManager.getInstance();

    private Hashtable<String, Variable> tentativeVariables = new Hashtable<String, Variable>();

    private static DocumentBuilder documentBuilder;

    /**
     * An execution contexct with transactional behavior: atomicity and isolation.
     * @param instance
     * @param scope 
     * @param flow 
     */
    public TransactionalExecutionContext(ProcessInstance instance, Scope scope, Flow flow)  {
        super(instance, scope, flow);

        try {
            if (documentBuilder == null)
                documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        } catch (ParserConfigurationException exception) {
            exception.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Acquire a write lock on the variable.
     * 
     * @param variableName
     */
    public synchronized void writeLockVariable(String variableName) {
        Variable variable = super.getVariable(variableName);
        if (variable != null) {
            lockManager.lock(variable, this, LockType.WRITE);
            if (!tentativeVariables.containsKey(variable.getName()))
                tentativeVariables.put(variable.getName(), variable.clone());
            return;
        }
    }

    /**
     * Acquire a write lock on the partner link 'partner' role end-point .
     * 
     * @param partnerLinkName
     */
    public synchronized void writeLockPartnerLink(String partnerLinkName) {
        PartnerLink partnerLink = getPartnerLink(partnerLinkName);
        lockManager.lock(partnerLink, this, LockType.WRITE);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.transaction.Transaction#abort()
     */
    public synchronized void abort() {
        tentativeVariables.clear();
        lockManager.release(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.transaction.Transaction#commit()
     */
    public synchronized void commit() {
        // TODO: Must be atomic!!

        Variable copy, variable;
        Enumeration<Variable> tentative = tentativeVariables.elements();

        while (tentative.hasMoreElements()) {
            copy = tentative.nextElement();
            variable = super.getVariable(copy.getName());           
            
            if (lockManager.getLockType(variable) == LockType.WRITE) {
                try {log.info("Committing variable '" + copy.getName() + "'.");} catch (Exception a) {};
                variable.copyOf(copy);
            }
        }

        lockManager.release(this);
    }
    
    private Variable read(String variableName) {
                
        if (tentativeVariables.containsKey(variableName)) {
            return tentativeVariables.get(variableName);
        } else {
            Variable variable = super.getVariable(variableName);
            lockManager.lock(variable, this, LockType.READ);
            Variable copy = variable.clone(); //copy(variable);
            tentativeVariables.put(copy.getName(), copy);
            return copy;
        }
    }
    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContextImpl#getPartnerLink(java.lang.String)
     */
    @Override
    public PartnerLink getPartnerLink(String name) {
        PartnerLink partnerLink = super.getPartnerLink(name);
        lockManager.lock(partnerLink, this, LockType.READ);
        return partnerLink;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContextImpl#getVariable(java.lang.String)
     */
    @Override
    public Variable getVariable(String name) {
        return read(name);
    }



    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContextImpl#getVariableValue(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public Node getVariableValue(String variableName, String part) throws UninitializedVariable {
        return ((MessageVariable) read(variableName)).getValue(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContextImpl#getVariableValue(java.lang.String)
     */
    @Override
    public Node getVariableValue(String variableName) throws UninitializedVariable {
        return read(variableName).getValue();        
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContextImpl#isPartnerRoleInitialized(java.lang.String)
     */
    @Override
    public boolean isPartnerRoleInitialized(String partnerLink) {
        lockManager.lock(super.getPartnerLink(partnerLink), this, LockType.READ);
        return super.isPartnerRoleInitialized(partnerLink);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContextImpl#isVariableInitialized(java.lang.String,
     *      java.lang.String)
     */
    @Override
    public boolean isVariableInitialized(String variable, String part) {
        return ((MessageVariable) read(variable)).isInitialized(part);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.beepell.execution.ExecutionContextImpl#isVariableInitialized(java.lang.String)
     */
    @Override
    public boolean isVariableInitialized(String variable) {
        return read(variable).isInitialized();        
    }
    
    

    /* (non-Javadoc)
     * @see com.beepell.execution.ExecutionContextImpl#initializeVariable(java.lang.String, java.lang.String)
     */
    @Override
    public void initializeVariable(String variableName, String part) {
        writeLockVariable(variableName);
        ((MessageVariable) tentativeVariables.get(variableName)).initialize(part);        
    }

    /* (non-Javadoc)
     * @see com.beepell.execution.ExecutionContextImpl#initializeVariable(java.lang.String)
     */
    @Override
    public void initializeVariable(String variable) {
        writeLockVariable(variable);
        tentativeVariables.get(variable).initialize();        
    }
    
    

    /* (non-Javadoc)
     * @see com.beepell.execution.ExecutionContextImpl#validate(java.lang.String, java.lang.String)
     */
    @Override
    public void validate(String messageVariableName, String partName) throws InvalidVariables, UninitializedVariable {
        ((MessageVariable) read(messageVariableName)).validate(partName);
    }

    /* (non-Javadoc)
     * @see com.beepell.execution.ExecutionContextImpl#validate(java.lang.String)
     */
    @Override
    public void validate(String variableName) throws InvalidVariables, UninitializedVariable {
        read(variableName).validate();

    }

    public Transaction getParent() {
        // TODO Auto-generated method stub
        return null;
    }

}
