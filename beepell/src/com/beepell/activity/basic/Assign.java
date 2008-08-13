package com.beepell.activity.basic;

import java.util.ArrayList;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.w3c.dom.TypeInfo;

import com.beepell.activity.ActivityState;
import com.beepell.exceptions.BPELFault;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.MismatchedAssignmentFailure;
import com.beepell.exceptions.SelectionFailure;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedPartnerRole;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.execution.PartnerLink;
import com.beepell.execution.transaction.Transaction;
import com.beepell.model.AssignActivity;
import com.beepell.model.Copy;
import com.beepell.model.From;
import com.beepell.model.RolesEnumeration;
import com.beepell.model.To;
import com.beepell.repository.SchemaRepository;
import com.beepell.util.XML;
import com.beepell.variable.MessageVariable;
import com.beepell.variable.Variable;

/**
 * Assign Activity.
 * <p>
 * The assign activity can be used to copy data from one variable to another, as
 * well as to construct and insert new data using expressions. The use of
 * expressions is primarily motivated by the need to perform simple computation
 * (such as incrementing sequence numbers). Expressions operate on variables,
 * properties, and literal constants to produce a new value. The assign activity
 * can also be used to copy endpoint references to and from partnerLinks.
 * <p>
 * When validate is set to true, the assign activity validates all the variables
 * being modified by the activity. ... If the validate part of the assign
 * activity fails, that is, one of the variables is invalid against its
 * corresponding XML definition, a standard fault bpel:invalidVariables MUST be
 * thrown.
 * <p>
 * The assign activity MUST be executed as if, for the duration of its
 * execution, it was the only activity in the process being executed.
 * 
 * @author Tim Hallwyl
 */
public class Assign extends AbstractBasicActivity {

    private final List<Copy> copies;

    private final boolean validate;
    
    private final List<String> modified;

    /**
     * Create an assign activity.
     * 
     * @param configuration
     */
    public Assign(AssignActivity configuration) {
        super(configuration);
        copies = configuration.getCopy();
        validate = configuration.isValidate();
        modified = new ArrayList<String>();
        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {
        this.context = context.newTransaction();

        try {
            
            for (Copy copy : copies) {
                copy(copy);
            }

            // Validate all modified variables, if requested
            if (validate) {
                for (String variable: modified)
                    context.validate(variable);
            }
            
        } catch (BPELFault exception) {
            
            ((Transaction) context).abort();
            throw exception;
        
        }       


        ((Transaction) context).commit();
    }

    /**
     * Tests if the Copy operation is a copy from one whole MessageVariable to
     * another whole MessageVarable. If so, this method check compatibility.
     * 
     * @param copy
     * @return true if both to and from are whole MessageVariables.
     * @throws MismatchedAssignmentFailure
     */
    private boolean isMessageVariableCopy(Copy copy) throws MismatchedAssignmentFailure {

        From from = copy.getFrom();
        To to = copy.getTo();

        boolean isFromMessage = false;
        boolean isToMessage = false;

        Variable fromVariable = null;
        Variable toVariable = null;

        if (from.getVariable() != null && from.getPart() == null && from.getProperty() == null && from.getQueryElementNode() == null) {
            fromVariable = context.getVariable(from.getVariable());
            if (fromVariable instanceof MessageVariable)
                isFromMessage = true;
        }

        if (to.getVariable() != null && to.getPart() == null && to.getProperty() == null && to.getQueryElementNode() == null) {
            toVariable = context.getVariable(to.getVariable());
            if (toVariable instanceof MessageVariable)
                isToMessage = true;
        }

        if (!isFromMessage && !isToMessage)
            return false;

        // the selection result of the from-spec is a variable of a WSDL message
        // type and that of the to-spec is not, or vice versa
        if ((isFromMessage && !isToMessage) || (!isFromMessage && isToMessage))
            throw new MismatchedAssignmentFailure("Both the to-spec or from-spec must select a message variable or none of them.");

        // the selection results of both the from-spec and the to-spec are
        // variables of a WSDL message type, and the two variables are not of
        // the same WSDL message type
        if (!fromVariable.getType().equals(toVariable.getType()))
            throw new MismatchedAssignmentFailure("Cannot copy a '" + fromVariable.getType() + "' message into a '" + toVariable.getType() + "'.");

        return true;
    }

    private void copy(Node from, Node to, boolean ignoreMissingFromData, boolean isKeepSrcElementName) throws MismatchedAssignmentFailure, SelectionFailure {
        
        if (from == null) {
            if (ignoreMissingFromData)
                return;
            else
                throw new SelectionFailure("The from-spec did not select a node.");
        }

        if (to == null)
            throw new SelectionFailure("The to-spec did not select a node.");

        // Write lock and add to list of modified variables
        String partnerLink = null;
        String variable = context.getVariable(to);
        if (variable != null) {
            this.modified.add(variable);
            ((Transaction) context).writeLockVariable(variable);
        } else {
            // 'to' node must be a parter role endpoint
            partnerLink = context.getPartnerLink(to);
            ((Transaction) context).writeLockPartnerLink(partnerLink);
        }
        
        if (variable != null)
            log.info("Assign copy '" + XML.toString(from) + "' into variable '" + variable + "' "+ XML.toPath(to) +".");
        else
            log.info("Assign copy '" + XML.toString(from) + "' into partner link '" + partnerLink + "'.");
        
        
        if (to instanceof Element && from instanceof Element) {
            // Replace element
            
            // If not keepSrcElementName, remember destination name
            QName destination = null;
            if (!isKeepSrcElementName)
                destination = new QName(to.getNamespaceURI(), to.getLocalName());

            else if (!substitutable(to, from))
                throw new MismatchedAssignmentFailure("The element selected by the to-spec cannot be replaced by the element selected by the from-spec");

            Node imported = to.getOwnerDocument().importNode(from, true);
            //to.getOwnerDocument().replaceChild(imported, to);
            to.getParentNode().replaceChild(imported, to);

            // If not keepSrcElementName, then keep destination name
            if (!isKeepSrcElementName)
                imported.getOwnerDocument().renameNode(imported, destination.getNamespaceURI(), destination.getLocalPart());

        } else {
            // Replace content
            if (isKeepSrcElementName)
                throw new SelectionFailure("keepSrcElement may only be used when copying an Element node to an other Element node.");

            // If the source is an EII with an xsi:nil="true", a
            // selectionFailure fault MUST be thrown.
            if (from instanceof Element) {
                String nil = ((Element) from).getAttributeNS(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "nil");
                if (nil.equals("true"))
                    throw new SelectionFailure("Source element must not be xsi:nil='true'.");
            }

            // Once the information item is returned from the source, a TII will
            // be computed based upon it.
            from.normalize();
            String fromString = from.getTextContent();

            // If the destination is an EII, all [children] properties (if any)
            // are removed and the source content TII is added as the child of
            // the EII.
            if (to instanceof Element) {
                NodeList children = to.getChildNodes();
                for (int index = children.getLength() - 1; index <= 0; index++)
                    to.removeChild(children.item(index));

                Text text = to.getOwnerDocument().createTextNode(fromString);
                to.appendChild(text);
            }

            // If the destination is an AII, the value of AII is replaced with
            // the TII from the source. The value MUST be normalized.
            if (to instanceof Attr) {
                to.setNodeValue(fromString);
                to.normalize();
            }

            // If the destination is a TII, the TII in the destination is
            // replaced with the TII from the source.
            if (to instanceof Text) {
                /*
                 * A bpel:mismatchedAssignmentFailure fault MUST be thrown when
                 * the to-spec selects a TII as an lvalue, which does NOT belong
                 * to a WS-BPEL variable of an XSD string type (or a type
                 * derived from XSD string), and fromString is empty.
                 */
                if (fromString.length() == 0) {
                    TypeInfo type = ((Element) to.getParentNode()).getSchemaTypeInfo();
                    if (!type.isDerivedFrom(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string", 0)) {
                        throw new MismatchedAssignmentFailure("The from-spec produced an empty value and the to-spec is NOT a string type.");
                    }
                }

                to.setNodeValue(fromString);
            }

        }

    }

    private void copy(Copy copy) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable, SelectionFailure, MismatchedAssignmentFailure, InvalidVariables, UninitializedPartnerRole {
        From from = copy.getFrom();
        To to = copy.getTo();

        // Special case: message variable to message variable (no part
        // specified) See: Replacement Logic for WSDL Message Variables
        if (isMessageVariableCopy(copy)) {
            // If the from-spec message variable is completely uninitialized
            // then the standard bpel:uninitializedVariable fault is thrown. If
            // the from-spec message variable is partially initialized then any
            // uninitialized parts of the from-spec variable result in the same
            // parts of the to-spec variable becoming uninitialized.
            if (!context.isVariableInitialized(from.getVariable()))
                throw new UninitializedVariable("The from-spec message variable '" + from.getVariable() + "' is completely uninitialized.");

            MessageVariable fromVariable = (MessageVariable) context.getVariable(from.getVariable());
            MessageVariable toVariable = (MessageVariable) context.getVariable(to.getVariable());
            
            ((Transaction) context).writeLockVariable(toVariable.getName());
            toVariable.copyOf(fromVariable);            
            log.info("Assign copy complete message variable '" + from.getVariable() + "' to '" + to.getVariable() + "'.");

            return;
        }

        copy(getFromNode(from), getToNode(to), copy.isIgnoreMissingFromData(), copy.isKeepSrcElementName());

    }

    /**
     * Checks if the from nodes qualified name is a valid substitute for the to
     * nodes qualified name.
     * <p>
     * When the keepSrcElementName attribute is set to "yes" and the destination
     * element is the Document EII of an element-based variable or an
     * element-based part of a WSDL message-type-based variable, a WS-BPEL
     * processor MUST make sure the name of the source element belongs to the
     * substitutionGroup of the destination element used in the element variable
     * declaration or WSDL part definition.
     * 
     * @param to target node
     * @param from source node
     */
    private boolean substitutable(Node to, Node from) {
        try {
            QName toQName = new QName(to.getNamespaceURI(), to.getLocalName());
            QName fromQName = new QName(from.getNamespaceURI(), from.getLocalName());
            SchemaRepository schemas = context.getSchemaRepository();
            return schemas.getElement(toQName).canBeSubstitutedBy(schemas.getElement(fromQName));
        } catch (Exception exception) {
            return false;
        }
    }

    private Node getFromNode(From from) throws SubLanguageExecutionFault, InvalidExpressionValue, UninitializedVariable, UninitializedPartnerRole {

        if (from.getVariable() != null && from.getProperty() == null) {

            if (from.getQuery() != null) {

                if (from.getPart() == null)
                    return from.getQuery().evaluate(from.getVariable(), context);
                else
                    return from.getQuery().evaluate(from.getVariable(), from.getPart(), context);

            } else {

                if (from.getPart() == null)
                    return context.getVariableValue(from.getVariable());
                else
                    return context.getVariableValue(from.getVariable(), from.getPart());

            }
        }

        if (from.getPartnerLink() != null) {
            PartnerLink partnerLink = context.getPartnerLink(from.getPartnerLink());
            if (from.getEndpointReference() == RolesEnumeration.MY_ROLE)
                return partnerLink.getMyEndpoint().getServiceReference();
            else
                return partnerLink.getPartnerEndpoint().getServiceReference();
        }

        if (from.getVariable() != null && from.getProperty() != null) {
            return context.getVariablePropertyValue(from.getVariable(), from.getProperty());
        }

        if (from.getExpession() != null) {
            return from.getExpession().evaluate(context);
        }

        if (from.getLiteral() != null) {
            return (Node) from.getLiteral().getFirstChild();
        }

        return null;
    }

    private Node getToNode(To to) throws SubLanguageExecutionFault, InvalidExpressionValue, SelectionFailure {

        // Initialize variables
        if (to.getVariable() != null) {
            if (to.getPart() == null && !context.isVariableInitialized(to.getVariable()) ) {
                context.initializeVariable(to.getVariable());
            } else if (to.getPart() != null && !context.isVariableInitialized(to.getVariable(), to.getPart())) {
                context.initializeVariable(to.getVariable(), to.getPart());
            }
        }

        try {
            if (to.getVariable() != null && to.getProperty() == null) {

                if (to.getQuery() != null) {

                    if (to.getPart() == null)
                        return to.getQuery().evaluate(to.getVariable(), context);
                    else
                        return to.getQuery().evaluate(to.getVariable(), to.getPart(), context);

                } else {

                    if (to.getPart() == null)
                        return context.getVariableValue(to.getVariable());
                    else
                        return context.getVariableValue(to.getVariable(), to.getPart());

                }
            }

            if (to.getVariable() != null && to.getProperty() != null) {
                return context.getVariablePropertyValue(to.getVariable(), to.getProperty());
            }
            
        } catch (UninitializedVariable exception) {
            /* This should not happen as we just initialized it */
            exception.printStackTrace();
            return null;
        }

        if (to.getPartnerLink() != null) {
            try {
                PartnerLink partnerLink = context.getPartnerLink(to.getPartnerLink());
                if (!partnerLink.getPartnerEndpoint().isInitialized())
                    partnerLink.getPartnerEndpoint().initialize();

                return partnerLink.getPartnerEndpoint().getServiceReference();
            } catch (UninitializedPartnerRole exception) {
                /* This should not happen as we just initialized it */
                exception.printStackTrace();
                return null;
            }
        }

        if (to.getExpession() != null) {
            Node node = to.getExpession().evaluate(context);
            if (node instanceof Text) {
                if (node.getNamespaceURI().equals("http://beepell.com/expression/") && node.getLocalName().equals("rvalue"))
                    throw new SelectionFailure("The to-spec produced an r-value (calculated value).");
            }
            return node;

        }

        return null;
    }
}
