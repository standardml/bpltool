package com.beepell.broker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPBodyElement;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;

import org.w3c.dom.DOMException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import com.beepell.exceptions.UninitializedVariable;
import com.beepell.variable.MessageVariable;

/**
 * Utility class to bind SOAP messages with message variables.
 * 
 * @author Tim Hallwyl
 */
public class MessageBinding {

    /**
     * Direction request.
     */
    public static final String REQUEST = "request";

    /**
     * Direction response.
     */
    public static final String RESPONSE = "response";

    /**
     * Creates a SOAP message from a message variable.
     * <p>
     * Note: encoded style is not WS-I compliant, so literal binding is assumed.
     * 
     * @param variable variable to bind as message, may be null if the message
     *            has no parts.
     * @param style must be either document or rpc.
     * @param operation operation name
     * @param direction Either "request" or "response"
     * @return A SOAP (request or reply) message.
     * @throws SOAPException if the message cannot be bound, for example if the
     *             WSDL is not WS-I compliant.
     * @throws DOMException should not happen.
     * @throws UninitializedVariable if the message variable is not initialized
     */
    public static SOAPMessage bind(MessageVariable variable, String style, QName operation, String direction) throws SOAPException, DOMException, UninitializedVariable {
        /*
         * The MessageVariable variable MAY be null, if the message has no
         * parts. In that case there is no MessageVariable matching the WSDL
         * message. We still need a SOAP message, as if there was a message
         * variable without any parts. As 'variable' is only accessed in loops
         * running though parts, it is sufficient to initialize parts to an
         * empty list.
         */
        List<Part> parts;
        if (variable != null)
            parts = variable.getParts();
        else
            parts = new ArrayList<Part>(0);

        MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
        SOAPMessage message = factory.createMessage();

        SOAPPart soap = message.getSOAPPart();
        SOAPEnvelope envelope = soap.getEnvelope();
        // SOAPHeader header = envelope.getHeader();
        SOAPBody body = envelope.getBody();

        /*
         * WS-I requires for rpc/literal style that message parts refer to type
         * declarations, not elements. Each part is wrapped in a part accessor
         * element whose local names is the name of the part and it is not
         * namespace qualified.
         */
        if (style.equals("rpc")) {

            SOAPBodyElement wrapperElement;

            if (operation.getNamespaceURI() != null) {
                if (direction.equals(RESPONSE))
                    wrapperElement = body.addBodyElement(envelope.createName(operation.getLocalPart() + "Response", "rpc", operation.getNamespaceURI()));
                else
                    wrapperElement = body.addBodyElement(envelope.createName(operation.getLocalPart(), "rpc", operation.getNamespaceURI()));

            } else
                throw new SOAPException("WS-I R1014, R2717: A RPC/literal message must declare an operation namespace in soap:body.");

            for (Part part : parts) {
                if (part.getTypeName() != null) {
                    /*
                     * Create an unqualified part accessor element.
                     */
                    Node variableNode = variable.getValue(part.getName());

                    if (variableNode instanceof Text) {
                        // Simple type part variable
                        SOAPElement partAccessorElement = wrapperElement.addChildElement(part.getName());
                        partAccessorElement.setTextContent(variableNode.getTextContent());
                    } else {
                        // Complex type part variable
                        Node variableNodeCopy = wrapperElement.getOwnerDocument().importNode(variableNode, true);
                        variableNodeCopy.getOwnerDocument().renameNode(variableNodeCopy, null, part.getName());
                        wrapperElement.appendChild(variableNodeCopy);
                    }

                } else {
                    throw new SOAPException("WS-I R2203: A RPC/literal message part must refer to a type.");
                }
            }

        } else {

            /*
             * DOCUMENT STYLE WS-I requires for document/literal style that
             * message parts refer elements (not types) and at most one part
             * must be used per message. The element that is referred by the
             * message part is copied as the only child of the SOAP body.
             */

            if (parts.size() > 1)
                throw new SOAPException("WS-I R2210, R2201, R2712, R9981: A document/literal message may contain at most one part.");

            for (Part part : parts) {
                if (part.getElementName() != null) {
                    body.appendChild(body.getOwnerDocument().importNode(variable.getValue(part.getName()), true));
                } else {
                    throw new SOAPException("WS-I R2204: A document/literal message part must refer to an element.");
                }
            }

        }

        message.saveChanges();
        return message;

    }

    /**
     * Binds a SOAP message to a message variable. If either message or variable
     * is null, nothing is done. The binding is a copy operation, thus changing
     * the variable afterwards will not affect the SOAP message.
     * 
     * @param message message to copy values from
     * @param variable message variable to copy values to.
     * @param style either "rpc" or "document"
     * @param operation operation name
     * @param direction Either "request" or "response"
     * @throws SOAPException
     */
    public static void bind(SOAPMessage message, MessageVariable variable, String style, QName operation, String direction) throws SOAPException {
        if (message == null || variable == null)
            return;

        Collection<Part> parts = variable.getParts();
        SOAPBody body = message.getSOAPBody();

        if (body.getFault() != null)
            return;

        /*
         * Document / literal style Lazy implementation: we do not check the
         * name on the body child, we just used it as it is.
         */
        if (style.equals("document")) {
            Iterator<Part> iterator = parts.iterator();

            // If no parts are expected, we're done.
            if (!iterator.hasNext())
                return;

            else {
                // A part is expected. Note, at most one part may be expected.

                // If the variable is not initialized, we do so.
                if (!variable.isInitialized())
                    variable.initialize();

                Element bodyChild = (Element) body.getChildElements().next();
                if (bodyChild == null)
                    throw new SOAPException("An empty message was received, but a part '" + ((Part) iterator.next()).getName() + "' was expected.");

                try {

                    // Get the variable part node
                    Node variablePartNode = variable.getValue(((Part) iterator.next()).getName());

                    // Import a copy the one child of SOAPBody to the variable
                    // part.
                    Node copyOfBodyChild = variablePartNode.getOwnerDocument().importNode(bodyChild, true);

                    // Replace the variable part node with imported copy.
                    variablePartNode.getOwnerDocument().replaceChild(copyOfBodyChild, variablePartNode);

                } catch (UninitializedVariable exception) {
                    // Should not happen, we just initialized it.
                    exception.printStackTrace();
                }
            }

            if (iterator.hasNext())
                throw new SOAPException("WS-I R2210, R2201, R2712, R9981: A document/literal message may contain at most one part.");

        }

        /*
         * RPC/literal binding
         */
        if (style.equals("rpc")) {

            if (operation.getNamespaceURI() == null)
                throw new SOAPException("WS-I R1014, R2717: A RPC/literal message must declare an operation namespace in soap:body.");

            Iterator<Part> iterator = parts.iterator();
            QName wrapperQName;
            if (direction.equals(RESPONSE))
                wrapperQName = new QName(operation.getNamespaceURI(), operation.getLocalPart() + "Response");
            else
                wrapperQName = new QName(operation.getNamespaceURI(), operation.getLocalPart());

            Element wrapperElement = (Element) body.getChildElements(wrapperQName).next();

            if (wrapperElement == null)
                throw new SOAPException("Failed to find the expected '" + wrapperQName + "' in the message.");

            // If the variable is not initialized, we do so.
            if (!variable.isInitialized())
                variable.initialize();

            Part part;
            Element partAccessorElement;
            Node variablePartNode;
            Node copyOfPartNode;
            while (iterator.hasNext()) {
                part = iterator.next();
                partAccessorElement = (Element) body.getElementsByTagName(part.getName()).item(0);

                if (partAccessorElement == null)
                    throw new SOAPException("Failed to find the expected part '" + part.getName() + "' in the message.");

                try {
                    // Get the variable part node
                    variablePartNode = variable.getValue(part.getName());
                    if (variablePartNode instanceof Text)
                        variablePartNode = variablePartNode.getParentNode();

                    // An imported copy of the part
                    copyOfPartNode = variablePartNode.getOwnerDocument().importNode(partAccessorElement, true);

                    // Rename to use qualified name of the variable node
                    copyOfPartNode.getOwnerDocument().renameNode(copyOfPartNode, variablePartNode.getNamespaceURI(), variablePartNode.getLocalName());

                    // Replace variable node with imported copy
                    variablePartNode.getOwnerDocument().replaceChild(copyOfPartNode, variablePartNode);

                } catch (UninitializedVariable exception) {
                    // Should not happen, we just initialized it.
                    exception.printStackTrace();
                }

            }
        }
    }

    /**
     * Bind an incoming SOAP fault to a message variable.
     * <p>
     * A fault message, contained in the details of a SOAPFault, is a
     * document/literal bound message – and as such is may have at most one
     * part, and this part must be referring an element declaration. R2204,
     * R2205, R2206.
     * 
     * @param fault the incoming SOAP fault
     * @param variable the message variable
     * @throws MessagingFault
     */
    public static void bind(SOAPFault fault, MessageVariable variable) throws MessagingFault {

        if (!fault.hasDetail())
            return;

        if (!variable.isInitialized())
            variable.initialize();

        List<Part> parts = variable.getParts();
        if (parts.size() != 1)
            throw new MessagingFault("Fault messages may have exactly one part.");

        if (parts.size() == 0)
            return;

        Iterator iterator = fault.getDetail().getChildElements();
        if (iterator.hasNext()) {
            try {
                // Get the variable part node
                Node variablePartNode = variable.getValue(parts.get(0).getName());

                // Import a copy the one child of SOAPBody to the variable
                // part.
                Node copyOfFaultDetailChildElement = variablePartNode.getOwnerDocument().importNode((Element) fault.getDetail().getChildElements().next(), true);

                // Replace the variable part node with imported copy.
                variablePartNode.getOwnerDocument().replaceChild(copyOfFaultDetailChildElement, variablePartNode);

            } catch (UninitializedVariable exception) {
                // Should not happen, we just initialized it.
                throw new MessagingFault("Unable to initialize variable '" + variable.getName() + "'.");
            }

        }
    }
}
