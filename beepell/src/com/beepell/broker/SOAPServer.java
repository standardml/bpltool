package com.beepell.broker;

import java.util.Hashtable;
import java.util.List;

import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import javax.xml.ws.Endpoint;

import com.beepell.deployment.ProcessContext;
import com.beepell.model.OnEvent;
import com.beepell.model.OnMessage;
import com.beepell.model.PartnerLink;
import com.beepell.model.ReceiveActivity;
import com.beepell.repository.PartnerLinkType;
import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 */
public class SOAPServer {

    private static Hashtable<String, Endpoint> endpoints = new Hashtable<String, Endpoint>();

    private static Hashtable<QName, InboundMessageHandler> handlers = new Hashtable<QName, InboundMessageHandler>();

    /**
     * Gets the inbound message handler for the port type.
     * 
     * @param portType
     * @return the inbound message handler
     */
    public static InboundMessageHandler getHandler(QName portType) {
        return handlers.get(portType);
    }

    /**
     * Publish a process. This publish all inbound message activities.
     * 
     * @param context
     * @throws Exception
     */
    public static void publish(final ProcessContext context) throws Exception {
        ReceiveActivity receive;
        OnMessage onMessage;
        OnEvent onEvent;

        /**
         * List of published end-points indexed by PartnerLinkType.
         */

        PartnerLink partnerLink = null;
        InboundMessageHandler provider = null;
        PartnerLinkType partnerLinkType;
        PortType portType;
        List<Object> inboundMessageActivities = context.getInboundMessageActivities();

        for (Object object : inboundMessageActivities) {

            if (object instanceof ReceiveActivity) {
                receive = (ReceiveActivity) object;
                partnerLink = context.getPartnerLink(receive);
            }

            if (object instanceof OnMessage) {
                onMessage = (OnMessage) object;
                partnerLink = context.getPartnerLink(onMessage);
            }

            if (object instanceof OnEvent) {
                onEvent = (OnEvent) object;
                partnerLink = context.getPartnerLink(onEvent);
            }

            partnerLinkType = context.getServiceRepository().getPartnerLinkType(partnerLink.getPartnerLinkType());
            if (partnerLinkType == null)
                throw new IllegalStateException("PartnerLinkType definition not found for partner link '" + partnerLink.getName() + "'");

            portType = partnerLinkType.getPortType(partnerLink.getMyRole());
            if (portType == null)
                throw new IllegalStateException("PortType definition not found for partner link '" + partnerLink.getName() + "', role '" + partnerLink.getMyRole() + "'.");

            if (!endpoints.containsKey(getKey(partnerLinkType.getName(), partnerLink.getMyRole()))) {
                // TODO: This will create two handlers for same port type if it
                // has more than one address, they COULD use the same.
                // TODO: Bug: Tow processes using same partner link type
                // definition, but with different endpoint should be possible,
                // but is not because we save endpoints by their partner link
                // type and role.
                provider = new InboundMessageHandler(context, partnerLinkType, partnerLink.getMyRole());
                publish(partnerLink, provider, context.getServiceRepository());
                handlers.put(portType.getQName(), provider);
            }
        }
    }

    private static void publish(PartnerLink partnerLink, Object provider, ServiceRepository services) throws Exception {
        // TODO: Bug: a Partner Link Type may have two end-point
        String myRole = partnerLink.getMyRole();
        QName partnerLinkTypeName = partnerLink.getPartnerLinkType();

        String address = services.getMyAddress(partnerLinkTypeName, myRole);
        Endpoint endpoint = Endpoint.create(provider);
        endpoint.publish(address);

        System.out.println("INFO: Published '" + partnerLinkTypeName.getLocalPart() + "' (" + partnerLink.getMyRole() + ") at " + address);
        endpoints.put(getKey(partnerLinkTypeName, partnerLink.getMyRole()), endpoint);
    }

    private static void unpublish(PartnerLink partnerLink, ServiceRepository services) {
        String myRole = partnerLink.getMyRole();
        QName partnerLinkTypeName = partnerLink.getPartnerLinkType();
        QName portType = services.getPartnerLinkType(partnerLinkTypeName).getPortType(myRole).getQName();
        Endpoint endpoint = endpoints.remove(getKey(partnerLinkTypeName, myRole));
        handlers.remove(portType);
        if (endpoint != null)
            endpoint.stop();
    }

    /**
     * Unpublish a process.
     * 
     * @param context
     */
    public static void unpublish(final ProcessContext context) {
        ReceiveActivity receive;
        OnMessage onMessage;
        OnEvent onEvent;
        PartnerLink partnerLink = null;

        List<Object> inboundMessageActivities = context.getInboundMessageActivities();
        for (Object object : inboundMessageActivities) {

            if (object instanceof ReceiveActivity) {
                receive = (ReceiveActivity) object;
                partnerLink = context.getPartnerLink(receive);
            }

            if (object instanceof OnMessage) {
                onMessage = (OnMessage) object;
                partnerLink = context.getPartnerLink(onMessage);
            }

            if (object instanceof OnEvent) {
                onEvent = (OnEvent) object;
                partnerLink = context.getPartnerLink(onEvent);
            }
            unpublish(partnerLink, context.getServiceRepository());

        }
    }

    private static String getKey(QName partnerLinkType, String role) {
        return partnerLinkType.getNamespaceURI() + partnerLinkType.getLocalPart() + "#" + role;
    }

}
