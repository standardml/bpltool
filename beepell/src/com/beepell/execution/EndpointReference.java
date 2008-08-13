package com.beepell.execution;

import javax.wsdl.Port;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.beepell.BPELConstants;
import com.beepell.exceptions.UninitializedPartnerRole;
import com.beepell.util.XML;

/**
 * Endpoint reference.
 * 
 * @author Tim Hallwyl
 */
public class EndpointReference {

    private final static QName serviceRef = new QName("http://docs.oasis-open.org/wsbpel/2.0/serviceref", "service-ref", "sref");

    private Document document;

    private boolean initialized = false;
    
    /**
     * Create an endpoint reference.
     * @param partnerLink 
     */
    public EndpointReference(PartnerLink partnerLink) {
        this();
        setPartnerLink(partnerLink);
    }
    
    /**
     * Create an endpoint reference.
     */
    public EndpointReference() {
        
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            this.document = builder.newDocument();
        } catch (ParserConfigurationException exception) {
            /* Should not happen when we use default configuration */
            exception.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Sets the partner link - needed when resolving from a node to partner link.
     * @param partnerLink
     */
    public void setPartnerLink(PartnerLink partnerLink) {
        this.document.setUserData("partnerlink", partnerLink, null);
    }
    
    /**
     * Create an end-point and initialize it with a WSDL port object.
     * @param partnerLink 
     * 
     * @param port
     */
    public EndpointReference(Port port) {
        this();
        this.initialize();

        Element portElement = document.createElementNS(BPELConstants.WSDL, "port");
        portElement.setAttribute("name", port.getName());
        portElement.setAttribute("binding", port.getBinding().getQName().toString());

        Element addressElement = document.createElementNS(BPELConstants.SOAP, "address");
        addressElement.setAttribute("location", ((SOAPAddress) port.getExtensibilityElements().get(0)).getLocationURI());
        portElement.appendChild(addressElement);

        this.document.getDocumentElement().appendChild(portElement);
    }

    /**
     * Initialize the endpoint.
     */
    public void initialize() {
        Element documentElement = this.document.createElementNS(serviceRef.getNamespaceURI(), serviceRef.getLocalPart());
        this.document.appendChild(documentElement);
        this.initialized = true;
    }

    /**
     * Returns true if this endpoint has been initialized.
     * 
     * @return true if this endpoint has been initialized.
     */
    public boolean isInitialized() {
        return this.initialized;
    }

    /**
     * Gets the service reference element.
     * 
     * @return the service reference element.
     * @throws UninitializedPartnerRole
     */
    public Element getServiceReference() throws UninitializedPartnerRole {
        if (!initialized)
            throw new UninitializedPartnerRole("Endpoint is not initialized.");

        return this.document.getDocumentElement();
    }

    /**
     * @return the location
     * @throws UninitializedPartnerRole
     */
    public String getLocation() throws UninitializedPartnerRole {
        Element service = getServiceReference();
        Element address = (Element) service.getElementsByTagNameNS(BPELConstants.SOAP, "address").item(0);
        return address.getAttribute("location");
    }

    /**
     * @return the port
     * @throws UninitializedPartnerRole
     */
    public QName getBinding() throws UninitializedPartnerRole {
        Element service = getServiceReference();
        Element port = (Element) service.getElementsByTagNameNS(BPELConstants.WSDL, "port").item(0);
        return XML.qualify(port, port.getAttribute("binding"));
    }

    /**
     * @return A QName with the port name as local-name.
     * @throws UninitializedPartnerRole
     */
    public QName getPort() throws UninitializedPartnerRole {
        Element service = getServiceReference();
        Element port = (Element) service.getElementsByTagNameNS(BPELConstants.WSDL, "port").item(0);
        return new QName(port.getAttribute("name"));
    }

}
