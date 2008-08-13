package com.beepell.execution;

import javax.xml.namespace.QName;

/**
 * partner link class.
 * 
 * @author Tim Hallwyl
 */
public class PartnerLink {

    private final String name;

    private final String myRole;

    private final String partnerRole;

    private final QName partnerLinkType;

    private final boolean initializePartnerRole;

    private final EndpointReference myEndpoint;

    private final EndpointReference partnerEndpoint;

    /**
     * Create a runtime partner link.
     * 
     * @param configuration
     * @param myEndpoint Must be set if myRole is defined
     * @param partnerEndpoint Must be set if partnerRole is defined
     */
    public PartnerLink(com.beepell.model.PartnerLink configuration, EndpointReference myEndpoint, EndpointReference partnerEndpoint) {
        this.name = configuration.getName();
        this.myRole = configuration.getMyRole();
        this.partnerRole = configuration.getPartnerRole();
        this.partnerLinkType = configuration.getPartnerLinkType();
        this.initializePartnerRole = configuration.isInitializePartnerRole() == null ? false : true;

        // Check partner link endpoint
        if (this.partnerRole != null) {

            if (partnerEndpoint == null)
                throw new IllegalArgumentException("This partner link requires a partner role end-point.");

            if (!initializePartnerRole && partnerEndpoint.isInitialized())
                throw new IllegalArgumentException("This partner link must not initialize its partner role end-point.");

            this.partnerEndpoint = partnerEndpoint;
            this.partnerEndpoint.setPartnerLink(this);

        } else {
            this.partnerEndpoint = null;
        }

        if (this.myRole != null) {

            if (myEndpoint == null)
                throw new IllegalArgumentException("This partner link requires a my role end-point.");
            
            if (!myEndpoint.isInitialized())
                throw new IllegalArgumentException("My end-point must be initialized.");

            this.myEndpoint = myEndpoint;
            this.myEndpoint.setPartnerLink(this);

        } else {
            this.myEndpoint = null;
        }

    }

    /**
     * Gets my role.
     * 
     * @return my role.
     */
    public String getMyRole() {
        return myRole;
    }

    /**
     * Gets the partner link name.
     * 
     * @return partner link name.
     */
    public String getName() {
        return name;
    }

    /**
     * Gets the partner link role.
     * 
     * @return the partner link role.
     */
    public String getPartnerRole() {
        return partnerRole;
    }

    /**
     * Gets the partner link type.
     * 
     * @return the partner link type.
     */
    public QName getPartnerLinkType() {
        return partnerLinkType;
    }

    /**
     * @return the myEndpoint
     */
    public EndpointReference getMyEndpoint() {
        return myEndpoint;
    }

    /**
     * @return the partnerEndpoint
     */
    public EndpointReference getPartnerEndpoint() {
        return partnerEndpoint;
    }

}
