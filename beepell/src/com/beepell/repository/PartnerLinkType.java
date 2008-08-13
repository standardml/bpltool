package com.beepell.repository;

import javax.wsdl.PortType;
import javax.xml.namespace.QName;

/**
 * <p>
 *  [6.1] &quot;A &lt;partnerLinkType&gt; characterizes the conversational relationship
 *  between two services by defining the roles played by each of the services in
 *  the conversation and specifying the portType provided by each service to
 *  receive messages within the context of the conversation. Each &lt;role&gt;
 *  specifies exactly one WSDL portType. &quot;
 * </p>
 * 
 * @author Tim Hallwyl, 2006
 */
public class PartnerLinkType {

    private final QName name;
    private final Role[] roles;
    
    /**
     * Creates a partner link type with only one role.
     * @param name qualified name
     * @param role
     */
    public PartnerLinkType(final QName name, final Role role) {
        this.name = name;
        this.roles = new Role[] { role };
    }

    /**
     * Creates a partner link type with two roles.
     * @param name qualified name
     * @param firstRole
     * @param secondRole
     */
    public PartnerLinkType(final QName name, final Role firstRole, final Role secondRole) {
        this.name = name;
        this.roles = new Role[] { firstRole, secondRole };
    }
    
    /**
     * Gets a Role by name.
     * @param name
     * @return Named Role
     */
    public Role getRole(final String name) {
        for (int i = 0; i < roles.length; i++) {
            if (roles[i] != null && roles[i].getName().equals(name))
              return roles[i];
        }
        return null;        
    }
    
    /**
     * Gets the PortType by Role Name.
     * Equvilant to getRole(roleName).getPortType()
     * @param roleName
     * @return the WSDL port type of the named role.
     */
    public PortType getPortType(final String roleName) {
        Role role = getRole(roleName);
        if (role != null) return role.getPortType();
        return null;
    }
    
    /**
     * Gets the qualified name of this PartnerLinkType.
     * @return the qualified name of this PartnerLinkType.
     */
    public QName getName() {
        return name;
    }
    
}
