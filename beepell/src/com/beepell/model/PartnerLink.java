package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.namespace.QName;

/**
 * <p>
 * Java class for tPartnerLink complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tPartnerLink&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;attribute name=&quot;initializePartnerRole&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; /&gt;
 *       &lt;attribute name=&quot;myRole&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;name&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;partnerLinkType&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;partnerRole&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tPartnerLink")
public class PartnerLink {

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean initializePartnerRole;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String myRole;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String name;

    @XmlAttribute(required = true)
    protected QName partnerLinkType;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String partnerRole;

    /**
     * Gets the value of the initializePartnerRole property.
     * 
     * @return possible object is {@link String }
     */
    public Boolean isInitializePartnerRole() {
        return initializePartnerRole;
    }

    /**
     * Sets the value of the initializePartnerRole property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setInitializePartnerRole(Boolean value) {
        this.initializePartnerRole = value;
    }

    /**
     * Gets the value of the myRole property.
     * 
     * @return possible object is {@link String }
     */
    public String getMyRole() {
        return myRole;
    }

    /**
     * Sets the value of the myRole property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setMyRole(String value) {
        this.myRole = value;
    }

    /**
     * Gets the value of the name property.
     * 
     * @return possible object is {@link String }
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the partnerLinkType property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getPartnerLinkType() {
        return partnerLinkType;
    }

    /**
     * Sets the value of the partnerLinkType property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setPartnerLinkType(QName value) {
        this.partnerLinkType = value;
    }

    /**
     * Gets the value of the partnerRole property.
     * 
     * @return possible object is {@link String }
     */
    public String getPartnerRole() {
        return partnerRole;
    }

    /**
     * Sets the value of the partnerRole property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setPartnerRole(String value) {
        this.partnerRole = value;
    }

}
