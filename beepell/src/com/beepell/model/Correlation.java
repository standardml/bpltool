package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * <p>
 * Java class for tCorrelation complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tCorrelation&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;attribute name=&quot;initiate&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tInitiate&quot; default=&quot;no&quot; /&gt;
 *       &lt;attribute name=&quot;set&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tCorrelation")
public class Correlation {

    @XmlAttribute
    protected InitiateEnumeration initiate;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String set;

    /**
     * Gets the value of the initiate property.
     * 
     * @return possible object is {@link InitiateEnumeration }
     */
    public InitiateEnumeration getInitiate() {
        if (initiate == null) {
            return InitiateEnumeration.NO;
        } else {
            return initiate;
        }
    }

    /**
     * Sets the value of the initiate property.
     * 
     * @param value allowed object is {@link InitiateEnumeration }
     */
    public void setInitiate(InitiateEnumeration value) {
        this.initiate = value;
    }

    /**
     * Gets the value of the set property.
     * 
     * @return possible object is {@link String }
     */
    public String getSet() {
        return set;
    }

    /**
     * Sets the value of the set property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setSet(String value) {
        this.set = value;
    }

}
