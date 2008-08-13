package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * <p>
 * Java class for tCopy complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tCopy&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}from&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}to&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;ignoreMissingFromData&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *       &lt;attribute name=&quot;keepSrcElementName&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tCopy", propOrder = { "from", "to" })
public class Copy {

    @XmlElement(required = true)
    protected From from;

    @XmlElement(required = true)
    protected To to;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean ignoreMissingFromData;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean keepSrcElementName;

    /**
     * Gets the value of the from property.
     * 
     * @return possible object is {@link From }
     */
    public From getFrom() {
        return from;
    }

    /**
     * Sets the value of the from property.
     * 
     * @param value allowed object is {@link From }
     */
    public void setFrom(From value) {
        this.from = value;
    }

    /**
     * Gets the value of the to property.
     * 
     * @return possible object is {@link To }
     */
    public To getTo() {
        return to;
    }

    /**
     * Sets the value of the to property.
     * 
     * @param value allowed object is {@link To }
     */
    public void setTo(To value) {
        this.to = value;
    }

    /**
     * Gets the value of the ignoreMissingFromData property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isIgnoreMissingFromData() {
        if (ignoreMissingFromData == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return ignoreMissingFromData;
        }
    }

    /**
     * Sets the value of the ignoreMissingFromData property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setIgnoreMissingFromData(Boolean value) {
        this.ignoreMissingFromData = value;
    }

    /**
     * Gets the value of the keepSrcElementName property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isKeepSrcElementName() {
        if (keepSrcElementName == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return keepSrcElementName;
        }
    }

    /**
     * Sets the value of the keepSrcElementName property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setKeepSrcElementName(Boolean value) {
        this.keepSrcElementName = value;
    }

}
