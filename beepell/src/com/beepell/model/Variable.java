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
 * Java class for tVariable complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tVariable&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}from&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;element&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;messageType&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;name&quot; use=&quot;required&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *       &lt;attribute name=&quot;type&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tVariable", propOrder = { "from" })
public class Variable {

    protected From from;

    @XmlAttribute
    protected QName element;

    @XmlAttribute
    protected QName messageType;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String name;

    @XmlAttribute
    protected QName type;

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
     * Gets the value of the element property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getElement() {
        return element;
    }

    /**
     * Sets the value of the element property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setElement(QName value) {
        this.element = value;
    }

    /**
     * Gets the value of the messageType property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getMessageType() {
        return messageType;
    }

    /**
     * Sets the value of the messageType property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setMessageType(QName value) {
        this.messageType = value;
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
     * Gets the value of the type property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setType(QName value) {
        this.type = value;
    }

}
