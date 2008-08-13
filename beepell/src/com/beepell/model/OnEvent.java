package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;

/**
 * <p>
 * Java class for tOnEvent complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tOnEvent&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tOnMsgCommon&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}scope&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;element&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;messageType&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tOnEvent", propOrder = { "scope" })
public class OnEvent extends OnMsgCommon {

    @XmlElement(required = true)
    protected ScopeActivity scope;

    @XmlAttribute
    protected QName element;

    @XmlAttribute
    protected QName messageType;

    /**
     * Gets the value of the scope property.
     * 
     * @return possible object is {@link ScopeActivity }
     */
    public ScopeActivity getScope() {
        return scope;
    }

    /**
     * Sets the value of the scope property.
     * 
     * @param value allowed object is {@link ScopeActivity }
     */
    public void setScope(ScopeActivity value) {
        this.scope = value;
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

}
