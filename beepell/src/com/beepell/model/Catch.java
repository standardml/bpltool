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
 * Java class for tCatch complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tCatch&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivityContainer&quot;&gt;
 *       &lt;attribute name=&quot;faultElement&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;faultMessageType&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;faultName&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;faultVariable&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tCatch")
public class Catch extends ActivityContainer {

    @XmlAttribute
    protected QName faultElement;

    @XmlAttribute
    protected QName faultMessageType;

    @XmlAttribute
    protected QName faultName;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String faultVariable;

    /**
     * Gets the value of the faultElement property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getFaultElement() {
        return faultElement;
    }

    /**
     * Sets the value of the faultElement property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setFaultElement(QName value) {
        this.faultElement = value;
    }

    /**
     * Gets the value of the faultMessageType property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getFaultMessageType() {
        return faultMessageType;
    }

    /**
     * Sets the value of the faultMessageType property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setFaultMessageType(QName value) {
        this.faultMessageType = value;
    }

    /**
     * Gets the value of the faultName property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getFaultName() {
        return faultName;
    }

    /**
     * Sets the value of the faultName property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setFaultName(QName value) {
        this.faultName = value;
    }

    /**
     * Gets the value of the faultVariable property.
     * 
     * @return possible object is {@link String }
     */
    public String getFaultVariable() {
        return faultVariable;
    }

    /**
     * Sets the value of the faultVariable property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setFaultVariable(String value) {
        this.faultVariable = value;
    }

}
