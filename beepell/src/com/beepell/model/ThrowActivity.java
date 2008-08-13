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
 * Java class for tThrow complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tThrow&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;attribute name=&quot;faultName&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;faultVariable&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tThrow")
public class ThrowActivity extends Activity {

    @XmlAttribute(required = true)
    protected QName faultName;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String faultVariable;

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
