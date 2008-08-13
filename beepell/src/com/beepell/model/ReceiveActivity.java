package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.namespace.QName;

/**
 * XSD Authors: The child element correlations needs to be a Local Element
 * Declaration, because there is another correlations element defined for the
 * invoke activity.
 * <p>
 * Java class for tReceive complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tReceive&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element name=&quot;correlations&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tCorrelations&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;createInstance&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *       &lt;attribute name=&quot;messageExchange&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;operation&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;partnerLink&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;portType&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;variable&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tReceive", propOrder = { "correlations" })
public class ReceiveActivity extends Activity {

    protected Correlations correlations;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean createInstance;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String messageExchange;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String operation;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String partnerLink;

    @XmlAttribute
    protected QName portType;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String variable;

    /**
     * Gets the value of the correlations property.
     * 
     * @return possible object is {@link Correlations }
     */
    public Correlations getCorrelations() {
        return correlations;
    }

    /**
     * Sets the value of the correlations property.
     * 
     * @param value allowed object is {@link Correlations }
     */
    public void setCorrelations(Correlations value) {
        this.correlations = value;
    }

    /**
     * Gets the value of the createInstance property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isCreateInstance() {
        if (createInstance == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return createInstance;
        }
    }

    /**
     * Sets the value of the createInstance property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setCreateInstance(Boolean value) {
        this.createInstance = value;
    }

    /**
     * Gets the value of the messageExchange property.
     * 
     * @return possible object is {@link String }
     */
    public String getMessageExchange() {
        return messageExchange;
    }

    /**
     * Sets the value of the messageExchange property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setMessageExchange(String value) {
        this.messageExchange = value;
    }

    /**
     * Gets the value of the operation property.
     * 
     * @return possible object is {@link String }
     */
    public String getOperation() {
        return operation;
    }

    /**
     * Sets the value of the operation property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setOperation(String value) {
        this.operation = value;
    }

    /**
     * Gets the value of the partnerLink property.
     * 
     * @return possible object is {@link String }
     */
    public String getPartnerLink() {
        return partnerLink;
    }

    /**
     * Sets the value of the partnerLink property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setPartnerLink(String value) {
        this.partnerLink = value;
    }

    /**
     * Gets the value of the portType property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getPortType() {
        return portType;
    }

    /**
     * Sets the value of the portType property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setPortType(QName value) {
        this.portType = value;
    }

    /**
     * Gets the value of the variable property.
     * 
     * @return possible object is {@link String }
     */
    public String getVariable() {
        return variable;
    }

    /**
     * Sets the value of the variable property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setVariable(String value) {
        this.variable = value;
    }

}
