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
 * non-invoke activities.
 * <p>
 * Java class for tInvoke complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tInvoke&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element name=&quot;correlations&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tCorrelationsWithPattern&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;inputVariable&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *       &lt;attribute name=&quot;operation&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;outputVariable&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *       &lt;attribute name=&quot;partnerLink&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;portType&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tInvoke", propOrder = { "correlations" })
public class InvokeActivity extends Activity {

    protected CorrelationsWithPattern correlations;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String inputVariable;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String operation;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String outputVariable;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String partnerLink;

    @XmlAttribute
    protected QName portType;

    /**
     * Gets the value of the correlations property.
     * 
     * @return possible object is {@link CorrelationsWithPattern }
     */
    public CorrelationsWithPattern getCorrelations() {
        return correlations;
    }

    /**
     * Sets the value of the correlations property.
     * 
     * @param value allowed object is {@link CorrelationsWithPattern }
     */
    public void setCorrelations(CorrelationsWithPattern value) {
        this.correlations = value;
    }

    /**
     * Gets the value of the inputVariable property.
     * 
     * @return possible object is {@link String }
     */
    public String getInputVariable() {
        return inputVariable;
    }

    /**
     * Sets the value of the inputVariable property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setInputVariable(String value) {
        this.inputVariable = value;
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
     * Gets the value of the outputVariable property.
     * 
     * @return possible object is {@link String }
     */
    public String getOutputVariable() {
        return outputVariable;
    }

    /**
     * Sets the value of the outputVariable property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setOutputVariable(String value) {
        this.outputVariable = value;
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

}
