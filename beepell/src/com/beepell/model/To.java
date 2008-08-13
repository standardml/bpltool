package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.beepell.expression.CopyQuery;
import com.beepell.expression.GeneralExpression;
import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tTo complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tTo&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;choice minOccurs=&quot;0&quot;&gt;
 *           &lt;element name=&quot;expression&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tExpression&quot; minOccurs=&quot;0&quot;/&gt;
 *           &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}query&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;/choice&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;part&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;partnerLink&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;property&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}QName&quot; /&gt;
 *       &lt;attribute name=&quot;variable&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tTo", propOrder = { "any" })
public class To {
    
    @XmlAnyElement
    protected Element any;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String part;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String partnerLink;

    @XmlAttribute
    protected QName property;

    @XmlAttribute
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String variable;

    /**
     * @return possible object is {@link Element }
     */
    public Element getExpressionElementNode() {
        if (any != null && any.getLocalName().equals("expression"))
            return any;
        else
            return null;
    }

    /**
     * Get the expression.
     * 
     * @return the expression
     */
    public GeneralExpression getExpession() {
        if (any != null && any.getLocalName().equals("expression")) {
            String expression = any.getTextContent();
            NamespaceContext context = new NodeNamespaceContext(any);
            return new GeneralExpression(expression, context);
        } else
            return null;
    }

    /**
     * @param value allowed object is {@link Element }
     */
    public void setExpressionElementNode(Element value) {
        this.any = value;
    }

    /**
     * Get the query.
     * 
     * @return the query.
     */
    public CopyQuery getQuery() {
        if (any != null && any.getLocalName().equals("query")) {
            String expression = any.getTextContent();
            NamespaceContext context = new NodeNamespaceContext(any);
            return new CopyQuery(expression, context);
        } else
            return null;
    }

    /**
     * @return possible object is {@link Element }
     */
    public Element getQueryElementNode() {
        if (any != null && any.getLocalName().equals("query"))
            return any;
        else
            return null;
    }

    /**
     * @param value allowed object is {@link Element }
     */
    public void setQueryElementNode(Element value) {
        this.any = value;
    }

    /**
     * Gets the value of the part property.
     * 
     * @return possible object is {@link String }
     */
    public String getPart() {
        return part;
    }

    /**
     * Sets the value of the part property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setPart(String value) {
        this.part = value;
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
     * Gets the value of the property property.
     * 
     * @return possible object is {@link QName }
     */
    public QName getProperty() {
        return property;
    }

    /**
     * Sets the value of the property property.
     * 
     * @param value allowed object is {@link QName }
     */
    public void setProperty(QName value) {
        this.property = value;
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
