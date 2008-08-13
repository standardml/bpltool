package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Element;

import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tSource complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tSource&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}transitionCondition&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;linkName&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tSource", propOrder = { "transitionConditionElementNode" })
public class Source {

    @XmlAnyElement
    protected Element transitionConditionElementNode;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String linkName;

    /**
     * Get the boolean transition condition expression.
     * 
     * @return the boolean transition condition expression.
     */
    public com.beepell.expression.BooleanExpression getTransitionCondition() {
        if (transitionConditionElementNode == null)
            return null;

        String expression = transitionConditionElementNode.getTextContent();
        NamespaceContext context = new NodeNamespaceContext(transitionConditionElementNode);
        return new com.beepell.expression.BooleanExpression(expression, context);
    }

    /**
     * 
     * @return possible object is {@link Element }
     */
    public Element getTransitionConditionElementNode() {
        return transitionConditionElementNode;
    }

    /**
     * 
     * @param value allowed object is {@link Element }
     */
    public void setTransitionConditionElementNode(Element value) {
        this.transitionConditionElementNode = value;
    }

    /**
     * Gets the value of the linkName property.
     * 
     * @return possible object is {@link String }
     */
    public String getLinkName() {
        return linkName;
    }

    /**
     * Sets the value of the linkName property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setLinkName(String value) {
        this.linkName = value;
    }

}
