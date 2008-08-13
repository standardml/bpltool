package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Element;

import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tTargets complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tTargets&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}joinCondition&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}target&quot; maxOccurs=&quot;unbounded&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tTargets", propOrder = { "joinConditionElementNode", "target" })
public class Targets {

    @XmlAnyElement
    protected Element joinConditionElementNode;

    @XmlElement(required = true)
    protected List<Target> target;

    /**
     * Gets the boolean join condition expression.
     * 
     * @return the boolean join condition expression.
     */
    public com.beepell.expression.JoinConditionExpression getJoinCondition() {
        if (joinConditionElementNode == null)
            return null;

        String expression = joinConditionElementNode.getTextContent();
        NamespaceContext context = new NodeNamespaceContext(joinConditionElementNode);
        return new com.beepell.expression.JoinConditionExpression(expression, context);
    }

    /**
     * 
     * @return possible object is {@link Element }
     */
    public Element getJoinConditionElementNode() {
        return joinConditionElementNode;
    }

    /**
     * 
     * @param value allowed object is {@link Element }
     */
    public void setJoinConditionElementNode(Element value) {
        this.joinConditionElementNode = value;
    }

    /**
     * Gets the value of the target property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the target property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getTarget().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Target }
     * @return Target
     */
    public List<Target> getTarget() {
        if (target == null) {
            target = new ArrayList<Target>();
        }
        return this.target;
    }

}
