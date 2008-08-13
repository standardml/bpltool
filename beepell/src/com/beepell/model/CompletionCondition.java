package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Element;

import com.beepell.expression.UnsignedIntegerExpression;
import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tCompletionCondition complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tCompletionCondition&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}branches&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tCompletionCondition", propOrder = { "branchesElementNode" })
public class CompletionCondition {

    @XmlAnyElement
    protected Element branchesElementNode;

    /**
     * Gets the completion condition expression.
     * 
     * @return the completion condition expression.
     */
    public UnsignedIntegerExpression getConditionExpression() {

        String expression = branchesElementNode.getTextContent();
        NamespaceContext context = new NodeNamespaceContext(branchesElementNode);
        return new UnsignedIntegerExpression(expression, context);
    }

    /**
     * @return possible object is {@link Element }
     */
    public Element getBranchesElementNode() {
        return branchesElementNode;
    }

    /**
     * @param value allowed object is {@link Element }
     */
    public void setBranchesElementNode(Element value) {
        this.branchesElementNode = value;
    }

}
