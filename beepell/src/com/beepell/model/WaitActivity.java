package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlType;

import org.w3c.dom.Element;

import com.beepell.expression.DeadlineExpression;
import com.beepell.expression.DurationExpression;
import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tWait complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tWait&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;choice&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}for&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}until&quot;/&gt;
 *       &lt;/choice&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tWait", propOrder = { "elementNode" })
public class WaitActivity extends Activity {

    @XmlAnyElement
    protected Element elementNode;



    /**
     * Gets the for-expression.
     * 
     * @return null if none is set, otherwise a for-expression.
     */
    public DurationExpression getForExpression() {
        if ("for".equals(elementNode.getLocalName()))
            return new DurationExpression(elementNode.getTextContent(), new NodeNamespaceContext(elementNode));
        else
            return null;
    }

    /**
     * Gets until-expression.
     * 
     * @return null if none is set, otherwise an until-expression.
     */
    public DeadlineExpression getUntilExpression() {
        if ("until".equals(elementNode.getLocalName()))
            return new DeadlineExpression(elementNode.getTextContent(), new NodeNamespaceContext(elementNode));
        else      
            return null;
    }

    /**
     * 
     * @return possible object is {@link Element }
     */
    public Element getForElementNode() {
        return elementNode;
    }

    /**
     * 
     * @param value allowed object is {@link Element }
     */
    public void setForElementNode(Element value) {
        this.elementNode = value;
    }

    /**
     * 
     * @return possible object is {@link Element }
     */
    public Element getUntilElementNode() {
        return elementNode;
    }

    /**
     * 
     * @param value allowed object is {@link Element }
     */
    public void setUntilElementNode(Element value) {
        this.elementNode = value;
    }

}
