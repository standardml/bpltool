package com.beepell.model;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Element;

import com.beepell.expression.UnsignedIntegerExpression;
import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tForEach complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tForEach&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}startCounterValue&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}finalCounterValue&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}completionCondition&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}scope&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;counterName&quot; use=&quot;required&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}BPELVariableName&quot; /&gt;
 *       &lt;attribute name=&quot;parallel&quot; use=&quot;required&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tForEach", propOrder = {"nodes", "completionCondition", "scope" })
public class ForEachActivity extends Activity {

    @XmlAnyElement
    protected List<Element> nodes;

    protected CompletionCondition completionCondition;

    @XmlElement(required = true)
    protected ScopeActivity scope;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String counterName;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean parallel;

    /**
     * Get the Start Counter Expression.
     * 
     * @return the Start Counter Expression.
     */
    public UnsignedIntegerExpression getStartCounterExpression() {
        for (Element element : nodes) {
            if (element.getLocalName().equals("startCounterValue")) {
                String expression = element.getTextContent();
                NamespaceContext context = new NodeNamespaceContext(element);
                return new UnsignedIntegerExpression(expression, context);
            }
        }
        throw new IllegalStateException("No startCounterValue found in this forEach.");
    }

    /**
     * @return possible object is {@link Element }
     */
    public Element getStartCounterValueElementNode() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param value allowed object is {@link Element }
     */
    public void setStartCounterValueElementNode(Element value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Get the Final Counter Expression.
     * 
     * @return the Final Counter Expression.
     */
    public UnsignedIntegerExpression getFinalCounterExpression() {
        for (Element element : nodes) {
            if (element.getLocalName().equals("finalCounterValue")) {
                String expression = element.getTextContent();
                NamespaceContext context = new NodeNamespaceContext(element);
                return new UnsignedIntegerExpression(expression, context);
            }
        }
        throw new IllegalStateException("No finalCounterValue found in this forEach.");
    }

    /**
     * @return possible object is {@link Element }
     */
    public Element getFinalCounterValueElementNode() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param value allowed object is {@link Element }
     */
    public void setFinalCounterValueElementNode(Element value) {
        throw new UnsupportedOperationException();
    }

    /**
     * Gets the value of the completionCondition property.
     * 
     * @return possible object is {@link CompletionCondition }
     */
    public CompletionCondition getCompletionCondition() {
        return completionCondition;
    }

    /**
     * Sets the value of the completionCondition property.
     * 
     * @param value allowed object is {@link CompletionCondition }
     */
    public void setCompletionCondition(CompletionCondition value) {
        this.completionCondition = value;
    }

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
     * Gets the value of the counterName property.
     * 
     * @return possible object is {@link String }
     */
    public String getCounterName() {
        return counterName;
    }

    /**
     * Sets the value of the counterName property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setCounterName(String value) {
        this.counterName = value;
    }

    /**
     * Gets the value of the parallel property.
     * 
     * @return possible object is {@link String }
     */
    public Boolean isParallel() {
        return parallel;
    }

    /**
     * Sets the value of the parallel property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setParallel(Boolean value) {
        this.parallel = value;
    }

}
