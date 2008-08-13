package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import org.w3c.dom.Element;

import com.beepell.xml.namespace.NodeNamespaceContext;

import com.beepell.expression.DurationExpression;
import com.beepell.expression.DeadlineExpression;

/**
 * <p>
 * Java class for tOnAlarmEvent complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tOnAlarmEvent&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;choice&gt;
 *           &lt;sequence&gt;
 *             &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}forOrUntilGroup&quot;/&gt;
 *             &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}repeatEvery&quot; minOccurs=&quot;0&quot;/&gt;
 *           &lt;/sequence&gt;
 *           &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}repeatEvery&quot;/&gt;
 *         &lt;/choice&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}scope&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tOnAlarmEvent", propOrder = { "elementNodes", "scope" })
public class OnAlarmEvent {

    @XmlAnyElement
    protected Element[] elementNodes; 

    @XmlElement(required = true)
    protected ScopeActivity scope;

    /**
     * Gets the for-expression.
     * 
     * @return null if none is set, otherwise a for-expression.
     */
    public DurationExpression getForExpression() {
        Element forElement = null;
        for (Element element : elementNodes) {
            if ("for".equals(element.getLocalName()))
                forElement = element;
        }
        
        if (forElement != null)            
            return new DurationExpression(forElement.getTextContent(), new NodeNamespaceContext(forElement));
        else
            return null;
    }
    
    /**
     * Gets the until-expression.
     * 
     * @return null if none is set, otherwise a until-expression.
     */
    public DeadlineExpression getUntilExpression() {
        Element untilElement = null;
        for (Element element : elementNodes) {
            if ("until".equals(element.getLocalName()))
                untilElement = element;
        }
        
        if (untilElement != null)            
            return new DeadlineExpression(untilElement.getTextContent(), new NodeNamespaceContext(untilElement));
        else
            return null;
    }
    
    /**
     * Gets the repeatEvery-expression.
     * 
     * @return null if none is set, otherwise a repeatEvery-expression.
     */
    public DurationExpression getRepeatEveryExpression() {
        Element repeatElement = null;
        for (Element element : elementNodes) {
            if ("repeatEvery".equals(element.getLocalName()))
                repeatElement = element;
        }
        
        if (repeatElement != null)            
            return new DurationExpression(repeatElement.getTextContent(), new NodeNamespaceContext(repeatElement));
        else
            return null;
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

}
