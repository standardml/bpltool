package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;

import org.w3c.dom.Element;

import com.beepell.expression.DeadlineExpression;
import com.beepell.expression.DurationExpression;
import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tOnAlarmPick complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tOnAlarmPick&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}forOrUntilGroup&quot;/&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tOnAlarmPick", propOrder = { "elementNode", "activity" })
public class OnAlarmPick {

    @XmlAnyElement
    protected Element elementNode;

    @XmlElements( { @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class),
            @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class) })
    protected Activity activity;
    
    
    /**
     * Get the Duration Expression
     * @return the Duration Expression
     */
    public DurationExpression getDurationExpression() {
        if ("for".equals(elementNode.getLocalName()))
            return new DurationExpression(elementNode.getTextContent(), new NodeNamespaceContext(elementNode));
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
     * Gets the deadline expression
     * @return the deadline expression
     */
    public DeadlineExpression getDeadLineExpression() {
        if ("until".equals(elementNode.getLocalName()))
            return new DeadlineExpression(elementNode.getTextContent(), new NodeNamespaceContext(elementNode));
        else      
            return null;
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

    /**
     * Gets the value of the activity property.
     * 
     * @return possible object is {@link RethrowActivity } {@link ExitActivity }
     *         {@link InvokeActivity } {@link AssignActivity }
     *         {@link CompensateActivity } {@link IfActivity }
     *         {@link ThrowActivity } {@link EmptyActivity }
     *         {@link ForEachActivity } {@link ReplyActivity }
     *         {@link WaitActivity } {@link PickActivity }
     *         {@link SequenceActivity } {@link FlowActivity }
     *         {@link ValidateActivity } {@link RepeatUntilActivity }
     *         {@link WhileActivity } {@link CompensateScopeActivity }
     *         {@link ScopeActivity } {@link ReceiveActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link RethrowActivity }
     *            {@link ExitActivity } {@link InvokeActivity }
     *            {@link AssignActivity } {@link CompensateActivity }
     *            {@link IfActivity } {@link ThrowActivity }
     *            {@link EmptyActivity } {@link ForEachActivity }
     *            {@link ReplyActivity } {@link WaitActivity }
     *            {@link PickActivity } {@link SequenceActivity }
     *            {@link FlowActivity } {@link ValidateActivity }
     *            {@link RepeatUntilActivity } {@link WhileActivity }
     *            {@link CompensateScopeActivity } {@link ScopeActivity }
     *            {@link ReceiveActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

}
