package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Element;

import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tElseif complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tElseif&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}condition&quot;/&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tElseif", propOrder = { "conditionElementNode", "activity" })
public class Elseif {

    @XmlAnyElement
    protected Element conditionElementNode;

    @XmlElements( { @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class),
            @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class) })
    protected Activity activity;

    /**
     * @return possible object is {@link Element }
     */
    public Element getConditionElementNode() {
        return conditionElementNode;
    }

    /**
     * @param value allowed object is {@link Element }
     */
    public void setConditionElementNode(Element value) {
        this.conditionElementNode = value;
    }

    /**
     * Gets the boolean expression.
     * 
     * @return the boolean expression.
     */
    public com.beepell.expression.BooleanExpression getConditionExpression() {
        String expression = this.getConditionElementNode().getTextContent();
        NamespaceContext context = new NodeNamespaceContext(this.getConditionElementNode());
        return new com.beepell.expression.BooleanExpression(expression, context);
    }

    /**
     * Gets the value of the activity property.
     * 
     * @return possible object is {@link IfActivity } {@link RethrowActivity }
     *         {@link EmptyActivity } {@link ExitActivity }
     *         {@link WaitActivity } {@link CompensateScopeActivity }
     *         {@link ForEachActivity } {@link ScopeActivity }
     *         {@link FlowActivity } {@link RepeatUntilActivity }
     *         {@link WhileActivity } {@link SequenceActivity }
     *         {@link ReplyActivity } {@link ValidateActivity }
     *         {@link CompensateActivity } {@link InvokeActivity }
     *         {@link ThrowActivity } {@link PickActivity }
     *         {@link AssignActivity } {@link ReceiveActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link IfActivity }
     *            {@link RethrowActivity } {@link EmptyActivity }
     *            {@link ExitActivity } {@link WaitActivity }
     *            {@link CompensateScopeActivity } {@link ForEachActivity }
     *            {@link ScopeActivity } {@link FlowActivity }
     *            {@link RepeatUntilActivity } {@link WhileActivity }
     *            {@link SequenceActivity } {@link ReplyActivity }
     *            {@link ValidateActivity } {@link CompensateActivity }
     *            {@link InvokeActivity } {@link ThrowActivity }
     *            {@link PickActivity } {@link AssignActivity }
     *            {@link ReceiveActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

}
