package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.NamespaceContext;

import org.w3c.dom.Element;

import com.beepell.expression.BooleanExpression;
import com.beepell.xml.namespace.NodeNamespaceContext;

/**
 * <p>
 * Java class for tRepeatUntil complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tRepeatUntil&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}condition&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tRepeatUntil", propOrder = { "activity", "conditionElementNode" })
public class RepeatUntilActivity extends Activity {

    @XmlElements( { @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "exit", type = ExitActivity.class),
            @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class), @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class) })
    protected Activity activity;

    @XmlAnyElement
    protected Element conditionElementNode;

    /**
     * Gets the value of the activity property.
     * 
     * @return possible object is {@link RepeatUntilActivity }
     *         {@link ValidateActivity } {@link ThrowActivity }
     *         {@link FlowActivity } {@link ReceiveActivity }
     *         {@link PickActivity } {@link WhileActivity }
     *         {@link ScopeActivity } {@link CompensateActivity }
     *         {@link ExitActivity } {@link SequenceActivity }
     *         {@link ReplyActivity } {@link IfActivity } {@link WaitActivity }
     *         {@link CompensateScopeActivity } {@link RethrowActivity }
     *         {@link EmptyActivity } {@link ForEachActivity }
     *         {@link AssignActivity } {@link InvokeActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link RepeatUntilActivity }
     *            {@link ValidateActivity } {@link ThrowActivity }
     *            {@link FlowActivity } {@link ReceiveActivity }
     *            {@link PickActivity } {@link WhileActivity }
     *            {@link ScopeActivity } {@link CompensateActivity }
     *            {@link ExitActivity } {@link SequenceActivity }
     *            {@link ReplyActivity } {@link IfActivity }
     *            {@link WaitActivity } {@link CompensateScopeActivity }
     *            {@link RethrowActivity } {@link EmptyActivity }
     *            {@link ForEachActivity } {@link AssignActivity }
     *            {@link InvokeActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

    /**
     * Gets the condition expression.
     * 
     * @return the condition expression.
     */
    public BooleanExpression getConditionExpression() {
        String expression = conditionElementNode.getTextContent();
        NamespaceContext context = new NodeNamespaceContext(conditionElementNode);
        return new BooleanExpression(expression, context);
    }

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

}
