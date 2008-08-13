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
 * Java class for tWhile complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tWhile&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}condition&quot;/&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tWhile", propOrder = { "conditionElementNode", "activity" })
public class WhileActivity extends Activity {

    @XmlAnyElement
    protected Element conditionElementNode;

    @XmlElements( { @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class),
            @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "assign", type = AssignActivity.class) })
    protected Activity activity;

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

    /**
     * Gets the value of the activity property.
     * 
     * @return possible object is {@link ThrowActivity }
     *         {@link CompensateActivity } {@link InvokeActivity }
     *         {@link SequenceActivity } {@link ValidateActivity }
     *         {@link EmptyActivity } {@link ExitActivity }
     *         {@link RepeatUntilActivity } {@link WaitActivity }
     *         {@link ReceiveActivity } {@link RethrowActivity }
     *         {@link IfActivity } {@link ScopeActivity }
     *         {@link CompensateScopeActivity } {@link FlowActivity }
     *         {@link ForEachActivity } {@link ReplyActivity }
     *         {@link PickActivity } {@link WhileActivity }
     *         {@link AssignActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link ThrowActivity }
     *            {@link CompensateActivity } {@link InvokeActivity }
     *            {@link SequenceActivity } {@link ValidateActivity }
     *            {@link EmptyActivity } {@link ExitActivity }
     *            {@link RepeatUntilActivity } {@link WaitActivity }
     *            {@link ReceiveActivity } {@link RethrowActivity }
     *            {@link IfActivity } {@link ScopeActivity }
     *            {@link CompensateScopeActivity } {@link FlowActivity }
     *            {@link ForEachActivity } {@link ReplyActivity }
     *            {@link PickActivity } {@link WhileActivity }
     *            {@link AssignActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

}
