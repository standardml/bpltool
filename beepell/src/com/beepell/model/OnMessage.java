package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for tOnMessage complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tOnMessage&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tOnMsgCommon&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tOnMessage", propOrder = { "activity" })
public class OnMessage extends OnMsgCommon {

    @XmlElements( { @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class),
            @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class) })
    protected Activity activity;

    /**
     * Gets the value of the activity property.
     * 
     * @return possible object is {@link ScopeActivity }
     *         {@link ForEachActivity } {@link InvokeActivity }
     *         {@link SequenceActivity } {@link RethrowActivity }
     *         {@link IfActivity } {@link FlowActivity }
     *         {@link CompensateScopeActivity } {@link AssignActivity }
     *         {@link ReceiveActivity } {@link ValidateActivity }
     *         {@link CompensateActivity } {@link ThrowActivity }
     *         {@link ExitActivity } {@link RepeatUntilActivity }
     *         {@link WhileActivity } {@link PickActivity }
     *         {@link WaitActivity } {@link EmptyActivity }
     *         {@link ReplyActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link ScopeActivity }
     *            {@link ForEachActivity } {@link InvokeActivity }
     *            {@link SequenceActivity } {@link RethrowActivity }
     *            {@link IfActivity } {@link FlowActivity }
     *            {@link CompensateScopeActivity } {@link AssignActivity }
     *            {@link ReceiveActivity } {@link ValidateActivity }
     *            {@link CompensateActivity } {@link ThrowActivity }
     *            {@link ExitActivity } {@link RepeatUntilActivity }
     *            {@link WhileActivity } {@link PickActivity }
     *            {@link WaitActivity } {@link EmptyActivity }
     *            {@link ReplyActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

}
