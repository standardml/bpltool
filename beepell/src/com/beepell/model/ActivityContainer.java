package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for tActivityContainer complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tActivityContainer&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tActivityContainer", propOrder = { "activity" })
public class ActivityContainer {

    @XmlElements( { @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class),
            @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class) })
    protected Activity activity;

    /**
     * Gets the value of the activity property.
     * 
     * @return possible object is {@link AssignActivity } {@link PickActivity }
     *         {@link RepeatUntilActivity } {@link EmptyActivity }
     *         {@link CompensateActivity } {@link ThrowActivity }
     *         {@link ReceiveActivity } {@link FlowActivity }
     *         {@link IfActivity } {@link ReplyActivity }
     *         {@link RethrowActivity } {@link WhileActivity }
     *         {@link InvokeActivity } {@link ValidateActivity }
     *         {@link SequenceActivity } {@link WaitActivity }
     *         {@link ScopeActivity } {@link CompensateScopeActivity }
     *         {@link ExitActivity } {@link ForEachActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link AssignActivity }
     *            {@link PickActivity } {@link RepeatUntilActivity }
     *            {@link EmptyActivity } {@link CompensateActivity }
     *            {@link ThrowActivity } {@link ReceiveActivity }
     *            {@link FlowActivity } {@link IfActivity }
     *            {@link ReplyActivity } {@link RethrowActivity }
     *            {@link WhileActivity } {@link InvokeActivity }
     *            {@link ValidateActivity } {@link SequenceActivity }
     *            {@link WaitActivity } {@link ScopeActivity }
     *            {@link CompensateScopeActivity } {@link ExitActivity }
     *            {@link ForEachActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

}
