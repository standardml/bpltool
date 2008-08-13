package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for tFlow complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tFlow&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}links&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot; maxOccurs=&quot;unbounded&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tFlow", propOrder = { "links", "activity" })
public class FlowActivity extends Activity {

    protected Links links;

    @XmlElements( { @XmlElement(name = "receive", type = ReceiveActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "exit", type = ExitActivity.class),
            @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class) })
    protected List<Activity> activity;

    /**
     * Gets the value of the links property.
     * 
     * @return possible object is {@link Links }
     */
    public Links getLinks() {
        return links;
    }

    /**
     * Sets the value of the links property.
     * 
     * @param value allowed object is {@link Links }
     */
    public void setLinks(Links value) {
        this.links = value;
    }

    /**
     * Gets the value of the activity property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the activity property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getActivity().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ReceiveActivity } {@link EmptyActivity } {@link FlowActivity }
     * {@link ThrowActivity } {@link AssignActivity } {@link IfActivity }
     * {@link WhileActivity } {@link InvokeActivity } {@link SequenceActivity }
     * {@link ExitActivity } {@link ScopeActivity } {@link ForEachActivity }
     * {@link ReplyActivity } {@link WaitActivity } {@link CompensateActivity }
     * {@link ValidateActivity } {@link PickActivity } {@link RethrowActivity }
     * {@link CompensateScopeActivity } {@link RepeatUntilActivity }
     * @return Activities
     */
    public List<Activity> getActivity() {
        if (activity == null) {
            activity = new ArrayList<Activity>();
        }
        return this.activity;
    }

}
