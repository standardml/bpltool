package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

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
 * Java class for tIf complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tIf&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}condition&quot;/&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}elseif&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}else&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tIf", propOrder = { "conditionElementNode", "activity", "elseif", "_else" })
public class IfActivity extends Activity {

    @XmlAnyElement
    protected Element conditionElementNode;

    @XmlElements( { @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "pick", type = PickActivity.class),
            @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "rethrow", type = RethrowActivity.class) })
    protected Activity activity;

    protected List<Elseif> elseif;

    @XmlElement(name = "else")
    protected ActivityContainer _else;

    /**
     * Gets the condition expression.
     * 
     * @return the condition expression.
     */
    public BooleanExpression getConditionExpression() {
        if (conditionElementNode == null)
            return null;

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
     * @return possible object is {@link IfActivity } {@link AssignActivity }
     *         {@link ThrowActivity } {@link CompensateScopeActivity }
     *         {@link FlowActivity } {@link CompensateActivity }
     *         {@link SequenceActivity } {@link InvokeActivity }
     *         {@link WaitActivity } {@link PickActivity }
     *         {@link WhileActivity } {@link ExitActivity }
     *         {@link ReplyActivity } {@link RepeatUntilActivity }
     *         {@link ScopeActivity } {@link EmptyActivity }
     *         {@link ForEachActivity } {@link ReceiveActivity }
     *         {@link ValidateActivity } {@link RethrowActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link IfActivity }
     *            {@link AssignActivity } {@link ThrowActivity }
     *            {@link CompensateScopeActivity } {@link FlowActivity }
     *            {@link CompensateActivity } {@link SequenceActivity }
     *            {@link InvokeActivity } {@link WaitActivity }
     *            {@link PickActivity } {@link WhileActivity }
     *            {@link ExitActivity } {@link ReplyActivity }
     *            {@link RepeatUntilActivity } {@link ScopeActivity }
     *            {@link EmptyActivity } {@link ForEachActivity }
     *            {@link ReceiveActivity } {@link ValidateActivity }
     *            {@link RethrowActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

    /**
     * Gets the value of the elseif property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the elseif property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getElseif().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Elseif }
     * @return Elseifs
     */
    public List<Elseif> getElseif() {
        if (elseif == null) {
            elseif = new ArrayList<Elseif>();
        }
        return this.elseif;
    }

    /**
     * Gets the value of the else property.
     * 
     * @return possible object is {@link ActivityContainer }
     */
    public ActivityContainer getElse() {
        return _else;
    }

    /**
     * Sets the value of the else property.
     * 
     * @param value allowed object is {@link ActivityContainer }
     */
    public void setElse(ActivityContainer value) {
        this._else = value;
    }

}
