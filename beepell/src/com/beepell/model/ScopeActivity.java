package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * There is no schema-level default for "exitOnStandardFault" at "scope".
 * Because, it will inherit default from enclosing scope or process.
 * <p>
 * Java class for tScope complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tScope&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}partnerLinks&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}messageExchanges&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}variables&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}correlationSets&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}faultHandlers&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}compensationHandler&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}terminationHandler&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}eventHandlers&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;exitOnStandardFault&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; /&gt;
 *       &lt;attribute name=&quot;isolated&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tScope", propOrder = { "partnerLinks", "messageExchanges", "variables", "correlationSets", "faultHandlers", "compensationHandler", "terminationHandler", "eventHandlers", "activity" })
public class ScopeActivity extends Activity {

    protected PartnerLinks partnerLinks;

    protected MessageExchanges messageExchanges;

    protected Variables variables;

    protected CorrelationSets correlationSets;

    protected FaultHandlers faultHandlers;

    protected ActivityContainer compensationHandler;

    protected ActivityContainer terminationHandler;

    protected EventHandlers eventHandlers;

    @XmlElements( { @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "throw", type = ThrowActivity.class),
            @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "assign", type = AssignActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class) })
    protected Activity activity;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean exitOnStandardFault;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean isolated;

    /**
     * Gets the value of the partnerLinks property.
     * 
     * @return possible object is {@link PartnerLinks }
     */
    public PartnerLinks getPartnerLinks() {
        return partnerLinks;
    }

    /**
     * Sets the value of the partnerLinks property.
     * 
     * @param value allowed object is {@link PartnerLinks }
     */
    public void setPartnerLinks(PartnerLinks value) {
        this.partnerLinks = value;
    }

    /**
     * Gets the value of the messageExchanges property.
     * 
     * @return possible object is {@link MessageExchanges }
     */
    public MessageExchanges getMessageExchanges() {
        return messageExchanges;
    }

    /**
     * Sets the value of the messageExchanges property.
     * 
     * @param value allowed object is {@link MessageExchanges }
     */
    public void setMessageExchanges(MessageExchanges value) {
        this.messageExchanges = value;
    }

    /**
     * Gets the value of the variables property.
     * 
     * @return possible object is {@link Variables }
     */
    public Variables getVariables() {
        return variables;
    }

    /**
     * Sets the value of the variables property.
     * 
     * @param value allowed object is {@link Variables }
     */
    public void setVariables(Variables value) {
        this.variables = value;
    }

    /**
     * Gets the value of the correlationSets property.
     * 
     * @return possible object is {@link CorrelationSets }
     */
    public CorrelationSets getCorrelationSets() {
        return correlationSets;
    }

    /**
     * Sets the value of the correlationSets property.
     * 
     * @param value allowed object is {@link CorrelationSets }
     */
    public void setCorrelationSets(CorrelationSets value) {
        this.correlationSets = value;
    }

    /**
     * Gets the value of the faultHandlers property.
     * 
     * @return possible object is {@link FaultHandlers }
     */
    public FaultHandlers getFaultHandlers() {
        return faultHandlers;
    }

    /**
     * Sets the value of the faultHandlers property.
     * 
     * @param value allowed object is {@link FaultHandlers }
     */
    public void setFaultHandlers(FaultHandlers value) {
        this.faultHandlers = value;
    }

    /**
     * Gets the value of the compensationHandler property.
     * 
     * @return possible object is {@link ActivityContainer }
     */
    public ActivityContainer getCompensationHandler() {
        return compensationHandler;
    }

    /**
     * Sets the value of the compensationHandler property.
     * 
     * @param value allowed object is {@link ActivityContainer }
     */
    public void setCompensationHandler(ActivityContainer value) {
        this.compensationHandler = value;
    }

    /**
     * Gets the value of the terminationHandler property.
     * 
     * @return possible object is {@link ActivityContainer }
     */
    public ActivityContainer getTerminationHandler() {
        return terminationHandler;
    }

    /**
     * Sets the value of the terminationHandler property.
     * 
     * @param value allowed object is {@link ActivityContainer }
     */
    public void setTerminationHandler(ActivityContainer value) {
        this.terminationHandler = value;
    }

    /**
     * Gets the value of the eventHandlers property.
     * 
     * @return possible object is {@link EventHandlers }
     */
    public EventHandlers getEventHandlers() {
        return eventHandlers;
    }

    /**
     * Sets the value of the eventHandlers property.
     * 
     * @param value allowed object is {@link EventHandlers }
     */
    public void setEventHandlers(EventHandlers value) {
        this.eventHandlers = value;
    }

    /**
     * Gets the value of the activity property.
     * 
     * @return possible object is {@link EmptyActivity } {@link ExitActivity }
     *         {@link ForEachActivity } {@link WhileActivity }
     *         {@link RepeatUntilActivity } {@link RethrowActivity }
     *         {@link ValidateActivity } {@link PickActivity }
     *         {@link WaitActivity } {@link ThrowActivity } {@link IfActivity }
     *         {@link ScopeActivity } {@link AssignActivity }
     *         {@link FlowActivity } {@link ReceiveActivity }
     *         {@link CompensateScopeActivity } {@link ReplyActivity }
     *         {@link CompensateActivity } {@link InvokeActivity }
     *         {@link SequenceActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link EmptyActivity }
     *            {@link ExitActivity } {@link ForEachActivity }
     *            {@link WhileActivity } {@link RepeatUntilActivity }
     *            {@link RethrowActivity } {@link ValidateActivity }
     *            {@link PickActivity } {@link WaitActivity }
     *            {@link ThrowActivity } {@link IfActivity }
     *            {@link ScopeActivity } {@link AssignActivity }
     *            {@link FlowActivity } {@link ReceiveActivity }
     *            {@link CompensateScopeActivity } {@link ReplyActivity }
     *            {@link CompensateActivity } {@link InvokeActivity }
     *            {@link SequenceActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

    /**
     * Gets the value of the exitOnStandardFault property.
     * 
     * @return possible object is {@link String }
     */
    public Boolean isExitOnStandardFault() {
        return exitOnStandardFault;
    }

    /**
     * Sets the value of the exitOnStandardFault property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setExitOnStandardFault(Boolean value) {
        this.exitOnStandardFault = value;
    }

    /**
     * Gets the value of the isolated property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isIsolated() {
        if (isolated == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return isolated;
        }
    }

    /**
     * Sets the value of the isolated property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setIsolated(Boolean value) {
        this.isolated = value;
    }

}
