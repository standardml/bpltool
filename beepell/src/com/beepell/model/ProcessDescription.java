package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * <p>
 * Java class for tProcess complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tProcess&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}extensions&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}import&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}partnerLinks&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}messageExchanges&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}variables&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}correlationSets&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}faultHandlers&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}eventHandlers&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;group ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}activity&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;exitOnStandardFault&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *       &lt;attribute name=&quot;name&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}NCName&quot; /&gt;
 *       &lt;attribute name=&quot;suppressJoinFailure&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *       &lt;attribute name=&quot;targetNamespace&quot; use=&quot;required&quot; type=&quot;{http://www.w3.org/2001/XMLSchema}anyURI&quot; /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tProcess", propOrder = { "extensions", "_import", "partnerLinks", "messageExchanges", "variables", "correlationSets", "faultHandlers", "eventHandlers", "activity" })
public class ProcessDescription {

    protected TExtensions extensions;

    @XmlElement(name = "import")
    protected List<Import> _import;

    protected PartnerLinks partnerLinks;

    protected MessageExchanges messageExchanges;

    protected Variables variables;

    protected CorrelationSets correlationSets;

    protected FaultHandlers faultHandlers;

    protected EventHandlers eventHandlers;

    @XmlElements( { @XmlElement(name = "empty", type = EmptyActivity.class), @XmlElement(name = "exit", type = ExitActivity.class), @XmlElement(name = "invoke", type = InvokeActivity.class), @XmlElement(name = "wait", type = WaitActivity.class), @XmlElement(name = "rethrow", type = RethrowActivity.class), @XmlElement(name = "if", type = IfActivity.class), @XmlElement(name = "compensateScope", type = CompensateScopeActivity.class), @XmlElement(name = "reply", type = ReplyActivity.class), @XmlElement(name = "while", type = WhileActivity.class), @XmlElement(name = "validate", type = ValidateActivity.class),
            @XmlElement(name = "throw", type = ThrowActivity.class), @XmlElement(name = "sequence", type = SequenceActivity.class), @XmlElement(name = "receive", type = ReceiveActivity.class), @XmlElement(name = "compensate", type = CompensateActivity.class), @XmlElement(name = "repeatUntil", type = RepeatUntilActivity.class), @XmlElement(name = "flow", type = FlowActivity.class), @XmlElement(name = "scope", type = ScopeActivity.class), @XmlElement(name = "pick", type = PickActivity.class), @XmlElement(name = "forEach", type = ForEachActivity.class), @XmlElement(name = "assign", type = AssignActivity.class) })
    protected Activity activity;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean exitOnStandardFault;

    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    protected String name;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean suppressJoinFailure;

    @XmlAttribute(required = true)
    protected String targetNamespace;

    /**
     * Gets the value of the extensions property.
     * 
     * @return possible object is {@link TExtensions }
     */
    public TExtensions getExtensions() {
        return extensions;
    }

    /**
     * Sets the value of the extensions property.
     * 
     * @param value allowed object is {@link TExtensions }
     */
    public void setExtensions(TExtensions value) {
        this.extensions = value;
    }

    /**
     * Gets the value of the import property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the import property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getImport().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Import }
     * @return Import
     */
    public List<Import> getImport() {
        if (_import == null) {
            _import = new ArrayList<Import>();
        }
        return this._import;
    }

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
     *         {@link InvokeActivity } {@link WaitActivity }
     *         {@link RethrowActivity } {@link IfActivity }
     *         {@link CompensateScopeActivity } {@link ReplyActivity }
     *         {@link WhileActivity } {@link ValidateActivity }
     *         {@link ThrowActivity } {@link SequenceActivity }
     *         {@link ReceiveActivity } {@link CompensateActivity }
     *         {@link RepeatUntilActivity } {@link FlowActivity }
     *         {@link ScopeActivity } {@link PickActivity }
     *         {@link ForEachActivity } {@link AssignActivity }
     */
    public Activity getActivity() {
        return activity;
    }

    /**
     * Sets the value of the activity property.
     * 
     * @param value allowed object is {@link EmptyActivity }
     *            {@link ExitActivity } {@link InvokeActivity }
     *            {@link WaitActivity } {@link RethrowActivity }
     *            {@link IfActivity } {@link CompensateScopeActivity }
     *            {@link ReplyActivity } {@link WhileActivity }
     *            {@link ValidateActivity } {@link ThrowActivity }
     *            {@link SequenceActivity } {@link ReceiveActivity }
     *            {@link CompensateActivity } {@link RepeatUntilActivity }
     *            {@link FlowActivity } {@link ScopeActivity }
     *            {@link PickActivity } {@link ForEachActivity }
     *            {@link AssignActivity }
     */
    public void setActivity(Activity value) {
        this.activity = value;
    }

    /**
     * Gets the value of the exitOnStandardFault property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isExitOnStandardFault() {
        if (exitOnStandardFault == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return exitOnStandardFault;
        }
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
     * Gets the value of the name property.
     * 
     * @return possible object is {@link String }
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the suppressJoinFailure property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isSuppressJoinFailure() {
        if (suppressJoinFailure == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return suppressJoinFailure;
        }
    }

    /**
     * Sets the value of the suppressJoinFailure property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setSuppressJoinFailure(Boolean value) {
        this.suppressJoinFailure = value;
    }

    /**
     * Gets the value of the targetNamespace property.
     * 
     * @return possible object is {@link String }
     */
    public String getTargetNamespace() {
        return targetNamespace;
    }

    /**
     * Sets the value of the targetNamespace property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setTargetNamespace(String value) {
        this.targetNamespace = value;
    }

}
