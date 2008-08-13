package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

/**
 * XSD Authors: The child element onAlarm needs to be a Local Element
 * Declaration, because there is another onAlarm element defined for the pick
 * activity.
 * <p>
 * Java class for tEventHandlers complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tEventHandlers&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}onEvent&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element name=&quot;onAlarm&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tOnAlarmEvent&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tEventHandlers", propOrder = { "onEvent", "onAlarm" })
public class EventHandlers {

    protected List<OnEvent> onEvent;

    protected List<OnAlarmEvent> onAlarm;

    /**
     * Gets the value of the onEvent property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the onEvent property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getOnEvent().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link OnEvent }
     * @return OnEvents
     */
    public List<OnEvent> getOnEvent() {
        if (onEvent == null) {
            onEvent = new ArrayList<OnEvent>();
        }
        return this.onEvent;
    }

    /**
     * Gets the value of the onAlarm property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the onAlarm property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getOnAlarm().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link OnAlarmEvent }
     * @return OnAlarmEvents
     */
    public List<OnAlarmEvent> getOnAlarm() {
        if (onAlarm == null) {
            onAlarm = new ArrayList<OnAlarmEvent>();
        }
        return this.onAlarm;
    }

}
