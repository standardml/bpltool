package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * XSD Authors: The child element onAlarm needs to be a Local Element
 * Declaration, because there is another onAlarm element defined for event
 * handlers.
 * <p>
 * Java class for tPick complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tPick&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tActivity&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}onMessage&quot; maxOccurs=&quot;unbounded&quot;/&gt;
 *         &lt;element name=&quot;onAlarm&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tOnAlarmPick&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name=&quot;createInstance&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tBoolean&quot; default=&quot;no&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tPick", propOrder = { "onMessage", "onAlarm" })
public class PickActivity extends Activity {

    @XmlElement(required = true)
    protected List<OnMessage> onMessage;

    protected List<OnAlarmPick> onAlarm;

    @XmlAttribute
    @XmlJavaTypeAdapter(Adapter1.class)
    protected Boolean createInstance;

    /**
     * Gets the value of the onMessage property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the onMessage property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getOnMessage().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link OnMessage }
     * @return OnMessages
     */
    public List<OnMessage> getOnMessage() {
        if (onMessage == null) {
            onMessage = new ArrayList<OnMessage>();
        }
        return this.onMessage;
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
     * {@link OnAlarmPick }
     * @return OnAlarmPicks
     */
    public List<OnAlarmPick> getOnAlarm() {
        if (onAlarm == null) {
            onAlarm = new ArrayList<OnAlarmPick>();
        }
        return this.onAlarm;
    }

    /**
     * Gets the value of the createInstance property.
     * 
     * @return possible object is {@link String }
     */
    public boolean isCreateInstance() {
        if (createInstance == null) {
            return new Adapter1().unmarshal("no");
        } else {
            return createInstance;
        }
    }

    /**
     * Sets the value of the createInstance property.
     * 
     * @param value allowed object is {@link String }
     */
    public void setCreateInstance(Boolean value) {
        this.createInstance = value;
    }

}
