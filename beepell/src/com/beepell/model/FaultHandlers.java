package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for tFaultHandlers complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tFaultHandlers&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}catch&quot; maxOccurs=&quot;unbounded&quot; minOccurs=&quot;0&quot;/&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}catchAll&quot; minOccurs=&quot;0&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tFaultHandlers", propOrder = { "_catch", "catchAll" })
public class FaultHandlers {

    @XmlElement(name = "catch")
    protected List<Catch> _catch;

    protected ActivityContainer catchAll;

    /**
     * Gets the value of the catch property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the catch property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getCatch().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list {@link Catch }
     * @return Catchs
     */
    public List<Catch> getCatch() {
        if (_catch == null) {
            _catch = new ArrayList<Catch>();
        }
        return this._catch;
    }

    /**
     * Gets the value of the catchAll property.
     * 
     * @return possible object is {@link ActivityContainer }
     */
    public ActivityContainer getCatchAll() {
        return catchAll;
    }

    /**
     * Sets the value of the catchAll property.
     * 
     * @param value allowed object is {@link ActivityContainer }
     */
    public void setCatchAll(ActivityContainer value) {
        this.catchAll = value;
    }

}
