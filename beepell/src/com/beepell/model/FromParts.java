package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for tFromParts complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tFromParts&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element ref=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}fromPart&quot; maxOccurs=&quot;unbounded&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tFromParts", propOrder = { "fromPart" })
public class FromParts {

    @XmlElement(required = true)
    protected List<FromPart> fromPart;

    /**
     * Gets the value of the fromPart property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the fromPart property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getFromPart().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FromPart }
     * @return FromParts
     */
    public List<FromPart> getFromPart() {
        if (fromPart == null) {
            fromPart = new ArrayList<FromPart>();
        }
        return this.fromPart;
    }

}
