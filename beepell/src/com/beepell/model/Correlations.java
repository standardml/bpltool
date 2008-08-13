package com.beepell.model;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * XSD Authors: The child element correlation needs to be a Local Element
 * Declaration, because there is another correlation element defined for the
 * invoke activity.
 * <p>
 * Java class for tCorrelations complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tCorrelations&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}anyType&quot;&gt;
 *       &lt;sequence&gt;
 *         &lt;element name=&quot;correlation&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tCorrelation&quot; maxOccurs=&quot;unbounded&quot;/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tCorrelations", propOrder = { "correlation" })
public class Correlations {

    @XmlElement(required = true)
    protected List<Correlation> correlation;

    /**
     * Gets the value of the correlation property.
     * <p>
     * This accessor method returns a reference to the live list, not a
     * snapshot. Therefore any modification you make to the returned list will
     * be present inside the JAXB object. This is why there is not a <CODE>set</CODE>
     * method for the correlation property.
     * <p>
     * For example, to add a new item, do as follows:
     * 
     * <pre>
     * getCorrelation().add(newItem);
     * </pre>
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Correlation }
     * @return Correlation
     */
    public List<Correlation> getCorrelation() {
        if (correlation == null) {
            correlation = new ArrayList<Correlation>();
        }
        return this.correlation;
    }

}
