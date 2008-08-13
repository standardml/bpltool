package com.beepell.model;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;

/**
 * <p>
 * Java class for tCorrelationWithPattern complex type.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name=&quot;tCorrelationWithPattern&quot;&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tCorrelation&quot;&gt;
 *       &lt;attribute name=&quot;pattern&quot; type=&quot;{http://docs.oasis-open.org/wsbpel/2.0/process/executable}tPattern&quot; /&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "tCorrelationWithPattern")
public class CorrelationWithPattern extends Correlation {

    @XmlAttribute
    protected PatternEnumeration pattern;

    /**
     * Gets the value of the pattern property.
     * 
     * @return possible object is {@link PatternEnumeration }
     */
    public PatternEnumeration getPattern() {
        return pattern;
    }

    /**
     * Sets the value of the pattern property.
     * 
     * @param value allowed object is {@link PatternEnumeration }
     */
    public void setPattern(PatternEnumeration value) {
        this.pattern = value;
    }

}
