package com.beepell.model;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * <p>
 * Java class for tPattern.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * <p>
 * 
 * <pre>
 * &lt;simpleType name=&quot;tPattern&quot;&gt;
 *   &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}string&quot;&gt;
 *     &lt;enumeration value=&quot;request&quot;/&gt;
 *     &lt;enumeration value=&quot;response&quot;/&gt;
 *     &lt;enumeration value=&quot;request-response&quot;/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 */
@XmlEnum
public enum PatternEnumeration {

    /**
     * 
     */
    @XmlEnumValue("request")
    REQUEST("request"), 
    
    /**
     * 
     */
    @XmlEnumValue("response")
    RESPONSE("response"), 
    
    /**
     * 
     */
    @XmlEnumValue("request-response")
    REQUEST_RESPONSE("request-response");

    private final String value;

    PatternEnumeration(String v) {
        value = v;
    }

    /**
     * 
     * @return value
     */
    public String value() {
        return value;
    }

    /**
     * 
     * @param v
     * @return PatternEnumeration
     */
    public static PatternEnumeration fromValue(String v) {
        for (PatternEnumeration c : PatternEnumeration.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v.toString());
    }

}
