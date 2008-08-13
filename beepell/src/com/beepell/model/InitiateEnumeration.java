package com.beepell.model;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * <p>
 * Java class for tInitiate.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * <p>
 * 
 * <pre>
 * &lt;simpleType name=&quot;tInitiate&quot;&gt;
 *   &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}string&quot;&gt;
 *     &lt;enumeration value=&quot;yes&quot;/&gt;
 *     &lt;enumeration value=&quot;join&quot;/&gt;
 *     &lt;enumeration value=&quot;no&quot;/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 */
@XmlEnum
public enum InitiateEnumeration {

    /**
     * 
     */
    @XmlEnumValue("yes")
    YES("yes"),
    
    /**
     * 
     */
    @XmlEnumValue("join")
    JOIN("join"),
    
    /**
     * 
     */
    @XmlEnumValue("no")
    NO("no");

    private final String value;

    InitiateEnumeration(String v) {
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
     * @return InitiateEnumeration
     */
    public static InitiateEnumeration fromValue(String v) {
        for (InitiateEnumeration c : InitiateEnumeration.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v.toString());
    }

}
