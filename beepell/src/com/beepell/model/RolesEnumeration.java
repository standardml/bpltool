package com.beepell.model;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;

/**
 * <p>
 * Java class for tRoles.
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * <p>
 * 
 * <pre>
 * &lt;simpleType name=&quot;tRoles&quot;&gt;
 *   &lt;restriction base=&quot;{http://www.w3.org/2001/XMLSchema}string&quot;&gt;
 *     &lt;enumeration value=&quot;myRole&quot;/&gt;
 *     &lt;enumeration value=&quot;partnerRole&quot;/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 */
@XmlEnum
public enum RolesEnumeration {

    /**
     * 
     */
    @XmlEnumValue("myRole")
    MY_ROLE("myRole"), /**
     * 
     */
    @XmlEnumValue("partnerRole")
    PARTNER_ROLE("partnerRole");

    private final String value;

    RolesEnumeration(String v) {
        value = v;
    }

    /**
     * @return value
     */
    public String value() {
        return value;
    }

    /**
     * @param v
     * @return RolesEnumeration
     */
    public static RolesEnumeration fromValue(String v) {
        for (RolesEnumeration c : RolesEnumeration.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v.toString());
    }

}
