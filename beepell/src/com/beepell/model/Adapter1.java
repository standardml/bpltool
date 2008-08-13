package com.beepell.model;

import javax.xml.bind.annotation.adapters.XmlAdapter;

/**
 * Unused.
 */
public class Adapter1 extends XmlAdapter<String, Boolean> {

    @Override
    public Boolean unmarshal(String value) {
        return (com.beepell.deployment.bind.BooleanParser.parse(value));
    }

    @Override
    public String marshal(Boolean value) {
        return (com.beepell.deployment.bind.BooleanParser.print(value));
    }

}
