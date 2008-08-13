package com.beepell.deployment.bind;

/**
 * Custom JAXB parser for BPEL booleans
 * 
 * @author Tim Hallwyl
 */
public class BooleanParser {

    /**
     * 
     * @param xml
     * @return true or false boolean value
     */
    public static boolean parse(String xml) {

        if (xml.equals("yes"))
            return true;
        if (xml.equals("no"))
            return false;

        throw new IllegalArgumentException(xml + " is not a valid boolean value.");
    }

    /**
     * 
     * @param value
     * @return yes or no string value.
     */
    public static String print(boolean value) {
        if (value)
            return "yes";
        else
            return "no";
    }

}
