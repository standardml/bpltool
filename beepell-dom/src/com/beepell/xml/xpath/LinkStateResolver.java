package com.beepell.xml.xpath;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathVariableResolver;

import com.beepell.execution.bpel.Context;

/**
 * @author Tim Hallwyl
 * 
 */
public class LinkStateResolver implements XPathVariableResolver {

    private final Context context;

    /**
     * Create a link state resolver based on a context.
     * 
     * @param context
     */
    public LinkStateResolver(final Context context) {
        this.context = context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.xpath.XPathVariableResolver#resolveVariable(javax.xml.namespace.QName)
     */
    @Override
    public Object resolveVariable(QName variableName) {
        return this.context.getLinkState(variableName.getLocalPart());
    }

}
