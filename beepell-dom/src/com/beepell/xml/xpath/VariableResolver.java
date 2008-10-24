package com.beepell.xml.xpath;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathVariableResolver;

import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.beepell.execution.bpel.Context;

/**
 * An implementation of XPathVariableResolver based on ExpressionContext.
 * <p>
 * The job of the VariableResolver is to be an adapter between the
 * ExpressionContext and the XPathVariableResolver.
 * 
 * @author Tim Hallwyl
 */
public class VariableResolver extends Resolver implements XPathVariableResolver {

    /**
     * Create a VariableResolver based on an ExpressionContext.
     * 
     * @param context
     * @throws SAXException 
     */
    public VariableResolver(Context context) throws SAXException {
        super(context);
    }

    public Object resolveVariable(QName variableName) {
        if (variableName == null)
            throw new IllegalArgumentException("VariableResolver.resolveVariable: variableName must not be null.");

        String variable = variableName.getLocalPart();

        if (variable == null)
            throw new IllegalArgumentException("VariableResolver.resolveVariable: variable QName has no local name.");

        Node value = null;
        QName type = null;
        try {
            if (variable.contains(".")) {
                // Message part variable
                String part = variable.substring(variable.indexOf('.') + 1);
                variable = variable.substring(0, variable.indexOf('.'));
                value = this.context.getVariableValue(variable, part);
                type = this.context.getVariableType(variable, part);

            } else {
                // Ordinary type or element based variable.
                value = this.context.getVariableValue(variable);
                type = this.context.getVariableType(variable);
            }
        } catch (Exception exception) {
            // TODO: Should this cause a UninitializedVariable fault form
            // evaluation?
            System.err.println("ERROR: Resolving variable '" + variableName.getLocalPart() + " reulted in an exception: " + exception.getLocalizedMessage());
            exception.printStackTrace();
            return null;
        }

        

        if (type == null)
            throw new IllegalArgumentException("VariableResolver.resolveVariable: context returned null as type for " + variable);

        return toXPathObject(value, type);

    }

}
