package com.beepell.expression;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathVariableResolver;

/**
 * Implementation of XPathVariableResolver to access link state as XPath
 * variables. This is used for evaluation of JoinConditionExpressions. See
 * section.
 * <p>
 * Link status is obtained via XPath variable bindings, manifesting links that
 * target the activity containing the Enclosing Element as XPath variable
 * bindings of identical name. ... Link variables are manifested as XPath
 * Boolean objects whose value will be set to the link's value. [8.2.5]
 * </p>
 * <p>
 * Implementation note: A links status may be; true, false or unset. An activity
 * must not start before all incoming links are set and the join condition is
 * evaluated. This implementation assumes that all incoming links are set,
 * before attempting to evaluate the join condition. This is an implicit
 * requiremet, as a boolean link state is required to produce a boolean result.
 * </p>
 * 
 * @author Tim Hallwyl
 */
public class LinkStateResolver extends Resolver implements XPathVariableResolver {

    /**
     * Creates a LinkStateResolver based on the ExpressionContext.
     * 
     * @param context
     */
    public LinkStateResolver(ExpressionContext context) {
        super(context);
    }

    public Boolean resolveVariable(QName variableName) {
        
        Boolean state = context.getLinkState(variableName.getLocalPart());
        if (state != null)
            return state;
        else
            return null;
    }

}
