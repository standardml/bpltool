package com.beepell.expression;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.w3c.dom.Node;

import com.sun.xml.xsom.XSType;

/**
 * VariableResolver and FunctionResolver share the toXPathObject method.
 * LinnkStateResolver, VariableResolver and FunctionResolver all access an
 * ExpressionContext.
 * 
 * @author Tim Hallwyl
 */
public abstract class Resolver {

    protected final ExpressionContext context;

    protected final XSType booleanType;
    protected final XSType floatType;
    protected final XSType intType;
    protected final XSType unsignedInt;
    
    /**
     * Initializing context.
     * @param context
     */
    public Resolver(ExpressionContext context) {
        this.context = context;
        this.booleanType = context.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "boolean"));
        this.floatType = context.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "float"));
        this.intType = context.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "int"));
        this.unsignedInt = context.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedInt"));
    }
    
    protected Object toXPathObject(Node value, QName type) {
        if (value == null)
            throw new IllegalArgumentException("VariableResolver.toXPathObject: value must not be null");
        
        if (type == null)
            throw new IllegalArgumentException("VariableResolver.toXPathObject: type must not be null");
                
        XSType xs = context.getType(type);       

        
        /*
         * If the XML Schema type of the WS-BPEL simple type variable is
         * xsd:boolean or any types that are restrictions of xsd:boolean then
         * the WS-BPEL variable MUST be manifested as an XPath Boolean object.
         * If the XML Schema type of the WS-BPEL simple type variable is
         * xsd:float, xsd:int, xsd:unsignedInt or any restrictions of those
         * types then the WS-BPEL variable MUST be manifested as an XPath float
         * object. Any other XML Schema types MUST be manifested as an XPath
         * string object. [8.2.2]
         */
        if (xs != null && xs.isSimpleType()) {
            // TODO Is it selection failure if the selected node is not a Text or Attr node?

            if (xs.isDerivedFrom(booleanType))
                return Boolean.parseBoolean(value.getNodeValue());

            if (xs.isDerivedFrom(floatType) || xs.isDerivedFrom(intType) || xs.isDerivedFrom(unsignedInt))
                return Float.parseFloat(value.getNodeValue());

            return value.getNodeValue();
        }

        /*
         * WS-BPEL variables declared using a complex type MUST be manifested as
         * a node-set XPath variable with one member node containing the
         * anonymous document element that contains the actual value of the
         * WS-BPEL complex type variable. [8.2.2]
         */
        if (xs != null && xs.isComplexType()) {
            return value;
            
        }

        /*
         * WS-BPEL variables declared using an element MUST be manifested as a
         * node-set XPath variable with a single member node. That node is a
         * synthetic DII that contains a single child, the document element,
         * which is the value of the WS-BPEL variable. The XPath variable
         * binding will bind to the document element. [8.2.2]
         */
        return value;

    }

    

}
