package com.beepell.xml.xpath;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.sun.xml.xsom.XSType;

import com.beepell.execution.bpel.Context;
import com.beepell.repository.SchemaRepository;

/**
 * VariableResolver and FunctionResolver share the toXPathObject method.
 * LinnkStateResolver, VariableResolver and FunctionResolver all access an
 * ExpressionContext.
 * 
 * @author Tim Hallwyl
 */
public abstract class Resolver {

    protected final Context context;
    protected final SchemaRepository schemas;
    protected final XSType booleanType;
    protected final XSType floatType;
    protected final XSType intType;
    protected final XSType unsignedInt;
    
    /**
     * Initializing context.
     * @param context
     * @throws SAXException 
     */
    public Resolver(Context context) throws SAXException {
        this.context = context;
        this.schemas = context.getSchemas();
        this.booleanType = this.schemas.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "boolean"));
        this.floatType = this.schemas.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "float"));
        this.intType = this.schemas.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "int"));
        this.unsignedInt = this.schemas.getType(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedInt"));
    }
    
    protected Object toXPathObject(Node value, QName type) {
        if (value == null)
            throw new IllegalArgumentException("VariableResolver.toXPathObject: value must not be null");
        
        if (type == null)
            throw new IllegalArgumentException("VariableResolver.toXPathObject: type must not be null");
                
        XSType xs;
        try {
            xs = this.schemas.getType(type);
        } catch (SAXException exception) {            
            throw new IllegalStateException("Failed to get type information on " + type.getLocalPart(), exception);
        }       

        
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

            if (xs.isDerivedFrom(this.booleanType))
                return new Boolean(Boolean.parseBoolean(value.getNodeValue()));

            if (xs.isDerivedFrom(this.floatType) || xs.isDerivedFrom(this.intType) || xs.isDerivedFrom(this.unsignedInt))
                return new Float(Float.parseFloat(value.getNodeValue()));

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
