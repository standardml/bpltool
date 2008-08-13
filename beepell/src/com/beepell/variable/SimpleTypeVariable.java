package com.beepell.variable;

import java.io.IOException;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.beepell.exceptions.InvalidVariables;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.repository.SchemaRepository;

/**
 * A simple type variable.
 * 
 * @author Tim Hallwyl
 */
public class SimpleTypeVariable extends Variable {

    /**
     * Creates a simple type variable.
     * 
     * @param type the type of this variable
     * @param name the name used to access this variable
     * @param repository Repository of schema definitions used for validation.
     * @throws ParserConfigurationException if it is unable to create the
     *             variable document.
     */
    public SimpleTypeVariable(QName type, String name, SchemaRepository repository) throws ParserConfigurationException {
        super(type, name, repository);

    }

    @Override
    public Text getValue() throws UninitializedVariable {
        if (!initialized)
            throw new UninitializedVariable("Variable '" + name + "' is not initialized.");
        else
            return (Text) document.getDocumentElement().getFirstChild();
    }

    @Override
    public void validate() throws InvalidVariables, UninitializedVariable {
        if (!this.isInitialized())
            throw new UninitializedVariable("Simple type variable '" + name + "' is not initialized.");
        
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            Document document = dbf.newDocumentBuilder().newDocument();

            Element node = (Element) document.importNode(this.document.getDocumentElement(), true);

            node.setAttributeNS(XMLConstants.XMLNS_ATTRIBUTE_NS_URI, "xmlns:foo", type.getNamespaceURI());
            node.setAttributeNS(XMLConstants.W3C_XML_SCHEMA_INSTANCE_NS_URI, "type", "foo:" + type.getLocalPart());
            node = (Element) document.appendChild(node);

            //System.out.print(XML.toString(node));
            repository.getSchema(type).newValidator().validate(new DOMSource(node));

        } catch (SAXException exception) {
              throw new InvalidVariables("Variable '" + name + "' is invalid: " + exception.getLocalizedMessage());
        } catch (IOException exception) {
            throw new IllegalStateException("Variable '" + name + "' could not be validated: " + exception.getLocalizedMessage());
        } catch (ParserConfigurationException exception) {
            throw new IllegalStateException("Variable '" + name + "' could not be validated: " + exception.getLocalizedMessage());
        }

    }

    @Override
    public void initialize() {
        Element documentElement = this.document.createElementNS("http://beepell.com/variable/", "beepeel:simpleTypeVariable");
        this.document.appendChild(documentElement);
        Text text = document.createTextNode("");
        documentElement.appendChild(text);
        this.initialized = true;
    }

    @Override
    public SimpleTypeVariable clone() {
        try {
            SimpleTypeVariable copy = new SimpleTypeVariable(this.getType(), this.getName(), this.repository);
            copy(this, copy);
            return copy;
        } catch (ParserConfigurationException exception) {
            /* Should not happen */
            exception.printStackTrace();
            return null;
        }
    }

    @Override
    public void copyOf(Variable source) throws IllegalArgumentException {        
        copy(source, this);
    }
    
}
