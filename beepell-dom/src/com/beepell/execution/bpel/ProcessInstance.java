package com.beepell.execution.bpel;

import java.io.File;
import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * @author Tim Hallwyl
 *
 */
public class ProcessInstance {

    private final Document document;
    
    /**
     * Create a process instance from a W3C DOM Document.
     *  
     * @param instance The Document containing the instance tree.
     */
    public ProcessInstance(Document instance) {
        this.document = instance;
    }
    
    /**
     * Create a process instance from an XML file.
     * 
     * @param instance The File containing the XML instance tree.
     * @throws IOException If the file cannot be opened or parsed.
     */
    public ProcessInstance(File instance) throws IOException {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setValidating(false);
            factory.setCoalescing(false);
            factory.setIgnoringComments(true);
            factory.setIgnoringElementContentWhitespace(true);
            factory.setNamespaceAware(true);
            DocumentBuilder builder = factory.newDocumentBuilder();
            this.document = builder.parse(instance);
            this.document.normalizeDocument();
            
            if (!Utils.isInstanceElement(this.document.getDocumentElement()))
                throw new IOException("File does not contain a process instance tree.");

        } catch (SAXException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        } catch (ParserConfigurationException exception) {
            throw new IOException(exception.getLocalizedMessage(), exception);
        }
    }
    
    /**
     * Create a process instance from an URI to an XML resource.
     * 
     * @param instance The URI to a resource containing the XML instance tree.
     * @throws IOException If the file cannot be opened or parsed.
     */
    public ProcessInstance(java.net.URI instance) throws IOException {
        this(new File(instance));
    }
    
    /**
     * Executes the next step, if any.
     */
    public synchronized void step() {
        
        Element next = null;
        
        Context context = new Context(next);
        // TODO link semantics
        Semantics.rewrite(next, context);
        // TODO link semantics
        
    }
    
    
    
}
