package com.beepell.deployment.bind;

import java.io.File;
import java.io.FileInputStream;
import java.net.URI;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.w3c.dom.Document;

import com.beepell.model.ProcessDescription;

/**
 * Binding mechanism to bind a S-BPEL document to the object model.
 * @author Tim Hallwyl
 *
 */
public class ModelBinder {
    
    private static final SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
    
    /**
     * Bind a S-BPEL document to the object model.
     * @param sbpel S-BPEL Document
     * @return internal Process object
     * @throws Exception if it fails to bind to the model
     */
    public static ProcessDescription bind(Document sbpel) throws Exception {        
        
        DOMSource source = new DOMSource(sbpel);
        Schema schema = schemaFactory.newSchema(new File("schemas/s-bpel.xsd"));
        schema.newValidator().validate(source);
        
        TransformerFactory transformerFactory = TransformerFactory.newInstance();
        
        // Transform S-BPEL into parse friendly non compliant format
        DOMResult result = new DOMResult();
        
        URI uri = ModelBinder.class.getResource("parsing.xsl").toURI();
        File file = new File(uri);
        StreamSource stylesheet = new StreamSource(new FileInputStream(file), uri.getPath());
        
        Transformer transformer = transformerFactory.newTransformer(stylesheet);
        transformer.transform(source, result);
        Document transformed = (Document) result.getNode();
        
        // Use JAXB to bind to model
        JAXBContext jc = JAXBContext.newInstance("com.beepell.model");
        Unmarshaller unmarshaller = jc.createUnmarshaller();
        JAXBElement element = (JAXBElement) unmarshaller.unmarshal(transformed);
        
        return (ProcessDescription) element.getValue();        
    }
    
    
}
