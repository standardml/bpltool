package com.beepell.deployment.transform;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.util.logging.Logger;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;

import com.beepell.Settings;

/**
 * Utility class to transform a BPEL process description document into a BPEL
 * SEF document.
 * 
 * @author Tim Hallwyl
 */
public class SourceTransformer {
    private static final Logger log = Settings.getLogger();

    private static TransformerFactory transformerFactory = TransformerFactory.newInstance();

    /**
     * Transforms a standard complaint BPEL document into a Simplified BPEL (S-BPEL) document.
     * 
     * @param document A standard complaint BPEL document.
     * @return A Simplified BPEL (S-BPEL) document.
     * @throws TransformerFactoryConfigurationError
     * @throws TransformerException
     * @throws IOException
     */
    public static Document transform(Document document) throws TransformerFactoryConfigurationError, TransformerException, IOException {

        String[] sheets = { "extensions.xsl", "language.xsl", "irra.xsl", "handlers.xsl", "attributes.xsl"};
        Transformer transformer;
        DOMSource source = new DOMSource(document);
        DOMResult result = null;

        for (int index = 0; index < sheets.length; index++) {
            
            log.info("Applying transformation '" + sheets[index] + "'.");
            transformer = getTransformer(sheets[index]);
            transformer.setErrorListener(new ErrorListener());
            result = new DOMResult();
            transformer.transform(source, result);
            source = new DOMSource(result.getNode());

        }
       
        return (Document) result.getNode();
    }

    private static Transformer getTransformer(String filename) throws TransformerConfigurationException, TransformerFactoryConfigurationError, FileNotFoundException {
            URL url = SourceTransformer.class.getResource(filename);
            StreamSource source = new StreamSource(url.toString());
            return transformerFactory.newTransformer(source);
    }
}
