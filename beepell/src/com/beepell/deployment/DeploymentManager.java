package com.beepell.deployment;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import javax.wsdl.WSDLException;
import javax.xml.XMLConstants;
import javax.xml.transform.TransformerException;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.beepell.BPELConstants;
import com.beepell.broker.MessageBroker;
import com.beepell.deployment.analysis.Analyser;
import com.beepell.deployment.bind.ModelBinder;
import com.beepell.deployment.transform.SourceTransformer;
import com.beepell.model.ProcessDescription;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.util.XML;

/**
 * Deployment Manager. Handles deployment of processes.
 * 
 * @author Tim Hallwyl
 */
public class DeploymentManager {

    private static final SchemaFactory schemaFactory = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);

    private static final List<ProcessContext> deployed = new ArrayList<ProcessContext>();

    /**
     * Deploy a process.
     * 
     * @param bpel
     * @return the process context deployed.
     * @throws Exception
     */
    public static ProcessContext deploy(File bpel) throws Exception {
        // 1. Initial parsing of the process definition.
        Schema schema = schemaFactory.newSchema(new File("schemas/bpel.xsd"));
        Document document = XML.read(bpel, schema);
        return deploy(document);
    }

    /**
     * Deploy a process.
     * 
     * @param bpel
     * @return the process context deployed.
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    public static ProcessContext deploy(Document bpel) throws Exception {
        try {

            // 2. Importing external definition documents
            SchemaRepository schemas = new SchemaRepository();
            ServiceRepository services = new ServiceRepository();
            importDocuments(bpel, schemas, services);

            // 3. Static Analysis on WS-BPEL
            Analyser analyser = new Analyser();
            analyser.analyse(bpel);

            // 4.Transformation into S-BPEL
            Document sbpel = SourceTransformer.transform(bpel);

            // 5. Static Analysis on S-BPEL
            analyser.analyse(sbpel);

            // 6. Parsing the document object model to a specific BPEL object
            // model.
            ProcessDescription process = ModelBinder.bind(sbpel);

            // 7. Constructing a process context object
            ProcessContext context = new ProcessContextImpl(sbpel, process, schemas, services);

            // 8. Publish exposed services.
            MessageBroker.publish(context);
            deployed.add(context);
            return context;

        } catch (Exception exception) {
            throw new Exception("Unable to deploy process description.", exception);
        }
    }

    /**
     * Undeploy a process.
     * 
     * @param context
     */
    public static void undeploy(ProcessContext context) {
        MessageBroker.unpublish(context);
        deployed.remove(context);
    }

    /**
     * Document Linking.
     * 
     * @param bpel
     * @param schemas repository to store linked definitions
     * @param services repository to store linked definitions
     * @throws WSDLException If it fails to parse the WSDL document
     * @throws URISyntaxException If the URIs are invalid
     * @throws IOException 
     * @throws SAXException 
     * @throws TransformerException 
     */
    private static void importDocuments(Document bpel, SchemaRepository schemas, ServiceRepository services) throws WSDLException, URISyntaxException, SAXException, IOException, TransformerException {
        URI base = new URI(bpel.getBaseURI());

        NodeList imports = bpel.getElementsByTagNameNS(BPELConstants.BPEL, "import");
        String importType;
        URI location;

        Element element;
        for (int index = 0; index < imports.getLength(); index++) {
            element = (Element) imports.item(index);
            importType = element.getAttribute("importType");
            location = new URI(element.getAttribute("location"));

            if (!location.isAbsolute()) {
                location = base.resolve(location);
            }

            if (BPELConstants.WSDL.equals(importType)) {
                services.add(location);
                schemas.addWSDL(location);
            }
            if (BPELConstants.XSD.equals(importType)) {
                schemas.add(location);
            }

        }
    }

    /**
     * Gets the list of deployed processes.
     * 
     * @return the list of deployed processes.
     */
    public static List<ProcessContext> getDeployedProcesses() {
        return deployed;
    }

}
