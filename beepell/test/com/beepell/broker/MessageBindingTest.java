package com.beepell.broker;

import java.io.File;
import java.net.URI;

import javax.xml.namespace.QName;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPMessage;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Endpoint;
import javax.xml.ws.Provider;
import javax.xml.ws.Service;
import javax.xml.ws.ServiceMode;
import javax.xml.ws.WebServiceProvider;
import javax.xml.ws.soap.SOAPBinding;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.beepell.broker.MessageBinding;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.util.XML;
import com.beepell.variable.MessageVariable;

/**
 * Testing the MessageBinding class.
 * <p>
 * <b>implementation note:</b> these tests works as follows:
 * <ul>
 * <li>A WSDL is read into schema and service repositories
 * <li>A requestVariable (message type) is created for the request message
 * <li>Sample data is copies into the requestVariable
 * <li>A responseVariable (message type) is created for the response message
 * <li>Sample data is copies into the responseVariable
 * <li>requestVariable is bound to an request SOAPMessage
 * <li>the request SOAPMessage is tested
 * <li>A SOAP server is started
 * <li>A SOAP client is created, an invoked with the request SOAPMessage
 * <li>though the invoke-method the responseVariable is bound to an response
 * message in reply
 * <li>the response SOAPMessage is tested
 * <li>bind the response SOAPMessage to a responseVariable
 * <li>test the responseVariable
 * </ul> *
 * 
 * @author Tim Hallwyl
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class MessageBindingTest extends TestCase implements Provider<SOAPMessage> {

    private MessageVariable responseVariable;
    
    /**
     * Testing a document style binding.
     *
     */
    public final void testElementDocumentBinding() {

        try {
        // Prepare repositories
        File file = new File("./test/com/beepell/broker/directory.wsdl");
        URI wsdl = file.toURI();

        ServiceRepository services = new ServiceRepository();
        services.add(wsdl);

        SchemaRepository schemas = new SchemaRepository();
        schemas.addWSDL(wsdl);
     
        // Prepare Request Message Variable
        MessageVariable requestVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "PhoneNumberRequest"), "request", schemas, services);
        requestVariable.initialize();

        Node personNode = requestVariable.getValue("name");
        String personXML = "<dir:person xmlns:dir=\"http://tim.hallwyl.dk/directory\"><dir:firstname>Tim</dir:firstname><dir:lastname>Hallwyl</dir:lastname></dir:person>";
        Document personValue = (Document) XML.toNode(personXML);
        personValue.renameNode(personValue.getDocumentElement(), personNode.getNamespaceURI(), personNode.getLocalName());
        Node personCopy = personNode.getOwnerDocument().importNode(personValue.getDocumentElement(), true);
        personNode.getOwnerDocument().replaceChild(personCopy, personNode);
        //System.out.println(XML.toString(requestVariable.getValue("name").getOwnerDocument()));
        
        // Prepare Response Message Variable
        responseVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "PhoneNumberResponse"), "response", schemas, services);
        responseVariable.initialize();
        Node phoneNode = responseVariable.getValue("phone");
        String phoneXML = "<dir:phone xmlns:dir=\"http://tim.hallwyl.dk/directory\">+4560887050</dir:phone>";
        Document phoneValue = (Document) XML.toNode(phoneXML);
        phoneValue.renameNode(phoneValue.getDocumentElement(), phoneNode.getNamespaceURI(), phoneNode.getLocalName());
        Node phoneCopy = phoneNode.getOwnerDocument().importNode(phoneValue.getDocumentElement(), true);
        phoneNode.getOwnerDocument().replaceChild(phoneCopy, phoneNode);
        phoneNode.getOwnerDocument().normalizeDocument();
        //System.out.println(XML.toString(responseVariable.getValue("phone").getOwnerDocument()));

        // Binding to request message
        String soapAction = "http://tim.hallwyl.dk/PhoneNumberOperation";
        QName operation = new QName("PhoneNumberOperation");

        SOAPMessage requestMessage = MessageBinding.bind(requestVariable, "document", operation, MessageBinding.REQUEST);
        requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
        requestMessage.writeTo(System.out);
        System.out.println("");

        // Test request message
        Element content = (Element) requestMessage.getSOAPBody().getElementsByTagNameNS("http://tim.hallwyl.dk/directory", "person").item(0);
        assertNotNull(content);
        assertEquals(content.getFirstChild().getNamespaceURI(), "http://tim.hallwyl.dk/directory");
        assertEquals(content.getFirstChild().getLocalName(), "firstname");
        assertEquals(content.getFirstChild().getTextContent(), "Tim");
        
        assertEquals(content.getLastChild().getNamespaceURI(), "http://tim.hallwyl.dk/directory");
        assertEquals(content.getLastChild().getLocalName(), "lastname");
        assertEquals(content.getLastChild().getTextContent(), "Hallwyl");
        

        // Setting up server and client
        QName service = new QName("http://tim.hallwyl.dk/", "DirectoryService");
        QName port = new QName("http://tim.hallwyl.dk/", "DirectoryPort");
        String endpointAddress = "http://localhost:9000/samples/services/Directory";

        // Start server
        Endpoint endpoint = Endpoint.create(this);
        endpoint.publish(endpointAddress);

        // Create client
        Service webservice = Service.create(service);
        webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
        Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
        SOAPMessage responseMessage = dispatch.invoke(requestMessage);

        responseMessage.writeTo(System.out);
        System.out.println("");
        
        // Bind to variable
        responseVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "PhoneNumberResponse"), "response", schemas, services);
        MessageBinding.bind(responseMessage, responseVariable, "document", new QName("PhoneNumberOperation"), MessageBinding.RESPONSE);
        //System.out.println(XML.toString(responseVariable.getValue("phone").getOwnerDocument()));
        assertEquals("http://tim.hallwyl.dk/directory", responseVariable.getValue("phone").getNamespaceURI());
        assertEquals("phone", responseVariable.getValue("phone").getLocalName());
        assertEquals("+4560887050", responseVariable.getValue("phone").getTextContent());
        
        
        } catch (Exception e) {
            e.printStackTrace();
            fail("Unexpected exception.");
        }
    }
    
    /**
     * Testing a RPC/literal service using only simple typed parts.
     */
    public final void testSimpleTypedRPCBinding() {

        try {

            // Prepare repositories
            File file = new File("./test/com/beepell/broker/currencyConvertor.wsdl");
            URI wsdl = file.toURI();
            
            ServiceRepository services = new ServiceRepository();
            services.add(wsdl);

            SchemaRepository schemas = new SchemaRepository();
            schemas.addWSDL(wsdl);

            // Prepare Request Message Variable
            MessageVariable requestVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "ConversionRateRequest"), "myVariable", schemas, services);
            requestVariable.initialize();

            Text from = (Text) requestVariable.getValue("from");
            from.setData("DKK");

            Text to = (Text) requestVariable.getValue("to");
            to.setData("USD");

            // Prepare Response Message Variable
            responseVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "ConversionRateResponse"), "response", schemas, services);
            responseVariable.initialize();
            Text rate = (Text) responseVariable.getValue("rate");
            rate.setData("42.424");

            // Binding info
            String soapAction = "http://tim.hallwyl.dk/ConversionRateOperation";
            String style = "rpc";
            QName operation = new QName("http://tim.hallwyl.dk/request", "ConversionRateOperation");

            SOAPMessage requestMessage = MessageBinding.bind(requestVariable, style, operation, MessageBinding.REQUEST);
            requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
            requestMessage.writeTo(System.out);
            System.out.println("");

            // Test request
            Element wrapperElement = (Element) requestMessage.getSOAPBody().getElementsByTagNameNS("http://tim.hallwyl.dk/request", "ConversionRateOperation").item(0);
            assertNotNull(wrapperElement);

            Element partAccessor;
            partAccessor = (Element) wrapperElement.getElementsByTagName("from").item(0);
            assertNotNull(partAccessor);
            assertNull(partAccessor.getNamespaceURI());
            assertEquals("DKK", partAccessor.getTextContent());

            partAccessor = (Element) wrapperElement.getElementsByTagName("to").item(0);
            assertNotNull(partAccessor);
            assertNull(partAccessor.getNamespaceURI());
            assertEquals("USD", partAccessor.getTextContent());

            // Setting up server and client
            QName service = new QName("http://tim.hallwyl.dk/", "CurrencyConvertorService");
            QName port = new QName("http://tim.hallwyl.dk/", "CurrencyConvertorPort");
            String endpointAddress = "http://localhost:9000/samples/services/CurrencyConvertor";

            // Start server
            Endpoint endpoint = Endpoint.create(this);
            endpoint.publish(endpointAddress);

            // Create client
            Service webservice = Service.create(service);
            webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
            Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
            SOAPMessage response = dispatch.invoke(requestMessage);

            response.writeTo(System.out);
            System.out.println("");

            // Test response message.
            wrapperElement = (Element) response.getSOAPBody().getElementsByTagNameNS("http://tim.hallwyl.dk/response", "ConversionRateOperationResponse").item(0);
            assertNotNull(wrapperElement);

            partAccessor = (Element) wrapperElement.getElementsByTagName("rate").item(0);
            assertNotNull(partAccessor);

            assertEquals("42.424", partAccessor.getTextContent());

            // Bind response message to variable
            responseVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "ConversionRateResponse"), "response", schemas, services);
            MessageBinding.bind(response, responseVariable, "rpc", new QName("http://tim.hallwyl.dk/response", "ConversionRateOperation"), MessageBinding.RESPONSE);
            assertEquals("42.424", responseVariable.getValue("rate").getTextContent());

        } catch (Exception e) {
            e.printStackTrace();
            fail("Unexpected exception.");
        }
    }

    /**
     * Testing message binding with complex types.
     */
    public final void testComplexTypedRPCBinding() {
        try {

            // Prepare repositories
            File file = new File("./test/com/beepell/broker/insurance.wsdl");
            URI wsdl = file.toURI();
            
            ServiceRepository services = new ServiceRepository();
            services.add(wsdl);

            SchemaRepository schemas = new SchemaRepository();
            schemas.addWSDL(wsdl);

            // Prepare Request Message Variable
            MessageVariable requestVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "InsuranceRequest"), "request", schemas, services);
            requestVariable.initialize();

            Node driver = requestVariable.getValue("driver");
            Document driverValue = (Document) XML.toNode("<driver xmlns:ins=\"http://tim.hallwyl.dk/insurance\"><ins:firstname>Peter</ins:firstname><ins:lastname>Ulrich</ins:lastname></driver>");
            driverValue.renameNode(driverValue.getDocumentElement(), driver.getNamespaceURI(), driver.getLocalName());
            Node driverCopy = driver.getOwnerDocument().importNode(driverValue.getDocumentElement(), true);
            driver.getOwnerDocument().replaceChild(driverCopy, driver);

            Node cars = requestVariable.getValue("cars");
            // System.out.println(XML.toString(cars));
            String carsXML = "<cars xmlns:ins=\"http://tim.hallwyl.dk/insurance\"><ins:car><ins:manufacturer>Opel</ins:manufacturer><ins:model>Vectra</ins:model><ins:year>1992</ins:year></ins:car>" + "<ins:car><ins:manufacturer>Fiat</ins:manufacturer><ins:model>127</ins:model><ins:year>1981</ins:year></ins:car></cars>";

            Document carsValue = (Document) XML.toNode(carsXML);
            carsValue.renameNode(carsValue.getDocumentElement(), cars.getNamespaceURI(), cars.getLocalName());
            // System.out.println(XML.toString(carsValue));

            Node carsCopy = cars.getOwnerDocument().importNode(carsValue.getDocumentElement(), true);
            cars.getOwnerDocument().replaceChild(carsCopy, cars);

            // Prepare resonse variable
            responseVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "InsuranceResponse"), "response", schemas, services);
            responseVariable.initialize();
            Node responseNode = responseVariable.getValue("premium");
            String responseXML = "<premium xmlns:ins=\"http://tim.hallwyl.dk/insurance\"><ins:monthly>546.00</ins:monthly><ins:bonus>200.00</ins:bonus></premium>";
            Document responseValue = (Document) XML.toNode(responseXML);
            responseValue.renameNode(responseValue.getDocumentElement(), responseNode.getNamespaceURI(), responseNode.getLocalName());
            Node responseCopy = responseNode.getOwnerDocument().importNode(responseValue.getDocumentElement(), true);
            responseNode.getOwnerDocument().replaceChild(responseCopy, responseNode);

            // Binding info
            String soapAction = "http://tim.hallwyl.dk/InsuranceOperation";
            QName operation = new QName("http://tim.hallwyl.dk/request", "InsuranceOperation");

            SOAPMessage requestMessage = MessageBinding.bind(requestVariable, "rpc", operation, MessageBinding.REQUEST);
            requestMessage.getMimeHeaders().addHeader("SOAPAction", "\"" + soapAction + "\"");
            requestMessage.writeTo(System.out);
            System.out.println("");

            // Test request message
            SOAPBody body = requestMessage.getSOAPBody();
            Element wrapperElement = (Element) body.getChildElements(new QName("http://tim.hallwyl.dk/request", "InsuranceOperation")).next();

            NodeList partAccessorElements = wrapperElement.getChildNodes();
            Element driverAccessor = (Element) partAccessorElements.item(0);
            assertNull(driverAccessor.getNamespaceURI());
            assertNull(driverAccessor.getPrefix());

            assertEquals("firstname", driverAccessor.getFirstChild().getLocalName());
            assertEquals("http://tim.hallwyl.dk/insurance", driverAccessor.getFirstChild().getNamespaceURI());

            Element carsAccessor = (Element) partAccessorElements.item(1);
            assertNull(carsAccessor.getNamespaceURI());
            assertNull(carsAccessor.getPrefix());
            assertEquals("car", carsAccessor.getFirstChild().getLocalName());
            assertEquals("http://tim.hallwyl.dk/insurance", carsAccessor.getFirstChild().getNamespaceURI());

            QName service = new QName("http://tim.hallwyl.dk/", "InsuranceService");
            QName port = new QName("http://tim.hallwyl.dk/", "InsurancePort");
            String endpointAddress = "http://localhost:9000/samples/services/Insurance";

            // Start server
            Endpoint endpoint = Endpoint.create(this);
            endpoint.publish(endpointAddress);

            // Create client
            Service webservice = Service.create(service);
            webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, endpointAddress);
            Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
            SOAPMessage responseMessage = dispatch.invoke(requestMessage);

            // Test response
            body = responseMessage.getSOAPBody();
            wrapperElement = (Element) body.getChildElements(new QName("http://tim.hallwyl.dk/response", "InsuranceOperationResponse")).next();
            partAccessorElements = wrapperElement.getChildNodes();
            Element premiumAccessor = (Element) partAccessorElements.item(0);
            assertNull(premiumAccessor.getNamespaceURI());
            assertNull(premiumAccessor.getPrefix());
            Element monthly = (Element) premiumAccessor.getElementsByTagNameNS("http://tim.hallwyl.dk/insurance", "monthly").item(0);
            assertEquals("546.00", monthly.getTextContent());
            Element bonus = (Element) premiumAccessor.getElementsByTagNameNS("http://tim.hallwyl.dk/insurance", "bonus").item(0);
            assertEquals("200.00", bonus.getTextContent());

            // Bind response message to variable
            responseVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "InsuranceResponse"), "response", schemas, services);
            MessageBinding.bind(responseMessage, responseVariable, "rpc", new QName("http://tim.hallwyl.dk/response", "InsuranceOperation"), MessageBinding.RESPONSE);
            assertEquals("546.00", responseVariable.getValue("premium").getFirstChild().getTextContent());
            assertEquals("200.00", responseVariable.getValue("premium").getLastChild().getTextContent());
            responseMessage.writeTo(System.out);
            System.out.print(XML.toString(responseVariable.getValue("premium").getOwnerDocument()));

        } catch (Exception e) {
            e.printStackTrace();
            fail("Unexpected exception.");
        }

    }

    public SOAPMessage invoke(SOAPMessage message) {

        try {
           
            String style = "rpc";
            QName operation = null;
            
            if (message.getSOAPBody().getElementsByTagNameNS("http://tim.hallwyl.dk/directory", "person").getLength() > 0) {
                operation = new QName("PhoneNumberOperation");
                style = "document";
                System.out.println("PhoneNumberOperation invoked");
            }                
                
            if (message.getSOAPBody().getElementsByTagNameNS("http://tim.hallwyl.dk/request", "ConversionRateOperation").getLength() > 0) {
                operation = new QName("http://tim.hallwyl.dk/response", "ConversionRateOperation");
                style = "rpc";
                System.out.println("ConversionRateOperation invoked");
            }

            if (message.getSOAPBody().getElementsByTagNameNS("http://tim.hallwyl.dk/request", "InsuranceOperation").getLength() > 0) {
                operation = new QName("http://tim.hallwyl.dk/response", "InsuranceOperation");
                style = "rpc";
                System.out.println("InsuranceOperation invoked");
            }

            
            
            return MessageBinding.bind(responseVariable, style, operation, MessageBinding.RESPONSE);

        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }

    }
}
