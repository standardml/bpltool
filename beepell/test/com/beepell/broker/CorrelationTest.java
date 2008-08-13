package com.beepell.broker;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.broker.MessageBroker;
import com.beepell.exceptions.CorrelationViolation;
import com.beepell.execution.CorrelationSet;
import com.beepell.model.CorrelationWithPattern;
import com.beepell.model.InitiateEnumeration;
import com.beepell.model.PatternEnumeration;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.util.XML;
import com.beepell.variable.MessageVariable;

/**
 * 
 * @author Tim Hallwyl
 *
 */
public class CorrelationTest extends TestCase {

    /**
     * 
     *
     */
    public final void testCorrelationSet() {
        try {
            URI wsdl = CorrelationTest.class.getResource("correlation.wsdl").toURI();

            ServiceRepository services = new ServiceRepository();
            services.add(wsdl);

            SchemaRepository schemas = new SchemaRepository();
            schemas.addWSDL(wsdl);
            
            CorrelationContext context = new CorrelationContext(schemas, services);
            List<QName> properties;
            CorrelationSet correlationSet;
            
            properties = new ArrayList<QName>(3);
            properties.add(new QName("http://tim.hallwyl.dk/", "customerId"));
            properties.add(new QName("http://tim.hallwyl.dk/", "customerFirstname"));
            properties.add(new QName("http://tim.hallwyl.dk/", "customerLastname"));            
            correlationSet = new CorrelationSet(services, "customer", properties);
            context.addCorrelationSet(correlationSet);
            
            properties = new ArrayList<QName>(1);
            properties.add(new QName("http://tim.hallwyl.dk/", "quoteId"));
            correlationSet = new CorrelationSet(services, "quote", properties);
            context.addCorrelationSet(correlationSet);
            
            List<CorrelationWithPattern> correlations = new ArrayList<CorrelationWithPattern>(2);
            
            CorrelationWithPattern correlation;
            correlation = new CorrelationWithPattern();
            correlation.setInitiate(InitiateEnumeration.YES);
            correlation.setSet("customer");
            correlation.setPattern(PatternEnumeration.REQUEST);
            correlations.add(correlation);
            
            correlation = new CorrelationWithPattern();
            correlation.setInitiate(InitiateEnumeration.YES);
            correlation.setSet("quote");
            correlation.setPattern(PatternEnumeration.RESPONSE);
            correlations.add(correlation); 

            MessageVariable inputVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "InsuranceRequest"), "request", schemas, services);

            context.addVariable(inputVariable);
            MessageVariable outputVariable = new MessageVariable(new QName("http://tim.hallwyl.dk/", "InsuranceResponse"), "response", schemas, services);
            context.addVariable(outputVariable);            
            
            inputVariable.initialize();

            try {
                MessageBroker.invokeRequestCorrelation(correlations, "request", context);
                fail("Expected an correlation violation");
            } catch (CorrelationViolation exception) { /* correlation violation expected */ }

            Node personNode = inputVariable.getValue("driver");
            String personXML = "<driver xmlns:ins=\"http://tim.hallwyl.dk/insurance\"><ins:id>\t42\n</ins:id><ins:firstname>    Peter    \n\n</ins:firstname><ins:lastname>\t\tUlrich</ins:lastname></driver>";
            Document personValue = (Document) XML.toNode(personXML);
            personValue.renameNode(personValue.getDocumentElement(), personNode.getNamespaceURI(), personNode.getLocalName());
            Node personCopy = personNode.getOwnerDocument().importNode(personValue.getDocumentElement(), true);
            personNode.getOwnerDocument().replaceChild(personCopy, personNode);
            personNode.getOwnerDocument().normalizeDocument();
            
            try {
            
                MessageBroker.invokeRequestCorrelation(correlations, "request", context);
                CorrelationSet customer = context.getCorrelationSet("customer");
                assertEquals("42", customer.getCorrelationValue(new QName("http://tim.hallwyl.dk/", "customerId")));
                assertEquals("Peter", customer.getCorrelationValue(new QName("http://tim.hallwyl.dk/", "customerFirstname")));
                assertEquals("Ulrich", customer.getCorrelationValue(new QName("http://tim.hallwyl.dk/", "customerLastname")));

            } catch (Exception exception) { 
                exception.printStackTrace();
                fail("Unexpected exception"); 
            }
            
            
        } catch (Exception exception) {
            exception.printStackTrace();
            fail("Caught unexpected exception");
        }

    }
}
