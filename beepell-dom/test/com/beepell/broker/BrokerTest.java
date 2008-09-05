package com.beepell.broker;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.net.URI;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPElement;
import javax.xml.soap.SOAPEnvelope;
import javax.xml.soap.SOAPMessage;
import javax.xml.soap.SOAPPart;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Endpoint;
import javax.xml.ws.Service;
import javax.xml.ws.ServiceMode;
import javax.xml.ws.WebServiceProvider;
import javax.xml.ws.soap.SOAPBinding;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 * 
 */
@WebServiceProvider
@ServiceMode(value = Service.Mode.MESSAGE)
public class BrokerTest implements javax.xml.ws.Provider<SOAPMessage> {

    private Broker broker;

    protected final QName portType = new QName("http://www.beepell.com/samples/", "myPortType");
    protected final String echoOperation = "echoOperation";
    protected final String notifyOperation = "notifyOperation";
    protected final QName message = new QName("http://www.beepell.com/samples/", "myMessage");
    protected final QName binding = new QName("http://www.beepell.com/samples/", "myBinding");

    protected ServiceRepository services = new ServiceRepository();

    protected URI address;
    protected Endpoint endpoint;

    protected class Listener implements QueueListener {
        /** Latest message added */
        public Message addedMessage = null;

        /** Latest message removed */
        public Message removedMessage = null;

        @Override
        public void messageAdded(Message message) {
            this.addedMessage = message;
        }

        @Override
        public void messageRemoved(Message message) {
            this.removedMessage = message;
        }

    }

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        this.broker = new Broker(this.services);
        this.endpoint = Endpoint.create(this);
        this.address = new URI("http://localhost:1000/samples/");
        this.services.add(BrokerTest.class.getResource("brokerTest.wsdl").toURI());
    }

    /**
     * @throws Exception
     */
    @After
    public void tearDown() throws Exception {
        if (this.endpoint != null)
            this.endpoint.stop();
        this.broker.unpublish(this.binding);
    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#addMessage(com.beepell.broker.Message)}.
     */
    @Test
    public final void testAddMessage() {
        MessageQueue queue = this.broker.getMessageQueue(this.portType);
        Iterator<Message> iterator;

        // Test that the queue is empty at first
        synchronized (queue) {
            iterator = queue.getIterator();
            assertFalse(iterator.hasNext());
        }

        // Add a message to the queue
        Message message = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
        this.broker.addMessage(message);

        // Test that the message is in the queue
        synchronized (queue) {
            iterator = queue.getIterator();
            assertTrue(iterator.hasNext());
            assertEquals(message, iterator.next());
        }
    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#getResponse(com.beepell.broker.Message)}.
     */
    @Test
    public final void testGetResponse() {

        this.broker.publish(this.binding);

        class Invoker implements Runnable {

            SOAPMessage responseMessage;

            @Override
            public void run() {
                try {
                    MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
                    SOAPMessage requestMessage = factory.createMessage();
                    SOAPPart soap = requestMessage.getSOAPPart();
                    SOAPEnvelope envelope = soap.getEnvelope();
                    SOAPBody body = envelope.getBody();
                    SOAPElement content = body.addBodyElement(envelope.createName(BrokerTest.this.echoOperation, "rpc", BrokerTest.this.portType
                            .getNamespaceURI()));
                    SOAPElement partAccessorElement = content.addChildElement("text");
                    partAccessorElement.setTextContent("Hello World!");

                    Service webservice = Service.create(new QName(BrokerTest.this.portType.getNamespaceURI(), "myService"));
                    QName port = new QName(BrokerTest.this.portType.getNamespaceURI(), "myPort");
                    webservice.addPort(port, SOAPBinding.SOAP11HTTP_BINDING, BrokerTest.this.address.toString());
                    Dispatch<SOAPMessage> dispatch = webservice.createDispatch(port, SOAPMessage.class, Service.Mode.MESSAGE);
                    this.responseMessage = dispatch.invoke(requestMessage);
                    
                } catch (Exception exception) {
                    exception.printStackTrace();
                    fail(exception.getLocalizedMessage());
                }
            }

        }

        try {
            Invoker invoker = new Invoker();
            Thread thread = new Thread(invoker);
            thread.start();

            // NOTE: Dependency on real time may give incorrect results
            Thread.yield();
            Thread.sleep(200);

            Iterator<Message> iterator = this.broker.getMessageQueue(this.portType).getIterator();
            assertTrue(iterator.hasNext());
            Message request = iterator.next();
            this.broker.reply(request, request);
            
            // NOTE: Dependency on real time may give incorrect results
            Thread.yield();
            Thread.sleep(50);
            
            assertNotNull(invoker.responseMessage);
            assertEquals(invoker.responseMessage.getSOAPBody().getTextContent().trim(), "Hello World!");

        } catch (Exception exception) {
            exception.printStackTrace();
            fail(exception.getLocalizedMessage());
        }

    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#invoke(com.beepell.broker.Message)}.
     */
    @Test
    public final void testInvokeRequestResonse() {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document = builder.newDocument();

            Message request = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
            String text = "Hello World!";
            request.getParts().put("text", document.createTextNode(text));

            this.endpoint.publish("http://localhost:1000/samples/");

            Future<Message> future = this.broker.invoke(request);
            assertNotNull(future);

            Message response = future.get();
            assertFalse(future.isCancelled());
            assertTrue(future.isDone());
            
            assertNotNull(response);

            assertEquals(this.portType, response.getPortTypeName());
            assertEquals(this.echoOperation, response.getOperationName());
            assertEquals(this.message, response.getDefinitionName());

            assertEquals(text, response.getParts().get("text").getTextContent());

        } catch (InterruptedException exception) {
            exception.printStackTrace();
            fail("Future.get() was interrupted.");
        } catch (ExecutionException exception) {
            fail("Future.get() has thrown an exception.");
            exception.printStackTrace();
        } catch (ParserConfigurationException exception) {
            fail("DOM Parser failed.");
            exception.printStackTrace();
        }

    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#invoke(com.beepell.broker.Message)}.
     */
    @Test
    public final void testInvokeOneWay() {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document document = builder.newDocument();

            String text = "Hello World!";
            Map<String, Node> parts = new HashMap<String, Node>();
            parts.put("text", document.createTextNode(text));
            Message request = new Message(this.message, this.portType, this.notifyOperation, this.binding, this.address);

            this.endpoint.publish(this.address.toString());

            Future<Message> future = this.broker.invoke(request);
            assertNotNull(future);

            Message response = future.get();

            assertNull(response);
            assertFalse(future.isCancelled());
            assertTrue(future.isDone());

        } catch (InterruptedException exception) {
            exception.printStackTrace();
            fail("Future.get() was interrupted.");
        } catch (ExecutionException exception) {
            exception.printStackTrace();
            fail("Future.get() has thrown an exception.");
        } catch (ParserConfigurationException exception) {
            exception.printStackTrace();
            fail("DOM Parser failed.");
        }
    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#reply(com.beepell.broker.Message, com.beepell.broker.Message)}.
     */
    @Test
    public final void testReply() {
        /* See testGetResponse */
    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#getMessageQueue(QName portTypeName)}.
     */
    @Test
    public final void testGetIterator() {
        // Add a messages to the queue
        Message message1 = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
        Message message2 = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
        Message message3 = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
        Message message4 = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
        this.broker.addMessage(message1);
        this.broker.addMessage(message2);
        this.broker.addMessage(message3);
        this.broker.addMessage(message4);

        MessageQueue queue = this.broker.getMessageQueue(this.portType);
        Iterator<Message> iterator;

        // Test that the messages are in the queue and remove the last one
        synchronized (queue) {
            iterator = queue.getIterator(this.echoOperation);
            assertTrue(iterator.hasNext());
            assertEquals(message1, iterator.next());
            assertTrue(iterator.hasNext());
            assertEquals(message2, iterator.next());
            assertTrue(iterator.hasNext());
            assertEquals(message3, iterator.next());
            assertTrue(iterator.hasNext());
            assertEquals(message4, iterator.next());
            iterator.remove();
            assertFalse(iterator.hasNext());
        }

        synchronized (queue) {
            // Test three messages remains and remove the second
            iterator = queue.getIterator(this.echoOperation);
            assertTrue(iterator.hasNext());
            assertEquals(message1, iterator.next());
            assertTrue(iterator.hasNext());
            assertEquals(message2, iterator.next());
            iterator.remove();
            assertTrue(iterator.hasNext());
            assertEquals(message3, iterator.next());
            assertFalse(iterator.hasNext());
        }

        synchronized (queue) {
            // Test two messages remains
            iterator = queue.getIterator(this.echoOperation);
            assertTrue(iterator.hasNext());
            assertEquals(message1, iterator.next());
            assertTrue(iterator.hasNext());
            assertEquals(message3, iterator.next());
            assertFalse(iterator.hasNext());
        }
    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#addListener(com.beepell.broker.QueueListener, javax.xml.namespace.QName)}.
     */
    @Test
    public final void testAddListener() {
        Listener listener = new Listener();

        assertEquals(null, listener.addedMessage);

        this.broker.addListener(listener, this.portType);

        // Add a message to the queue
        Message message = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
        this.broker.addMessage(message);

        assertEquals(message, listener.addedMessage);
        assertEquals(null, listener.removedMessage);

        MessageQueue queue = this.broker.getMessageQueue(this.portType);
        synchronized (queue) {
            Iterator<Message> iterator = queue.getIterator(this.echoOperation);
            iterator.next();
            iterator.remove();
            assertEquals(message, listener.removedMessage);
        }
    }

    /**
     * Test method for
     * {@link com.beepell.broker.Broker#removeListener(com.beepell.broker.QueueListener, javax.xml.namespace.QName)}.
     */
    @Test
    public final void testRemoveListener() {
        Listener listener = new Listener();

        this.broker.addListener(listener, this.portType);
        this.broker.removeListener(listener, this.portType);

        // Add a message to the queue
        Message message = new Message(this.message, this.portType, this.echoOperation, this.binding, this.address);
        this.broker.addMessage(message);

        MessageQueue queue = this.broker.getMessageQueue(this.portType);
        synchronized (queue) {
            Iterator<Message> iterator = queue.getIterator(this.echoOperation);
            iterator.next();
            iterator.remove();
        }

        assertEquals(null, listener.addedMessage);
        assertEquals(null, listener.removedMessage);
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.ws.Provider#invoke(java.lang.Object)
     */
    @Override
    public SOAPMessage invoke(SOAPMessage request) {
        try {
            request.writeTo(System.out);
            System.out.println();
        } catch (Exception exception) {
            exception.printStackTrace();
        }

        try {
            if (request.getSOAPBody().getFirstChild().getLocalName().equals("echoOperation")) {
                String text = request.getSOAPBody().getFirstChild().getTextContent();
                String message = "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\"><SOAP-ENV:Header/><SOAP-ENV:Body>" + "<rpc:"
                        + this.echoOperation + "Response xmlns:rpc=\"" + this.portType.getNamespaceURI() + "\">" + "<text>" + text + "</text>" + "</rpc:"
                        + this.echoOperation + "Response>" + "</SOAP-ENV:Body></SOAP-ENV:Envelope>";
                MessageFactory factory = MessageFactory.newInstance(SOAPConstants.SOAP_1_1_PROTOCOL);
                SOAPMessage response = factory.createMessage(null, new ByteArrayInputStream(message.getBytes()));
                return response;
            }

            return null;

        } catch (Exception exception) {
            exception.printStackTrace();
            fail("An exception occured in the service.");
            return null;
        }

    }

}
