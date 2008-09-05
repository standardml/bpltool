package com.beepell.broker;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Future;

import javax.wsdl.Port;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.soap.SOAPAddress;
import javax.wsdl.extensions.soap.SOAPBinding;
import javax.xml.namespace.QName;
import javax.xml.ws.Endpoint;

import com.beepell.repository.ServiceRepository;

/**
 * <p>
 * The responsibility of the Broker is to mediate between the Providers and the
 * clients (process instances or engine).
 * 
 * @author Tim Hallwyl
 * 
 */
public class Broker {

    private Message request = null;
    private Message response = null;

    private ServiceRepository services;

    /**
     * The set of published Endpoints stored by they binding name.
     */
    private Map<QName, Endpoint> endpoints = new HashMap<QName, Endpoint>();

    /**
     * The set of interface (port type) queues. The key is the QName of the
     * interface / port type.
     */
    private Map<QName, MessageQueue> queues = new Hashtable<QName, MessageQueue>();

    /**
     * Create a new message Broker.
     * 
     * @param services
     */
    public Broker(ServiceRepository services) {
        this.services = services;
    }

    /**
     * Publish a port type using the specified binding. The address is selected
     * automatically.
     * 
     * @param binding
     */
    @SuppressWarnings("unchecked")
    public void publish(QName binding) {
        SOAPProvider provider = new SOAPProvider(this, binding, this.services);
        Endpoint endpoint = Endpoint.create(provider);

        List<ExtensibilityElement> extenstions = this.services.getBinding(binding).getExtensibilityElements();
        SOAPBinding soapBinding = ServiceRepository.getExtension(extenstions, SOAPBinding.class);
        if (soapBinding == null) 
            throw new IllegalArgumentException("The " + binding + " does not have a SOAP binding.");

        // We assume to find a SOAPAddress in the port.
        Port port = this.services.getMatchingPort(binding);
        
        if (port == null)
            throw new IllegalArgumentException("The " + binding + " port does not have a matching port.");
            
        SOAPAddress address = ((SOAPAddress) port.getExtensibilityElements().get(0));
        if (address == null)
            throw new IllegalArgumentException("The " + port.getName() + " port does not have a SOAP address.");

        endpoint.publish(address.getLocationURI());
        this.endpoints.put(binding, endpoint);
    }

    /**
     * Un-publish the binding.
     * 
     * @param binding
     */
    public void unpublish(QName binding) {
        Endpoint endpoint = this.endpoints.get(binding);
        if (endpoint != null)
            endpoint.stop();
    }

    /**
     * Gets the message queue for the port type. This will create a queue if one
     * is not already created. This method is thread safe.
     * 
     * @param portTypeName
     * @return The message queue for the port type
     */
    public MessageQueue getMessageQueue(QName portTypeName) {
        MessageQueue queue = this.queues.get(portTypeName);
        if (queue == null) {
            synchronized (this.queues) {
                queue = this.queues.get(portTypeName);
                if (queue == null) {
                    queue = new MessageQueue();
                    this.queues.put(portTypeName, queue);
                }
            }
        }
        return queue;
    }

    /**
     * Adds a request message to the brokers message queue, but does not expect
     * a response message. This is for one-way invocations.
     * 
     * @param message The incoming message to be enqueued.
     */
    public void addMessage(Message message) {

        MessageQueue queue = getMessageQueue(message.getPortTypeName());
        queue.add(message);

    }

    /**
     * Adds a request message to the brokers message queue and returns the
     * corresponding response message. This is for request-response invocations.
     * 
     * @param request
     * @return The response message.
     * @throws Exception
     */
    public synchronized Message getResponse(Message request) throws Exception {
        this.addMessage(request);

        while (this.request != request) {
            wait();
        }

        return this.response;
    }

    /**
     * Invokes a service by sending the request message.
     * 
     * @param request The message to send.
     * @return The response message or null for one-way invocations.
     */
    public Future<Message> invoke(Message request) {

        /*
         * Note: We assume SOAP binding, but could in the future choose some
         * other protocol binding based on the message´s binding definition.
         */

        return new SOAPInvokeHandler(request, this.services);
    }

    /**
     * Sends the response message in reply to a request message identified by
     * portType, operation and optionally a channel identifier.
     * 
     * @param request The message received, the the response is a reply to.
     * @param response The message to be send in reply to the request message.
     */
    public synchronized void reply(Message request, Message response) {
        this.response = response;
        this.request = request;
        this.notifyAll();
    }

    /**
     * Adds a listener to be notified on changes in the message queue for the
     * specified port type.
     * 
     * @param listener The listener to be added.
     * @param portType The port type to listen on.
     */
    public void addListener(QueueListener listener, QName portType) {
        MessageQueue queue = getMessageQueue(portType);
        queue.addListener(listener);
    }

    /**
     * Removes a listener on the specified port type.
     * 
     * @param listener The listener to be removed.
     * @param portType The port type to listen on.
     */
    public void removeListener(QueueListener listener, QName portType) {
        MessageQueue queue = getMessageQueue(portType);
        queue.removeListener(listener);
    }

    // TODO add a set of general listener method to listen for all messages.

}
