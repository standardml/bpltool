package com.beepell.execution.bpel;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.beepell.BPELConstants;

/**
 * @author Tim Hallwyl
 * 
 */
public class ProcessInstance {

    private final Document document;

    private final List<Element> queue = new ArrayList<Element>();

    private final List<ChangeListener> listeners = new ArrayList<ChangeListener>();

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

        if (this.queue.isEmpty())
            this.enqueueActivities(this.document.getDocumentElement());

        if (this.queue.isEmpty())
            return;

        int i = 0;
        Element next = this.queue.get(i);
        while (this.isSynchronizing(next) && i < this.queue.size())
            next = this.queue.get(++i);

        if (next == null)
            return;

        Context context = new Context(next);

        if (context.evaluateJoinCondition())
            Semantics.rewrite(next, context);
        else if (next.getAttribute("suppressJoinFailure").equals("yes")) {
            Semantics.skip(next, context); // TODO: check with link semantics!
        } else {
            // TODO Start fault handling: joinFailure
        }
        
        this.fireChangeEvent(new ChangeEvent(this));

    }

    /**
     * Adds elements to the queue that needs to be executed.
     * 
     * @param element
     */
    public void enqueueActivities(Element element) {

        if (!element.getNamespaceURI().equals(BPELConstants.BPEL))
            return;

        final String localName = element.getLocalName();
        if (localName.equals("sequence")) {

            // Empty Sequence
            if (!Utils.hasChildActivities(element)) {
                this.queue.add(element);
                return;
            }

            // Non-empty sequence with incoming links
            if (Utils.hasTargets(element)) {
                this.queue.add(element);
                return;
            }

            // Non-empty sequence without incoming links
            enqueueActivities(Utils.getFirstChildActivity(element));
            return;
        }

        if (localName.equals("if")) {
            this.queue.add(element);
            return;
        }

        if (localName.equals("while")) {
            this.queue.add(element);
            return;
        }

        if (localName.equals("repeatUntil")) {
            this.queue.add(element);
            return;
        }

        if (localName.equals("flow")) {
            // Empty Flow
            if (!Utils.hasChildActivities(element))
                this.queue.add(element);

            // Non-empty Flow with incoming links
            if (Utils.hasTargets(element)) {
                this.queue.add(element);
                return;
            }

            // Non-empty Flow without incoming links
            Node node = element.getFirstChild();
            while (node != null) {
                if (node instanceof Element && Utils.isActivity((Element) node))
                    enqueueActivities(element);
                node = node.getNextSibling();
            }

        }

        if (localName.equals("forEach")) {
            this.queue.add(element);
            return;
        }

        if (localName.equals("scope")) {
            if ("running".equals(element.getAttribute("state")))
                enqueueActivities(Utils.getFirstChildActivity(element));
            else
                this.queue.add(element);
            return;
        }

        // All basic activities add them self.
        this.queue.add(element);

    }

    /**
     * Checks if the activity is synchronizing; that is, if it is waiting for
     * some incoming link to be determined.
     * 
     * @param element
     * @return true if the activity is waiting for one or incoming links, false
     *         if it is ready to be executed.
     */
    private boolean isSynchronizing(final Element element) {

        List<String> targets = Utils.getTargetsNames(element);
        if (targets == null)
            return false;

        Context context = new Context(element);
        for (String linkName : targets) {
            if (context.getLinkState(linkName) == null)
                return true;
        }

        return false;

    }

    private void fireChangeEvent(ChangeEvent event) {
        for (ChangeListener listener : this.listeners) {
            listener.stateChanged(event);
        }
    }

    /**
     * Add a ChangeListener to be notified on changes in the process instance.
     * 
     * @param listener The listener to be notified.
     */
    public void add(ChangeListener listener) {
        this.listeners.add(listener);
    }

    /**
     * Remove a listener from the subscription list.
     * 
     * @param listener
     */
    public void remove(ChangeListener listener) {
        this.listeners.remove(listener);
    }

}
