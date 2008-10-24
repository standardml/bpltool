package com.beepell.execution.bpel;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.beepell.BPELConstants;
import com.beepell.exceptions.BPELFault;

/**
 * @author Tim Hallwyl
 * 
 */
public class Semantics {

    /**
     * Rewrites the instance tree according to the semantics of the activity.
     * 
     * @param activity The activity to execute.
     * @param context The activity context.
     */
    public static void rewrite(final Element activity, final Context context) {
        activity.setAttributeNS(BPELConstants.BPI, "state", "running");
        
        try {

            if (activity.getLocalName().equals("if")) {
                rewriteIf(activity, context);
                return;
            }

        } catch (BPELFault fault) {
            // TODO: Start fault handlling
        }

        throw new IllegalArgumentException("Activity " + activity.getLocalName() + " is not supported.");
    }

    private static void rewriteIf(final Element activity, final Context context) throws BPELFault {

        // MAIN CONDITION
        Element selected = null;
        Element ifActivity = Utils.getFirstChildActivity(activity);
        Element elseElement = Utils.getChildElement("else", activity);
        Element elseActivity = null;
        if (elseElement != null)
            elseActivity = Utils.getFirstChildActivity(elseElement);

        Element condition = Utils.getChildElement("condition", activity);
        boolean result = context.evaluateBoolean(condition.getTextContent());

        if (result) {
            selected = ifActivity;
            skip(elseActivity, new Context(elseActivity));

        } else {
            selected = elseActivity;
            skip(ifActivity, new Context(ifActivity));
        }

        // No activity selected
        if (selected == null) {
            complete(activity, context);

        } else {
            Document document = activity.getOwnerDocument();
            Element sequence = document.createElementNS(BPELConstants.BPI, "sequence");
            sequence.setAttributeNS(BPELConstants.BPI, "state", "running");
            context.inheritOutgoingLinks(sequence);
            selected.getParentNode().removeChild(selected);
            sequence.appendChild(selected);
            activity.getParentNode().replaceChild(sequence, activity);
        }

        return;
    }

    private static void complete(Element activity, Context context) {
        activity.setAttributeNS(BPELConstants.BPI, "state", "completed");
        
        // Set outgoing links (sources), if any.
        Element sources = Utils.getChildElement(new QName(BPELConstants.BPI, "sources"), activity);
        if (sources != null) {
            boolean state = false;
            NodeList sourceElements = sources.getElementsByTagNameNS(BPELConstants.BPI, "source");
            int i = 0;
            try {
                Element source;
                for (; i < sourceElements.getLength(); i++) {
                    source = (Element) sourceElements.item(i);

                    String transitionCondition = source.getElementsByTagNameNS(BPELConstants.BPI, "transitionCondition").item(0).getTextContent();
                    state = context.evaluateBoolean(transitionCondition);
                    context.setLinkState(source.getAttribute("linkName"), state);

                }
            } catch (Exception exception) {

                // Only set inter-scope links to 'false'
                Element source;
                String linkName;
                for (; i < sourceElements.getLength(); i++) {
                    source = (Element) sourceElements.item(i);
                    linkName = source.getAttribute("linkName");

                    if (context.isInterScope(linkName))
                        context.setLinkState(linkName, false);
                }

                // TODO Start faulthandling

            }
        }

        // Remove the completed activity from instance tree
        activity.getParentNode().removeChild(activity);

    }

    /**
     * Note: This method does not remove the skipped activity, as this is/will
     * be done as part of the semantics of the enclosing structured activity.
     * 
     * @param activity The skipped activity.
     * @param context The skipped activity's context.
     */
    private static void skip(Element activity, Context context) {

        activity.setAttributeNS(BPELConstants.BPI, "state", "skipped");
        
        // TODO implement skip link semantics
        

    }
}
