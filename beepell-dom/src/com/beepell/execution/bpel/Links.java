package com.beepell.execution.bpel;

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.beepell.BPELConstants;

/**
 * @author Tim Hallwyl
 * 
 */
public class Links {

    /**
     * Sets link dependencies.
     * <p>
     * When an activity is skipped, all links leaving the sub-tree rooted at
     * that activity must be set false, but not before incoming links that the
     * link is depending on are determined. (Yes, this is tricky business)
     * <p>
     * This method will, with the help of a bunch of private methods, find links
     * leaving the sub-tree and for each of these link it will: 1) Find links
     * entering the sub-tree that the link depends on (is connected backwards
     * to) and 2) set name these incoming links as dependencies in the
     * declaration of the link (using Context.setDependencies).
     * <p>
     * Note: Context.setDependencies will check if the dependencies are already
     * determined. If a dependency is determined it is not added as a
     * dependency. If there is no dependencies at all, Context.setDependencies
     * will set the link to false.
     * <p>
     * See WS-BPEL section 11.6.2.
     * 
     * @param rootNode The root of the sub-tree to be skipped.
     * @param rootContext The context for the rootNode.
     */
    public static void setLinkDependencies(final Element rootNode, final Context rootContext) {

        Links.addLinks(rootNode, new ArrayList<String>(), rootNode, rootContext);

    }

    private static void addLinks(Element activity, List<String> internalLinks, Element root, Context rootContext) {

        if (activity.getLocalName().equals("flow"))
            addLinksFlow(activity, internalLinks, root, rootContext);

        if (activity.getLocalName().equals("if"))
            addLinksIf(activity, internalLinks, root, rootContext);

        if (activity.getLocalName().equals("pick"))
            addLinksPick(activity, internalLinks, root, rootContext);

        if (activity.getLocalName().equals("scope"))
            addLinksScope(activity, internalLinks, root, rootContext);

        if (activity.getLocalName().equals("sequence"))
            addLinksSequence(activity, internalLinks, root, rootContext);

        /*
         * We can skip children of repeatable constructs (ForEach, While and
         * RepeatUntil), but their OWN links must be included. Otherwise it is
         * an Assign, Compensate, CompensateScope, Empty, Exit, Invoke, Receive,
         * Reply, Rethrow, Throw, Validate or Wait activity.
         */
        setOwnLinks(activity, internalLinks, root, rootContext);
    }

    private static void addLinksSequence(Element activity, List<String> internalLinks, Element root, Context rootContext) {
        setOwnLinks(activity, internalLinks, root, rootContext);

        List<Element> children = Utils.getChlidActivities(activity);
        for (Element child : children) {
            addLinks(child, internalLinks, root, rootContext);
        }

    }

    private static void addLinksScope(Element activity, List<String> internalLinks, Element root, Context rootContext) {
        setOwnLinks(activity, internalLinks, root, rootContext);
        addLinks(Utils.getFirstChildActivity(activity), internalLinks, root, rootContext);
        // TODO Add links leaving fault handlers?

    }

    private static void addLinksPick(Element activity, List<String> internalLinks, Element root, Context rootContext) {
        setOwnLinks(activity, internalLinks, root, rootContext);

        List<Element> onMessages = Utils.getChildElements(new QName(BPELConstants.BPI, "onMessage"), activity);
        for (Element onMessage : onMessages) {
            addLinks(Utils.getFirstChildActivity(onMessage), internalLinks, root, rootContext);
        }

        List<Element> onAlarms = Utils.getChildElements(new QName(BPELConstants.BPI, "onAlarm"), activity);
        for (Element onAlarm : onAlarms) {
            addLinks(Utils.getFirstChildActivity(onAlarm), internalLinks, root, rootContext);
        }
    }

    private static void addLinksIf(Element activity, List<String> internalLinks, Element root, Context rootContext) {

        setOwnLinks(activity, internalLinks, root, rootContext);

        Element thenActivity = Utils.getFirstChildActivity(activity);
        addLinks(thenActivity, internalLinks, root, rootContext);

        Element elseActivity = Utils.getChildElement(new QName(BPELConstants.BPI, "else"), activity);
        if (elseActivity != null) {
            addLinks(elseActivity, internalLinks, root, rootContext);
        }

    }

    private static void addLinksFlow(Element activity, List<String> internalLinks, Element root, Context rootContext) {

        setOwnLinks(activity, internalLinks, root, rootContext);

        Element links = Utils.getChildElement("links", activity);
        Element link;
        if (links != null) {
            NodeList linkNodes = links.getElementsByTagNameNS(BPELConstants.BPI, "link");
            for (int i = 0; i < linkNodes.getLength(); i++) {
                link = (Element) linkNodes.item(i);
                internalLinks.add(link.getAttribute("name"));
            }

        }

        List<Element> children = Utils.getChlidActivities(activity);
        for (Element child : children) {
            addLinks(child, internalLinks, root, rootContext);
        }
    }

    private static void setOwnLinks(Element activity, List<String> internalLinks, Element root, Context rootContext) {

        List<String> linkNames = Utils.getSourceNames(activity);
        if (linkNames != null) {

            for (String linkName : linkNames) {
                if (!internalLinks.contains(linkName)) {
                    rootContext.setDependencies(linkName, getDependencies(root, activity));
                }
            }

        }

    }

    /**
     * @param linkName
     * @param activity
     * @return
     */
    private static List<String> getDependencies(Element root, Element activity) {
        List<String> list = new ArrayList<String>();
        getDependencies(root, activity, list);
        return list;
    }

    /**
     * @param activity
     * @param dependencies
     */
    private static void getDependencies(Element root, Element activity, List<String> dependencies) {

        List<String> targets = Utils.getTargetsNames(activity);
        for (String target : targets) {
            if (isInternalLink(target, activity, root)) {
                Element source = getSource(target, activity);
                getDependencies(root, source, dependencies);
            } else {
                dependencies.add(target);
            }
        }

    }

    private static boolean isScopeActivity(Element activity) {
        if (!activity.getLocalName().equals("scope"))
            return false;

        if (!activity.getNamespaceURI().equals(BPELConstants.BPI))
            return false;

        return true;
    }

    /**
     * Checks if the link is crossing the boundaries of a scope activity.
     * 
     * @param link Name of the link to check.
     * @param source The source activity of the link.
     * @return true if the link is inter-scope, otherwise false.
     */
    public static boolean isInterScope(String link, Element source) {

        Element flow = null, parent = (Element) source.getParentNode();
        while (flow == null) {
            if (isScopeActivity(parent))
                return true;

            if (definesLink(parent, link))
                flow = parent;
            else
                parent = (Element) parent.getParentNode();
        }

        return isSourceInterScope(link, flow);

    }

    private static boolean isSourceInterScope(String link, Element flow) {

        List<Element> children = Utils.getChlidActivities(flow);
        boolean source;

        for (Element child : children) {
            if (Utils.getSourceNames(child).contains(link))
                return false;
            else {
                if (!definesLink(child, link) && !isScopeActivity(child)) {
                    source = isSourceInterScope(link, child);
                    if (!source)
                        return false;
                }
            }
        }

        return true;
    }

    private static Element getSource(String link, Element activity) {

        Element flow = null, parent = (Element) activity.getParentNode();
        while (flow == null) {
            if (definesLink(parent, link))
                flow = parent;
            else
                parent = (Element) parent.getParentNode();
        }

        return findSource(link, flow);
    }

    private static Element findSource(String link, Element flow) {

        List<Element> children = Utils.getChlidActivities(flow);
        Element source;

        for (Element child : children) {
            if (Utils.getSourceNames(child).contains(link))
                return child;
            else {
                if (!definesLink(child, link)) {
                    source = findSource(link, child);
                    if (source != null)
                        return source;
                }
            }
        }

        return null;
    }

    private static boolean isInternalLink(String link, Element targetActivity, Element root) {
        Element activity = targetActivity;
        while (activity != root.getParentNode()) {
            if (definesLink(activity, link)) {
                return true;
            }
        }

        return false;
    }

    private static boolean definesLink(Element activity, String linkName) {
        if (!activity.getNamespaceURI().equals(BPELConstants.BPI))
            return false;
        if (!activity.getLocalName().equals("flow"))
            return false;

        Element linksElement = Utils.getChildElement("links", activity);
        if (linksElement == null)
            return false;

        List<Element> linkElements = Utils.getChildElements(new QName(BPELConstants.BPI), linksElement);
        if (linkElements == null)
            return false;

        for (Element linkElement : linkElements) {
            if (linkElement.getAttribute("name").equals(linkName))
                return true;
        }

        return false;

    }

}
