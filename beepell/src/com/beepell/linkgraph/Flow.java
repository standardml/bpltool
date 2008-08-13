package com.beepell.linkgraph;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import com.beepell.model.Activity;
import com.beepell.model.FlowActivity;
import com.beepell.model.ScopeActivity;

/**
 * Internal construct used by LinkGraph to track link declarations in Flows.
 */
public class Flow {

    private ArrayList<Link> leaving;

    private ArrayList<Link> entering;

    private Flow parent = null;

    private ArrayList<Flow> children = new ArrayList<Flow>();

    private Hashtable<String, Link> links;

    /**
     * Internal construct used by LinkGraph to track link declarations in Flows.
     * 
     * @param parent
     * @param flow
     */
    public Flow(Flow parent, FlowActivity flow) {
        if (parent == null) {
            this.leaving = new ArrayList<Link>();
            this.entering = new ArrayList<Link>();
        } else {
            this.parent = parent;
            parent.addChild(this);
        }

        this.links = new Hashtable<String, Link>();
        if (flow != null && flow.getLinks() != null) {
            List<com.beepell.model.Link> links = flow.getLinks().getLink();
            for (com.beepell.model.Link link : links) {
                this.links.put(link.getName(), new Link(link.getName()));
            }
        }

    }

    /**
     * When an activity is found, that is the target of a link, this method is
     * called to register the target to the link declaration.
     * 
     * @param linkName
     * @param target
     * @param scope
     */
    public void setTarget(String linkName, Activity target, ScopeActivity scope) {
        Link link = links.get(linkName);

        if (link != null) {
            link.target = target;
            link.targetScope = scope;
        } else {
            if (parent != null)
                parent.setTarget(linkName, target, scope);
            else {
                link = new Link(linkName);
                link.target = target;
                link.targetScope = scope;
                entering.add(link);
            }
        }
    }

    /**
     * When an activity is found, that is the source of a link, this method is
     * called to register the source to the link declaration.
     * 
     * @param linkName
     * @param source
     * @param scope
     */
    public void setSource(String linkName, Activity source, ScopeActivity scope) {
        Link link = links.get(linkName);

        if (link != null) {
            link.source = source;
            link.sourceScope = scope;
        }

        else {
            if (parent != null)
                parent.setSource(linkName, source, scope);
            else {
                link = new Link(linkName);
                link.source = source;
                link.sourceScope = scope;
                leaving.add(link);
            }
        }
    }

    /**
     * Used to register child flow nodes.
     * @param child
     */
    private void addChild(Flow child) {
        children.add(child);
    }

    /**
     * Get a list of ALL links declared within the flow construct, INCLUDING
     * other Flow constructs that may be declared among the descendants. 
     * 
     * @return a list of ALL links declared
     */
    public List<Link> getLinks() {
        ArrayList<Link> collection = new ArrayList<Link>(this.links.values());

        if (children.size() == 0)
            return collection;
        else {
            for (Flow child : children) {
                collection.addAll(child.getLinks());
            }
            return collection;
        }
    }

    /**
     * Get the list of links entering the sub-tree.
     * @return a list of links entering the sub-tree.
     */
    public ArrayList<Link> getEntering() {
        return entering;
    }

    /**
     * Get the list of links leaving the sub-tree.
     * @return a list of links leaving the sub-tree.
     */
    public ArrayList<Link> getLeaving() {
        return leaving;
    }

}
