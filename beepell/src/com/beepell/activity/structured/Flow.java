package com.beepell.activity.structured;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import com.beepell.activity.ActivityState;
import com.beepell.execution.LinkListener;
import com.beepell.model.Activity;
import com.beepell.model.FlowActivity;
import com.beepell.model.Link;
import com.beepell.model.Target;

/**
 * The flow activity provides concurrency and synchronization.
 * <p>
 * A fundamental semantic effect of grouping a set of activities in a flow is to
 * enable concurrency. A flow completes when all of the activities enclosed by
 * the flow have completed. If its enabling condition evaluates to false then an
 * activity is skipped and also considered completed.
 * 
 * @author Tim Hallwyl
 */
public class Flow extends AbstractStructuredActivity {

    private final Hashtable<String, Boolean> states;

    private final List<String> links;

    private final List<Activity> activities;

    private final Hashtable<String, LinkListener> listeners;

    private final Flow parent;

    private final List<Dependency> deadLinks = new ArrayList<Dependency>();

    /**
     * Create a Flow Activity.
     * 
     * @param configuration
     * @param parent parent Flow
     */
    public Flow(FlowActivity configuration, Flow parent) {
        super(configuration);

        if (configuration.getLinks() != null) {
            List<Link> links = configuration.getLinks().getLink();

            this.links = new ArrayList<String>(links.size());
            for (Link link : links) {
                this.links.add(link.getName());
            }
            
            states = new Hashtable<String, Boolean>(links.size());
        } else {
            this.links = new ArrayList<String>(0);
            this.states = new Hashtable<String, Boolean>(0);
        }
        
        activities = configuration.getActivity();

        this.parent = parent;
        listeners = new Hashtable<String, LinkListener>();
        setState(ActivityState.READY);
    }

    /**
     * Create a Flow Activity, without a parent.
     * 
     * @param configuration
     */
    public Flow(FlowActivity configuration) {
        this(configuration, null);
    }

    @Override
    protected synchronized void run() {

        int i = 0;
        Thread[] threads = new Thread[activities.size()];

        for (Activity activity : activities) {
            threads[i++] = executeThread(activity, context.create(this));
        }

        join(threads);

    }

    /**
     * Gets the state of the link in the Flow scope.
     * 
     * @param link
     * @return the state of the link
     */
    public Boolean getLinkState(String link) {
        if (links.contains(link))
            return states.get(link);

        if (parent == null)
            return null;
        else
            return parent.getLinkState(link);
    }

    /**
     * Set the state of the link in Flow scope.
     * 
     * @param link
     * @param value
     */
    public void setLinkState(String link, boolean value) {
        if (states.get(link) != null)
            throw new IllegalStateException("Link " + link + " has already been determined");
        
        if (links.contains(link)) {
            states.put(link, value);
            notifyListeners(link);
            updateDependencies();
            return;
        }

        if (parent != null)
            parent.setLinkState(link, value);
    }

    /**
     * Add a dead link dependency. The leaving Link link depends on a set of
     * incoming links: When all of the dependencies are set, regardless of the
     * values they are set to, the Link link is set to false. This is part of
     * the dead path elimination.
     * 
     * @param link
     * @param dependencies
     */
    public void addDeadLinkDependency(com.beepell.linkgraph.Link link, List<com.beepell.linkgraph.Link> dependencies) {
        Dependency dependency = new Dependency();
        dependency.link = link;
        dependency.dependencies = dependencies;
        this.addDependency(dependency);
    }

    /**
     * This is used by addDeadLinkDependency above AND by child Flows who pass a dependency up.
     * @param dependency
     */
    private void addDependency(Dependency dependency) {
        if (dependency.flow == null && links.contains(dependency.link.name)); 
            dependency.flow = this;
        
        this.deadLinks.add(dependency);
        update(dependency);
    }
    
    /**
     * Used to update a single dependency.
     * @param dependency
     */
    private void update(Dependency dependency) {

        boolean ancestorLinksOnly = true;
        for (com.beepell.linkgraph.Link link : dependency.dependencies) {
            if (this.getLinkState(link.name) != null)
                dependency.dependencies.remove(link);
            else if (links.contains(link.name))
                ancestorLinksOnly = false;
        }

        // If there is no dependencies left, set link state to false.
        if (dependency.dependencies.size() == 0) {
            if (dependency.flow == null)
                setLinkState(dependency.link.name, false);
            else
                dependency.flow.setLinkState(dependency.link.name, false);
            
            this.deadLinks.remove(dependency);
        }

        // If all dependencies left are declared in ancestor Flows, then let the
        // parent handle it.
        if (ancestorLinksOnly) {
            parent.addDependency(dependency);
            this.deadLinks.remove(dependency);
        }

    }

    /**
     * Update all dead link dependencies. Used when a link is set. 
     */
    private void updateDependencies() {
        for (Dependency dependency : deadLinks) {
            update(dependency);           
        }
    }

    /**
     * Only one listener may be added per link, namely it's target activity. It
     * will only be notified once, as links only change state once.
     * 
     * @param listener
     * @param targets
     */
    public void addLinkListener(LinkListener listener, List<Target> targets) {
        for (Target target : targets) {
            listeners.put(target.getLinkName(), listener);
        }
    }

    /**
     * Notifies a LinkListener and removes the listener from the list of
     * listeners; It can only be notified once, as links only change state once.
     * 
     * @param link
     */
    private void notifyListeners(String link) {
        LinkListener listener = listeners.get(link);
        if (listener != null) {
            listener.linkChanged(link);
            listeners.remove(link);
        }
    }

    private class Dependency {

        Flow flow;

        com.beepell.linkgraph.Link link;

        List<com.beepell.linkgraph.Link> dependencies;
    }

}
