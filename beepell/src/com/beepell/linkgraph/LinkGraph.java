package com.beepell.linkgraph;

import java.util.ArrayList;
import java.util.List;

import com.beepell.model.Activity;
import com.beepell.model.Catch;
import com.beepell.model.Elseif;
import com.beepell.model.FlowActivity;
import com.beepell.model.IfActivity;
import com.beepell.model.OnAlarmEvent;
import com.beepell.model.OnAlarmPick;
import com.beepell.model.OnEvent;
import com.beepell.model.OnMessage;
import com.beepell.model.PickActivity;
import com.beepell.model.ScopeActivity;
import com.beepell.model.SequenceActivity;
import com.beepell.model.Source;
import com.beepell.model.Target;

/**
 * @author Tim Hallwyl
 */
public class LinkGraph {

    private final Flow flow;

    /**
     * Create a LinkGraph with root activity.
     * 
     * @param activity
     */
    public LinkGraph(Activity activity) {
        flow = new Flow(null, null);
        addLinks(activity, flow, null);
    }

    /**
     * Get a list of links leaving the link graph.
     * 
     * @return a list of links leaving the link graph.
     */
    public List<Link> getLeavingLinks() {
        return flow.getLeaving();
    }

    /**
     * Get a list of links entering the link graph.
     * 
     * @return a list of links entering the link graph.
     */
    public List<Link> getEnteringLinks() {
        return flow.getEntering();
    }

    /*
     * unused. public Collection<Link> getLinkSet() { return flow.getLinks(); }
     */

    /**
     * Determine if the link is 'inter-scope'; if the target of the link is in
     * same scope as the source.
     * 
     * @param source the activity the link is sourced in
     * @param linkName name of the link to test
     * @return true if the target of the link is in same scope as the source.
     */
    public boolean isTargetInSameScope(Activity source, String linkName) {
        List<Link> links = flow.getLinks();
        for (Link link : links) {
            if (link.name.equals(linkName) && link.source.equals(source)) {

                if (link.sourceScope == null) {
                    if (link.targetScope == null)
                        return true;
                    else
                        return false;
                }

                if (link.sourceScope.equals(link.targetScope))
                    return true;
                else
                    return false;
            }
        }

        throw new IllegalArgumentException("Link '" + linkName + "' from source '" + source.getName() + "' was not found.");

    }

    /**
     * Gets a list of links entering the graph that the link given depends on.
     * 
     * @param link
     * @return a list of links entering the graph that the link given depends
     *         on.
     */
    public List<Link> getDependencies(Link link) {

        List<Link> links = getEnteringLinks(link.source);

        for (Link in : getIncomingLinks(link.source)) {
            links = merge(links, getDependencies(in));
        }
        return links;
    }

    private List<Link> getIncomingLinks(Activity target) {

        ArrayList<Link> incomingLinks = new ArrayList<Link>();
        for (Link link : flow.getLinks()) {
            if (link.target.equals(target))
                incomingLinks.add(link);
        }
        return incomingLinks;

    }

    private List<Link> getEnteringLinks(Activity target) {

        ArrayList<Link> enteringLinks = new ArrayList<Link>();
        for (Link link : flow.getEntering()) {
            if (link.target.equals(target))
                enteringLinks.add(link);
        }
        return enteringLinks;
    }

    private static void setOwnLinks(Activity activity, Flow flow, ScopeActivity scope) {

        if (activity.getTargets() != null) {
            List<Target> targets = activity.getTargets().getTarget();
            for (Target target : targets) {
                flow.setTarget(target.getLinkName(), activity, scope);
            }
        }

        if (activity.getSources() != null) {
            List<Source> sources = activity.getSources().getSource();
            for (Source source : sources) {
                flow.setSource(source.getLinkName(), activity, scope);
            }
        }

    }

    private static void addLinks(Activity activity, Flow flow, ScopeActivity scope) {

        if (activity instanceof FlowActivity)
            addLinks((FlowActivity) activity, flow, scope);

        if (activity instanceof IfActivity)
            addLinks((IfActivity) activity, flow, scope);

        if (activity instanceof PickActivity)
            addLinks((PickActivity) activity, flow, scope);

        if (activity instanceof ScopeActivity)
            addLinks((ScopeActivity) activity, flow, scope);

        if (activity instanceof SequenceActivity)
            addLinks((SequenceActivity) activity, flow, scope);

        /*
         * We can skip children of repeatable constructs (ForEach, While and
         * RepeatUntil), but their OWN links must be included. Otherwise it is
         * an Assign, Compensate, CompensateScope, Empty, Exit, Invoke, Receive,
         * Reply, Rethrow, Throw, Validate or Wait activity.
         */
        setOwnLinks(activity, flow, scope);
    }

    private static void addLinks(FlowActivity activity, Flow flow, ScopeActivity scope) {

        setOwnLinks(activity, flow, scope);
        flow = new Flow(flow, activity);

        List<Activity> children = activity.getActivity();
        for (Activity child : children) {
            addLinks(child, flow, scope);
        }
    }

    private static void addLinks(IfActivity activity, Flow flow, ScopeActivity scope) {
        setOwnLinks(activity, flow, scope);

        addLinks(activity.getActivity(), flow, scope);

        if (activity.getElse() != null)
            addLinks(activity.getElse().getActivity(), flow, scope);

        List<Elseif> elseifs = activity.getElseif();
        if (elseifs != null)
            for (Elseif elseif : elseifs)
                addLinks(elseif.getActivity(), flow, scope);
    }

    private static void addLinks(PickActivity activity, Flow flow, ScopeActivity scope) {
        setOwnLinks(activity, flow, scope);

        List<OnMessage> messages = activity.getOnMessage();
        for (OnMessage message : messages) {
            addLinks(message.getActivity(), flow, scope);
        }

        List<OnAlarmPick> alarms = activity.getOnAlarm();
        for (OnAlarmPick alarm : alarms) {
            addLinks(alarm.getActivity(), flow, scope);
        }

    }

    private static void addLinks(SequenceActivity activity, Flow flow, ScopeActivity scope) {
        setOwnLinks(activity, flow, scope);

        List<Activity> children = activity.getActivity();
        for (Activity child : children) {
            addLinks(child, flow, scope);
        }
    }

    private static void addLinks(ScopeActivity activity, Flow flow, ScopeActivity scope) {

        setOwnLinks(activity, flow, scope);

        // CHILD ACTIVITY
        addLinks(activity.getActivity(), flow, activity);

        // COMPENSATION HANDLER
        if (activity.getCompensationHandler() != null)
            addLinks(activity.getCompensationHandler().getActivity(), flow, scope);

        // EVENT HANDLERS
        if (activity.getEventHandlers() != null) {

            List<OnAlarmEvent> alarms = activity.getEventHandlers().getOnAlarm();
            if (alarms != null)
                for (OnAlarmEvent alarm : alarms)
                    addLinks(alarm.getScope(), flow, scope);

            List<OnEvent> events = activity.getEventHandlers().getOnEvent();
            if (events != null)
                for (OnEvent event : events)
                    addLinks(event.getScope(), flow, scope);

        }

        // FAULT HANDLERS
        if (activity.getFaultHandlers() != null) {
            List<Catch> catches = activity.getFaultHandlers().getCatch();
            for (Catch catchx : catches) {
                addLinks(catchx.getActivity(), flow, scope);
            }

            if (activity.getFaultHandlers().getCatchAll() != null)
                addLinks(activity.getFaultHandlers().getCatchAll().getActivity(), flow, scope);
        }

        // TERMINATION HANDLER
        if (activity.getTerminationHandler() != null)
            addLinks(activity.getTerminationHandler().getActivity(), flow, scope);

    }

    private List<Link> merge(List<Link> a, List<Link> b) {

        for (Link link : b) {
            a.add(link);
        }

        return a;
    }

}
