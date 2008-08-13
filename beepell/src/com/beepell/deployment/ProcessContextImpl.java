package com.beepell.deployment;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.beepell.execution.ProcessInstance;
import com.beepell.linkgraph.LinkGraph;
import com.beepell.model.Activity;
import com.beepell.model.Elseif;
import com.beepell.model.FlowActivity;
import com.beepell.model.ForEachActivity;
import com.beepell.model.IfActivity;
import com.beepell.model.OnAlarmPick;
import com.beepell.model.OnEvent;
import com.beepell.model.OnMessage;
import com.beepell.model.PartnerLink;
import com.beepell.model.PickActivity;
import com.beepell.model.ProcessDescription;
import com.beepell.model.ReceiveActivity;
import com.beepell.model.ScopeActivity;
import com.beepell.model.SequenceActivity;
import com.beepell.model.WhileActivity;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;

/**
 * Process Context holds static information on the process.
 * 
 * @author Tim Hallwyl
 */
public class ProcessContextImpl implements ProcessContext {

    /** XML Schema definitions */
    private final SchemaRepository schemas;

    /** WSDL definitions */
    private final ServiceRepository services;

    /** BPEL process definition. */
    private final ProcessDescription process;
    
    /** LinkGraph for the entire process */
    private final LinkGraph linkGraph;

    private final Document bpel;

    private List<ProcessInstance> instances = new ArrayList<ProcessInstance>();
    
    private Hashtable<Object, PartnerLink> partnerLinks = new Hashtable<Object, PartnerLink>();

    private List<Activity> startActivities = new ArrayList<Activity>();

    private List<Object> inboundMessageActivities = new ArrayList<Object>();

    private List<OnEvent> events = new ArrayList<OnEvent>();

    /**
     * Create a Process Context.
     * 
     * @param bpel
     * @param process
     * @param schemas
     * @param services
     */
    public ProcessContextImpl(Document bpel, ProcessDescription process, SchemaRepository schemas, ServiceRepository services) {

        this.schemas = schemas;
        this.services = services;
        this.bpel = bpel;
        this.process = process;
        this.linkGraph = new LinkGraph(process.getActivity());

        treeWalk(process);
        

    }

    public List<Object> getInboundMessageActivities() {
        return inboundMessageActivities;
    }

    public com.beepell.model.PartnerLink getPartnerLink(ReceiveActivity receive) {
        return partnerLinks.get(receive);
    }

    public SchemaRepository getSchemaRepository() {
        return schemas;
    }

    public ServiceRepository getServiceRepository() {
        return services;
    }

    public List<Activity> getStartActivities() {
        return startActivities;
    }

    private void treeWalk(ProcessDescription process) {
        Hashtable<String, PartnerLink> partnerLinks = new Hashtable<String, PartnerLink>();
        
        if (process.getPartnerLinks() != null) {
            List<PartnerLink> localLinks = process.getPartnerLinks().getPartnerLink();
            for (PartnerLink link : localLinks) {
                partnerLinks.put(link.getName(), link);
            }
        }
        
        treeWalk(process.getActivity(), partnerLinks);
    }

    @SuppressWarnings("unchecked")
    private void treeWalk(Activity activity, Hashtable<String, PartnerLink> partnerLinks) {

        if (activity instanceof ReceiveActivity) {
            ReceiveActivity receive = (ReceiveActivity) activity;

            this.inboundMessageActivities.add(activity);
            if (((ReceiveActivity) activity).isCreateInstance()) {
                this.startActivities.add(activity);
            }
            this.partnerLinks.put(receive, partnerLinks.get(receive.getPartnerLink()));
        }

        if (activity instanceof FlowActivity) {
            List<Activity> activities = ((FlowActivity) activity).getActivity();
            for (Activity activity2 : activities) {
                treeWalk(activity2, partnerLinks);
            }
        }

        if (activity instanceof ForEachActivity) {
            treeWalk(((ForEachActivity) activity).getScope(), partnerLinks);
        }

        if (activity instanceof IfActivity) {
            IfActivity ifActivity = (IfActivity) activity;
            treeWalk(ifActivity.getActivity(), partnerLinks);

            List<Elseif> elseifs = ifActivity.getElseif();
            for (Elseif elseif : elseifs) {
                treeWalk(elseif.getActivity(), partnerLinks);
            }

            if (ifActivity.getElse() != null)
                treeWalk(ifActivity.getElse().getActivity(), partnerLinks);
        }

        if (activity instanceof PickActivity) {
            PickActivity pickActivity = (PickActivity) activity;
            List<OnMessage> onMessages = pickActivity.getOnMessage();
            List<OnAlarmPick> onAlarms = pickActivity.getOnAlarm();

            if (pickActivity.getOnMessage().size() > 0) {                
                if (pickActivity.isCreateInstance())
                    this.startActivities.add(activity);
            }

            for (OnAlarmPick onAlarm : onAlarms) {
                treeWalk(onAlarm.getActivity(), partnerLinks);
            }

            for (OnMessage onMessage : onMessages) {
                this.partnerLinks.put(onMessage, partnerLinks.get(onMessage.getPartnerLink()));
                this.inboundMessageActivities.add(onMessage);
                treeWalk(onMessage.getActivity(), partnerLinks);
            }

        }

        if (activity instanceof ScopeActivity) {
            ScopeActivity scopeActivity = (ScopeActivity) activity;

            if (scopeActivity.getEventHandlers() != null) {
                List<OnEvent> events = scopeActivity.getEventHandlers().getOnEvent();
                for (OnEvent event : events) {
                    this.events.add(event);
                    this.inboundMessageActivities.add(event);
                    this.partnerLinks.put(event, partnerLinks.get(event.getPartnerLink()));
                }
            }

            if (scopeActivity.getPartnerLinks() != null) {
                partnerLinks = (Hashtable<String, PartnerLink>) partnerLinks.clone();
                List<PartnerLink> localLinks = scopeActivity.getPartnerLinks().getPartnerLink();
                for (PartnerLink link : localLinks) {
                    partnerLinks.put(link.getName(), link);
                }
            }

            treeWalk(((ScopeActivity) activity).getActivity(), partnerLinks);
        }

        if (activity instanceof SequenceActivity) {
            List<Activity> activities = ((SequenceActivity) activity).getActivity();
            for (Activity activity2 : activities) {
                treeWalk(activity2, partnerLinks);
            }
        }

        if (activity instanceof WhileActivity) {
            treeWalk(((WhileActivity) activity).getActivity(), partnerLinks);
        }

    }

    public ProcessDescription getDescription() {
        return process;
    }

    public com.beepell.model.PartnerLink getPartnerLink(OnMessage onMessage) {
        return this.partnerLinks.get(onMessage);
    }

    public com.beepell.model.PartnerLink getPartnerLink(OnEvent onEvent) {
        return this.partnerLinks.get(onEvent);
    }

    /**
     * @return the bpel
     */
    public Document getSBPELDocument() {
        return bpel;
    }

    public List<Activity> getStartActivities(final QName partnerLinkType, final String myrole) {
        PickActivity pick;
        PartnerLink partnerLink;
        
        List<Activity> list = new ArrayList<Activity>();
        
        for (Activity activity : startActivities) {
            
            if (activity instanceof ReceiveActivity) {
                partnerLink = getPartnerLink((ReceiveActivity) activity);
                if ( partnerLinkType.equals(partnerLink.getPartnerLinkType()) )
                    list.add(activity);
            }
            
            if (activity instanceof PickActivity) {
                pick = (PickActivity) activity; 
                for (OnMessage onMessage : pick.getOnMessage()) {
                    partnerLink = getPartnerLink(onMessage);
                    if ( partnerLinkType.equals(partnerLink.getPartnerLinkType()) ) {
                        list.add(activity);
                        break;
                    }
                }
            }
        }

        return list;
    }

    public LinkGraph getLinkGraph() {
        return linkGraph;
    }

    public void addInstance(ProcessInstance instance) {
        // TODO: change so process context creates instances.
        this.instances.add(instance);
        
    }

    public List<ProcessInstance> getInstances() {        
        return instances;
    }
}
