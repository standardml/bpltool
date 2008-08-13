package com.beepell.deployment;

import java.util.List;

import javax.xml.namespace.QName;

import org.w3c.dom.Document;

import com.beepell.execution.ProcessInstance;
import com.beepell.linkgraph.LinkGraph;
import com.beepell.model.Activity;
import com.beepell.model.OnEvent;
import com.beepell.model.OnMessage;
import com.beepell.model.PartnerLink;
import com.beepell.model.ProcessDescription;
import com.beepell.model.ReceiveActivity;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;

/**
 * @author Tim Hallwyl
 */
public interface ProcessContext {

    /**
     * Gets a list of all inbound message activities (IMAs) in the process
     * description. This is Receive activity, Pick activity using onMessage or
     * onEvent. To clarify; the list contains ReceiveActivity, OnMessage and
     * OnEvent objects.
     * 
     * @return a list of all inbound message activities (IMAs).
     */
    public List<Object> getInboundMessageActivities();

    /**
     * Gets a list of all start activities in the process description. This is
     * receive or pick activities with createInstance set to true (yes).
     * 
     * @return a list of all start activities.
     */
    public List<Activity> getStartActivities();

    /**
     * Gets a list of all start activities (ReceiveActivity or PickActivity)
     * receiving messages from a PartnerLink of the specified type with the
     * specified myRole.
     * <p>
     * 
     * @param partnerLinkType
     * @param myrole
     * @return A list of starting activities receiving messages on the port type
     *         pointed at by the partner link type and myrole.
     */
    public List<Activity> getStartActivities(QName partnerLinkType, String myrole);

    /**
     * Gets the PartnerLink object for the receive activity. Used at deployment
     * to get the partner link object for a receive activity configuration. This
     * method will walk through the process description tree to find the partner
     * link used for the receive.
     * 
     * @param receive
     * @return The partner link used.
     */
    public PartnerLink getPartnerLink(ReceiveActivity receive);

    /**
     * Gets the PartnerLink object for the Pick activity.
     * 
     * @param onMessage
     * @return the PartnerLink object for the Pick activity.
     */
    public PartnerLink getPartnerLink(OnMessage onMessage);

    /**
     * Gets the PartnerLink object for the OnEvent activity.
     * 
     * @param onEvent
     * @return the PartnerLink object for the OnEvent activity.
     */
    public PartnerLink getPartnerLink(OnEvent onEvent);

    /**
     * Get the schema repository for the process.
     * 
     * @return the schema repository for the process.
     */
    public SchemaRepository getSchemaRepository();

    /**
     * Get the service repository for the process.
     * 
     * @return the service repository for the process.
     */
    public ServiceRepository getServiceRepository();

    /**
     * The the ProcessDescription.
     * 
     * @return the process description.
     */
    public ProcessDescription getDescription();

    /**
     * Get the global LinkGraph for this process.
     * 
     * @return the global LinkGraph for this process.
     */
    public LinkGraph getLinkGraph();

    /**
     * Get list of all instances of this process, including completed.
     * 
     * @return list of all instances
     */
    public List<ProcessInstance> getInstances();

    /**
     * Used by process instance to add itself.
     * @param instance
     */
    void addInstance(ProcessInstance instance);
    
    /**
     * Gets the S-BPEL DOM tree.
     * @return the S-BPEL DOM tree.
     */
    public Document getSBPELDocument();
}
