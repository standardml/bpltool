package com.beepell.activity.structured;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Hashtable;
import java.util.List;
import java.util.Timer;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMException;
import org.xml.sax.SAXException;

import com.beepell.activity.ActivityState;
import com.beepell.activity.basic.InboundMessageActivity;
import com.beepell.broker.MessageBroker;
import com.beepell.exceptions.BPELFault;
import com.beepell.exceptions.BPELStandardFault;
import com.beepell.exceptions.InvalidExpressionValue;
import com.beepell.exceptions.JoinFailure;
import com.beepell.exceptions.MissingReply;
import com.beepell.exceptions.ScopeInitializationFailure;
import com.beepell.exceptions.SubLanguageExecutionFault;
import com.beepell.exceptions.UninitializedVariable;
import com.beepell.execution.CorrelationSet;
import com.beepell.execution.EndpointReference;
import com.beepell.execution.ExecutionContext;
import com.beepell.execution.ExecutionThread;
import com.beepell.execution.PartnerLink;
import com.beepell.execution.ProcessInstanceState;
import com.beepell.execution.TransactionalExecutionContext;
import com.beepell.execution.event.AlarmEventHandler;
import com.beepell.execution.event.MessageEventHandler;
import com.beepell.model.Activity;
import com.beepell.model.AssignActivity;
import com.beepell.model.Catch;
import com.beepell.model.Copy;
import com.beepell.model.EmptyActivity;
import com.beepell.model.EventHandlers;
import com.beepell.model.ExitActivity;
import com.beepell.model.OnAlarmEvent;
import com.beepell.model.OnEvent;
import com.beepell.model.ScopeActivity;
import com.beepell.model.SequenceActivity;
import com.beepell.model.To;
import com.beepell.repository.SchemaRepository;
import com.beepell.repository.ServiceRepository;
import com.beepell.variable.ComplexTypeVariable;
import com.beepell.variable.ElementVariable;
import com.beepell.variable.MessageVariable;
import com.beepell.variable.SimpleTypeVariable;
import com.beepell.variable.Variable;
import com.sun.xml.xsom.XSElementDecl;

/**
 * Scope Activity.
 * <p>
 * A scope provides the context which influences the execution behavior of its
 * enclosed activities. This behavioral context includes variables, partner
 * links, message exchanges, correlation sets, event handlers, fault handlers, a
 * compensation handler, and a termination handler. Contexts provided by scope
 * activities can be nested hierarchically, while the root context is provided
 * by the process construct.
 * <p>
 * A scope can declare variables, partner links, message exchanges and
 * correlation sets that are visible only within the scope.
 * 
 * @author Tim Hallwyl
 */
public class Scope extends AbstractStructuredActivity {

    private final boolean exitOnStandardFault;

    /**
     * Contexts provided by scope activities can be nested hierarchically, while
     * the â€œrootâ€� context is provided by the process construct.
     */
    private final Scope parent;

    /**
     * Each scope has a required primary activity that defines its normal
     * behavior.
     */
    private final Activity activity;

    private final Hashtable<String, PartnerLink> partnerLinks;

    private final Hashtable<String, Variable> variables;

    private final Hashtable<String, CorrelationSet> correlationSets;

    private final List<InboundMessageActivity> imas;

    private final List<Scope> enclosedScopes;

    private final boolean isIsolated;

    /* Compensation Handler */
    private Activity compensationHandler = null;

    /* Fault Handlers */
    private List<Catch> faultHandlers;

    private Activity catchAll;

    private Activity terminationHandler;

    private BPELFault fault;

    private SchemaRepository schemas;

    private final static Calendar calendar = GregorianCalendar.getInstance();

    private final static Timer timer = new Timer("EventTimerThread");

    private final List<AlarmEventHandler> alarms = new ArrayList<AlarmEventHandler>();

    /**
     * Create a Scope without a parent, that is, a global scope - also known as
     * the process scope.
     * 
     * @param configuration
     */
    public Scope(ScopeActivity configuration) {
        this(configuration, null);
    }

    /**
     * Create a local Scope with a counter variable. This is used by the ForEach
     * activity.
     * 
     * @param configuration
     * @param parent the parent scope, may be null.
     * @param counterVariableName name of the counter value.
     * @param counterVariableValue the value of the counter value.
     * @param schemas
     */
    public Scope(ScopeActivity configuration, Scope parent, String counterVariableName, long counterVariableValue, SchemaRepository schemas) {
        this(configuration, parent);
        try {
            SimpleTypeVariable counterVariable = new SimpleTypeVariable(new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedInt"), counterVariableName, schemas);
            counterVariable.initialize();
            counterVariable.getValue().setData(String.valueOf(counterVariableValue));
            this.variables.put(counterVariableName, counterVariable);

        } catch (ParserConfigurationException exception) {
            exception.printStackTrace();
            System.exit(1);
            /* This should not happen, using default parser configuraion */
        } catch (DOMException exception) {
            exception.printStackTrace();
            System.exit(1);
            /* This should not happen */
        } catch (UninitializedVariable exception) {
            exception.printStackTrace();
            System.exit(1);
            /* This should not happen, as we just initialized the variable */
        }
    }

    /**
     * A local Scope.
     * 
     * @param configuration
     * @param parent the parent scope.
     */
    public Scope(ScopeActivity configuration, Scope parent) {
        super(configuration);
        this.parent = parent;
        this.activity = configuration.getActivity();
        this.imas = new ArrayList<InboundMessageActivity>();
        this.enclosedScopes = new ArrayList<Scope>();

        this.isIsolated = configuration.isIsolated();

        if (parent != null)
            parent.addImmediatelyEnclosedScope(this);

        if (configuration.getPartnerLinks() != null)
            this.partnerLinks = new Hashtable<String, PartnerLink>(configuration.getPartnerLinks().getPartnerLink().size());
        else
            this.partnerLinks = null;

        if (configuration.getVariables() != null)
            this.variables = new Hashtable<String, Variable>(configuration.getVariables().getVariable().size());
        else
            this.variables = new Hashtable<String, Variable>();

        if (configuration.getCorrelationSets() != null)
            this.correlationSets = new Hashtable<String, CorrelationSet>(configuration.getCorrelationSets().getCorrelationSet().size());
        else
            this.correlationSets = null;

        this.exitOnStandardFault = configuration.isExitOnStandardFault();

        setState(ActivityState.READY);
    }

    @Override
    protected synchronized void run() throws BPELFault {
        ScopeActivity configuration = (ScopeActivity) this.configuration;
        this.schemas = context.getSchemaRepository();
        ExecutionContext originalContext = context;
        if (isIsolated)
            context = context.newTransaction();

        /*
         * Scope initialization is an all-or-nothing behavior: either it all
         * occurs successfully or a bpel:scopeInitializationFailure fault MUST
         * be thrown to the parent scope of the failed <scope>.
         */
        initialize();

        /*
         * Once scope initialization completes, the primary activity of the
         * scope is executed and the event handlers are installed in parallel
         * with each other.
         */
        /*
         * An exception to the previous rule applies to scope's that contain a
         * process' initial start activity: the initial start activity MUST
         * complete before the event handlers are installed.
         */

        installEventHandlers();
        execute(activity, context.create(this));
        uninstallEventHandlers();

        if (getState() == ActivityState.FAILING) {
            /* a fault occurred and all child activities have been terminated. */
            handleFault();
            /* Check for orphaned IMAs */
            if (imas.size() > 0) {
                /* 1. close orphans */
                for (InboundMessageActivity ima : imas) {
                    // TODO: How to close an IMA
                    ima.reply(null);
                }
                /* 2. throw missing reply fault to parent scope */
                if (parent != null)
                    throw new MissingReply("Scope '" + getName() + "' has " + imas.size() + " open inbound message activities.");
            }
        } else {
            /*
             * Normal (non-faulted) completion: 1. Check for orphaned IMAs 2.
             * install compensation handler
             */
            /* Check for orphaned IMAs */
            if (imas.size() > 0) {
                /* 1. close orphans */
                for (InboundMessageActivity ima : imas.toArray(new InboundMessageActivity[imas.size()])) {
                    // TODO: How to close an IMA
                    ima.reply(null);
                }
                /* 2. throw missing reply fault to own fault handler */
                fail(new MissingReply("Scope '" + getName() + "' has " + imas.size() + " open inbound message activities."));
                handleFault();
            } else {

                /* Install compensation handler */
                if (configuration.getCompensationHandler() != null)
                    this.compensationHandler = configuration.getCompensationHandler().getActivity();

            }
        }

        // Await events to complete
        joinEvents();

        if (isIsolated)
            ((TransactionalExecutionContext) context).commit();
        context = originalContext;

    }

    /**
     * <p>
     * Scope initialization consists of instantiating and initializing the
     * scope's variables and partner links; instantiating the correlation sets;
     * and installing fault handlers, termination handler and and event
     * handlers.
     * <p>
     * Scope initialization is an all-or-nothing behavior: either it all occurs
     * successfully or a bpel:scopeInitializationFailure fault MUST be thrown to
     * the parent scope of the failed scope.
     * 
     * @throws ScopeInitializationFailure
     */
    private void initialize() throws ScopeInitializationFailure {
        ScopeActivity configuration = (ScopeActivity) this.configuration;
        ServiceRepository services = context.getServiceRepository();

        try {
            /*
             * Initialize Partner Links: Any partner links defined in the scope
             * MUST be set before variables defined in the same scope whose
             * initialization logic refers to those partner links.
             */
            if (configuration.getPartnerLinks() != null)
                initializePartnerLinks(configuration.getPartnerLinks().getPartnerLink());

            /*
             * Instantiating and initializing the scope's variables
             */
            if (configuration.getVariables() != null)
                initializeVariables(configuration.getVariables().getVariable());

            /*
             * Instantiating the correlation sets
             */
            if (configuration.getCorrelationSets() != null) {
                CorrelationSet set;
                for (com.beepell.model.CorrelationSet correlationSetConf : configuration.getCorrelationSets().getCorrelationSet()) {
                    set = new CorrelationSet(services, correlationSetConf.getName(), correlationSetConf.getProperties());
                    this.correlationSets.put(set.getName(), set);
                }
            }

            if (configuration.getFaultHandlers() != null) {
                if (configuration.getFaultHandlers().getCatch() != null)
                    this.faultHandlers = configuration.getFaultHandlers().getCatch();

                if (configuration.getFaultHandlers().getCatchAll() != null)
                    this.catchAll = configuration.getFaultHandlers().getCatchAll().getActivity();
            }

            if (configuration.getTerminationHandler() != null)
                this.terminationHandler = configuration.getTerminationHandler().getActivity();

        } catch (Exception exception) {
            /*
             * Scope initialization is an all-or-nothing behavior: either it all
             * occurs successfully or a bpel:scopeInitializationFailure fault
             * MUST be thrown to the parent scope of the failed scope.
             */
            throw new ScopeInitializationFailure(exception);
        }

    }

    private void initializePartnerLinks(List<com.beepell.model.PartnerLink> configuration) {
        ServiceRepository repository = context.getServiceRepository();

        EndpointReference my, partner;
        PartnerLink partnerLink;
        for (com.beepell.model.PartnerLink link : configuration) {

            if (link.getMyRole() != null)
                my = new EndpointReference(repository.getPort(link.getPartnerLinkType(), link.getMyRole()));
            else
                my = null;

            if (link.getPartnerRole() != null && link.isInitializePartnerRole() == Boolean.TRUE)
                partner = new EndpointReference(repository.getPort(link.getPartnerLinkType(), link.getPartnerRole()));
            else if (link.getPartnerRole() != null && link.isInitializePartnerRole() != Boolean.TRUE)
                partner = new EndpointReference();
            else
                partner = null;

            partnerLink = new PartnerLink(link, my, partner);
            this.partnerLinks.put(partnerLink.getName(), partnerLink);
        }

    }

    private void initializeVariables(List<com.beepell.model.Variable> variables) throws Exception, ParserConfigurationException, SAXException {
        SchemaRepository schemas = context.getSchemaRepository();
        ServiceRepository services = context.getServiceRepository();
        SequenceActivity sequence = new SequenceActivity();

        Variable variable = null;
        String name = null;
        boolean fromSpecUsed = false;

        for (com.beepell.model.Variable configuration : variables) {
            name = configuration.getName();

            if (configuration.getElement() != null) {
                variable = new ElementVariable(configuration.getElement(), name, schemas);
            }

            if (configuration.getType() != null) {

                if (schemas.getType(configuration.getType()).isComplexType()) {
                    variable = new ComplexTypeVariable(configuration.getType(), name, schemas);
                } else {
                    variable = new SimpleTypeVariable(configuration.getType(), name, schemas);
                }

            }

            if (configuration.getMessageType() != null) {
                variable = new MessageVariable(configuration.getMessageType(), name, schemas, services);
            }

            this.variables.put(name, variable);

            if (configuration.getFrom() != null) {
                fromSpecUsed = true;
                /*
                 * A variable can optionally be initialized by using an in-line
                 * from-spec.
                 */
                Copy copy = new Copy();
                copy.setFrom(configuration.getFrom());
                To to = new To();
                to.setVariable(name);
                copy.setTo(to);
                AssignActivity assign = new AssignActivity();
                assign.setSuppressJoinFailure(false);
                assign.setValidate(false);
                assign.getCopy().add(copy);
                sequence.getActivity().add(assign);
            }

        }

        if (fromSpecUsed) {
            /*
             * Conceptually the in-line variable initializations are modeled as
             * a virtual sequence activity that contains a series of virtual
             * assign activities, one for each variable being initialized, in
             * the order they are listed in the variable declarations. The
             * virtual assign activities each contain a single virtual copy
             * whose from-spec is as given in the variable initialization and
             * the to-spec points to the variable being created.
             */
            sequence.setSuppressJoinFailure(false);
            this.execute(sequence, context.create(this));
        }
    }

    /**
     * Gets the named variable.
     * 
     * @param name
     * @return variable;
     * @throws IllegalArgumentException if no variable was found by the
     *             specified name.
     */
    public Variable getVariable(String name) throws IllegalArgumentException {
        if (name == null)
            throw new IllegalArgumentException("Variable name cannot be null.");

        if (this.variables != null) {
            Variable variable = variables.get(name);
            if (variable != null)
                return variable;
        }

        if (parent != null)
            return parent.getVariable(name);

        throw new IllegalArgumentException("Variable '" + name + "' was not found.");
    }

    /**
     * Get the named correlation set.
     * 
     * @param name
     * @return the named correlation set.
     */
    public CorrelationSet getCorrelationSet(String name) {
        if (this.correlationSets != null) {
            CorrelationSet set = correlationSets.get(name);
            if (set != null)
                return set;
        }

        if (parent != null)
            return parent.getCorrelationSet(name);

        throw new IllegalArgumentException("Correlation Set '" + name + "' was not found.");
    }

    /**
     * Gets the named partner link.
     * 
     * @param name
     * @return the named partner link.
     */
    public PartnerLink getPartnerLink(String name) {
        if (this.partnerLinks != null) {
            PartnerLink link = this.partnerLinks.get(name);
            if (link != null)
                return link;
        }

        if (parent != null)
            return parent.getPartnerLink(name);

        throw new IllegalArgumentException("Partner Link '" + name + "' was not found.");
    }

    /**
     * Method to add an IMA when a message is received and a reply is expected
     * 
     * @param ima
     */
    public void addOpenIMA(InboundMessageActivity ima) {
        imas.add(ima);

    }

    /**
     * Method to remove an open IMA after successful reply.
     * 
     * @param ima
     */
    public void removeOpenIMA(InboundMessageActivity ima) {
        imas.remove(ima);

    }

    /**
     * Method for Receive to get a reference to the corresponding IMA
     * 
     * @param partnerLink
     * @param operation
     * @param messageExchange
     * @return a reference to the corresponding IMA
     */
    public InboundMessageActivity getOpenIMA(String partnerLink, String operation, String messageExchange) {

        for (InboundMessageActivity ima : imas) {
            if (partnerLink.equals(ima.getPartnerLink()) && operation.equals(ima.getOperation())) {

                if (messageExchange == null && ima.getMessageExchange() == null)
                    return ima;

                if (messageExchange != null && messageExchange.equals(ima.getMessageExchange()))
                    return ima;
            }
        }

        if (parent == null)
            return null;
        else
            return parent.getOpenIMA(partnerLink, operation, messageExchange);
    }

    /**
     * Get the list of immediately enclosed (child) scope instances. Note: This
     * does not include scope instances in FCT-handlers.
     * 
     * @return the list of immediately enclosed (child) scope instances.
     */
    public List<Scope> getImmediatelyEnclosedScopeInstances() {
        return enclosedScopes;
    }

    private void addImmediatelyEnclosedScope(Scope scope) {
        this.enclosedScopes.add(scope);
    }

    /**
     * Activate the compensation handler to compensate the work of this Scope.
     * 
     * @throws BPELFault
     * @throws Exception The fault MUST be propagated to the caller of the
     *             compensation handler.
     */
    public void compensate() throws BPELFault {
        // TODO: WS-BPEL mandates that the compensation handler of an isolated
        // scope will itself have isolated behavior implicitly, although it will
        // create a separate isolation domain from that of its associated scope.
        // [12.5.3]

        /*
         * Any attempt to compensate a scope, for which the compensation handler
         * either has not been installed or has been installed and executed,
         * MUST be treated as executing an empty activity.
         */
        if (getState() != ActivityState.COMPLETED || compensationHandler == null) {
            execute(new EmptyActivity());
            return;
        }

        if (isIsolated)
            context = context.newTransaction();

        setState(ActivityState.COMPENSATING);

        this.faultHandlers = null;
        this.catchAll = null;
        execute(compensationHandler, context.create(this));
        this.compensationHandler = null;
        if (isIsolated)
            ((TransactionalExecutionContext) context).commit();
        setState(ActivityState.COMPENSATED);

        // TODO: Fix this...
        /*
         * After the partial work is undone, the compensation handler MUST be
         * uninstalled. The fault MUST be propagated to the caller of the
         * compensation handler.
         */

    }

    private void handleFault() {
        try {
            Catch match = match(fault);

            /*
             * If the scope faults before completion, then the appropriate fault
             * handler gets control and all other fault handlers and termination
             * handlers are uninstalled. A WS-BPEL processor MUST NOT run more
             * than one explicit or default FCT-handler for the same scope under
             * any circumstances.
             */

            if (match != null) {
                this.catchAll = null;
                if (match.getFaultVariable() != null) {
                    this.variables.put(match.getFaultVariable(), fault.getData().clone());
                }
                execute(match.getActivity(), context.create(this));
            } else {
                execute(catchAll, context.create(this));
            }

            this.faultHandlers = null;
            this.catchAll = null;
            this.terminationHandler = null;

        } catch (Exception exception) {
            // TODO handle exceptions
        }

    }

    /**
     * Called (indirectly) by an activity that failed, to start fault handling.
     * This will make the Scope enter FAILING state and terminate all children.
     * Actual fault handling is initiated in run() after child activities return
     * control.
     * 
     * @param fault the fault occurred
     */
    public void fail(BPELFault fault) {

        /*
         * If a fault occurs during compensation
         */
        if (getState() == ActivityState.COMPENSATING) {
            this.fault = fault;
            setState(ActivityState.FAILING);
            return;
        }

        /*
         * Termination handler cannot throw any fault. ... A fault in a
         * termination handler MUST cause all running contained activities to be
         * terminated.
         */
        if (getState() == ActivityState.TERMINATING) {
            nonBlockingTermination();
            return;
        }

        /*
         * Compensation is not enabled for a scope that has had an associated
         * fault handler invoked. This is ensured by never reaching COMPLETED
         * state.
         */
        this.fault = fault;
        setState(ActivityState.FAILING);

        /*
         * If the value of the exitOnStandardFault attribute on a scope is set
         * to "yes", then the process MUST exit immediately, as if an <exit>
         * activity has been reached, when any WS-BPEL standard fault other than
         * bpel:joinFailure reaches the scope.
         */
        if (exitOnStandardFault && fault instanceof BPELStandardFault && !(fault instanceof JoinFailure)) {
            log.info("This scope will exit on standard fault '" + fault.getName().getLocalPart() + "'.");
            execute(new ExitActivity());
            return;
        }

        /*
         * The behavior of a fault handler for a scope C begins by disabling the
         * scope's event handlers and implicitly terminating all activities
         * enclosed within C that are currently active (including all running
         * event handler instances). [12.6]
         */
        uninstallEventHandlers();

        /*
         * The behavior of fault handling for scope C MUST begin by terminating
         * all activities that are currently active and directly enclosed within
         * C. The termination of these activities MUST occur before the specific
         * behavior of a fault handler is started.
         */
        nonBlockingTermination();

    }

    private void nonBlockingTermination() {
        Thread thread = new Thread() {

            public synchronized void run() {
                com.beepell.activity.Activity[] children = getChildren().toArray(new com.beepell.activity.Activity[getChildren().size()]);
                for (com.beepell.activity.Activity child : children) {
                    child.terminate();
                }
            }
        };
        thread.start();
        try {
            Thread.sleep(100);
        } catch (InterruptedException exception) {
            /* ignore it */
        }
    }

    /**
     * Get the best match of the catch elements for the element type
     * 
     * @param type the fault data type
     * @param name the fault name; if null only handlers without a faultName
     *            attribute is considered
     * @param handlers the list for fault handlers (catch elements)
     * @return the best matching catch or null if no match was found.
     * @throws SAXException
     */
    private Catch bestElementMatch(QName type, QName name, List<Catch> handlers) throws SAXException {
        int selectedLevel = Integer.MAX_VALUE;
        Catch handler, selectedHandler = null;

        for (int index = 0; index < handlers.size(); index++) {
            handler = handlers.get(index);

            if ((name == null && handler.getFaultName() == null) || (name != null && name.equals(handler.getFaultName()))) {

                if (handler.getFaultElement() != null) {
                    XSElementDecl head = schemas.getElement(handler.getFaultElement());

                    if (head.canBeSubstitutedBy(schemas.getElement(type))) {
                        int level = getRelationLevel(handler.getFaultElement(), type);
                        if (level < selectedLevel) {
                            selectedLevel = level;
                            selectedHandler = handler;
                        }
                    }
                }
            }
        }

        return selectedHandler;
    }

    /**
     * Finds the Catch that matches, if any. If more than one Catch matches, the
     * best match is returned. If no Catch matches the fault null is returned.
     * 
     * @param fault the BPEL fault to match
     * @return the Catch that matches
     * @throws SAXException
     */
    private Catch match(BPELFault fault) throws SAXException {

        if (fault.getData() == null) {
            /*
             * When faults are thrown without associated data the fault MUST be
             * caught as follows:
             */

            /*
             * 1. If there is a <catch> construct with a matching faultName
             * value that does not specify a faultVariable attribute then the
             * fault is passed to the identified catch activity.
             */
            if (faultHandlers != null) {
                Catch handler;
                for (int index = 0; index < faultHandlers.size(); index++) {
                    handler = faultHandlers.get(index);
                    if (fault.getName().equals(handler.getFaultName()) && handler.getFaultVariable() == null)
                        return handler;
                }
            }

            return null;

        } else {
            Catch handler;
            /*
             * In the case of faults thrown with associated data the fault MUST
             * be caught as follows:
             */
            if (faultHandlers != null) {
                /*
                 * 1. If there is a <catch> construct with a matching faultName
                 * value that has a faultVariable whose type matches the type of
                 * the runtime fault data then the fault is passed to the
                 * identified <catch> construct [...] a WSDL message type
                 * variable can only match a WSDL message type fault data, while
                 * an element variable can only match element-based fault data.
                 */

                if (fault.getData() instanceof ElementVariable) {
                    Catch match = bestElementMatch(fault.getData().getType(), fault.getName(), faultHandlers);
                    if (match != null)
                        return match;

                } else {
                    for (int index = 0; index < faultHandlers.size(); index++) {
                        handler = faultHandlers.get(index);
                        /*
                         * For the case of WSDL message-based fault, they match
                         * only when their QNames are identical.
                         */
                        if (fault.getName().equals(handler.getFaultName()) && fault.getData().getType().equals(handler.getFaultMessageType()))
                            return handler;
                    }
                }

                /*
                 * 2. Otherwise if the fault data is a WSDL message type where
                 * the message contains a single part defined by an element and
                 * there exists a <catch> construct with a matching faultName
                 * value that has a faultVariable whose associated
                 * faultElementâ€™s QName matches the QName of the runtime element
                 * data of the single WSDL message part, then the fault is
                 * passed to the identified <catch> construct with the
                 * faultVariable initialized to the value in the single partâ€™s
                 * element
                 */
                if (fault.getData() instanceof MessageVariable) {
                    MessageVariable data = (MessageVariable) fault.getData();
                    if (data.getParts().size() == 1) {
                        Catch match = bestElementMatch(data.getType(data.getParts().get(0).getName()), fault.getName(), faultHandlers);
                        if (match != null)
                            return match;
                    }
                }

                /*
                 * 3. Otherwise if there is a <catch> construct with a matching
                 * faultName value that does not specify a faultVariable
                 * attribute then the fault is passed to the identified <catch>
                 * construct.
                 */
                for (int index = 0; index < faultHandlers.size(); index++) {
                    handler = faultHandlers.get(index);
                    if (fault.getName().equals(handler.getFaultName()) && handler.getFaultVariable() == null)
                        return handler;
                }

                /*
                 * 4. Otherwise if there is a <catch> construct without a
                 * faultName attribute that has a faultVariable whose type
                 * matches the type of the runtime fault data then the fault is
                 * passed to the identified <catch> construct [...] a WSDL
                 * message type variable can only match a WSDL message type
                 * fault data, while an element variable can only match
                 * element-based fault data.
                 */

                if (fault.getData() instanceof ElementVariable) {

                    Catch match = bestElementMatch(fault.getData().getType(), null, faultHandlers);
                    if (match != null)
                        return match;

                } else {
                    for (int index = 0; index < faultHandlers.size(); index++) {
                        handler = faultHandlers.get(index);
                        if (handler.getFaultName() == null) {
                            /*
                             * For the case of WSDL message-based fault, they
                             * match only when their QNames are identical.
                             */
                            if (fault.getData().getType().equals(handler.getFaultMessageType()))
                                return handler;
                        }
                    }
                }

                /*
                 * 5. Otherwise if the fault data is a WSDL message type where
                 * the message contains a single part defined by an element and
                 * there exists a <catch> construct without a faultName
                 * attribute that has a faultVariable whose associated
                 * faultElementâ€™s QName matches the QName of the runtime element
                 * data of the single WSDL message part, then the fault is
                 * passed to the identified <catch> construct with the
                 * faultVariable initialized to the value in the single partâ€™s
                 * element
                 */
                if (fault.getData() instanceof MessageVariable) {
                    MessageVariable data = (MessageVariable) fault.getData();
                    if (data.getParts().size() == 1) {
                        Catch match = bestElementMatch(data.getType(data.getParts().get(0).getName()), fault.getName(), faultHandlers);
                        if (match != null)
                            return match;
                    }
                }

            }

        }

        return null;
    }

    /**
     * Compares two elements; the HEAD and a substitutable returning the level
     * of substitutionGroup relation in XML element declaration: If it is the
     * same element the level is 0, if the substitution element has the HEAD
     * element as its substitution affiliate the level is 1, if the
     * substitutions affiliates affiliate is the HEAD the level is 2 and so on.
     * <p>
     * This method assumes that &qout;substitutable&qout; has been check to be a
     * substitutable of &qout;head&qout;.
     * 
     * @return level of substitutionGroup relation in XML element declaration
     * @throws SAXException
     */
    private int getRelationLevel(QName head, QName substitutable) throws SAXException {

        if (head.equals(substitutable))
            return 0;
        else {

            XSElementDecl substAffiliation = schemas.getElement(substitutable).getSubstAffiliation();
            QName affiliateName = new QName(substAffiliation.getTargetNamespace(), substAffiliation.getName());
            return 1 + getRelationLevel(head, affiliateName);

        }
    }

    /**
     * Uninstall the compensation handler.
     */
    public synchronized void uninstallCompensationHandler() {
        this.compensationHandler = null;
    }

    @Override
    public void terminate() {

        /*
         * Forced termination for a scope applies only if the scope is in normal
         * processing mode. [12.6]
         */
        ActivityState state = getState();
        if (state != ActivityState.WAITING && state != ActivityState.RUNNING) {
            return;
        }

        /*
         * The forced termination of a scope begins by disabling the scope's
         * event handlers and terminating its primary activity and all running
         * event handler instances.
         */
        uninstallEventHandlers();
        super.terminate();

        /*
         * Following this, the custom <terminationHandler> for the scope, if
         * present, is run. Otherwise, the default termination handler is run.
         * [12.6] <p> In our implementation, exit is the same as terminating the
         * process scope, but without termination handlers.
         * <p>
         * Process (global scope) does not have a termination handler. All Scope do. 
         */
        if (this.terminationHandler != null && context.getInstanceState() != ProcessInstanceState.EXITING)
            execute(this.terminationHandler, context.create(this));

    }

    /**
     * @return the fault
     */
    public BPELFault getFault() {
        return fault;
    }

    /**
     * Used by AlarmEventHandler
     * 
     * @param event
     * @return the execution thread
     */
    public ExecutionThread executeAlarmEvent(ScopeActivity event) {
        return executeThread(event, context.create(this));
    }

    private void installEventHandlers() throws SubLanguageExecutionFault, InvalidExpressionValue {
        long time = 0;
        EventHandlers handlers = ((ScopeActivity) this.configuration).getEventHandlers();
        if (handlers == null)
            return;

        List<OnAlarmEvent> alarms = handlers.getOnAlarm();
        for (OnAlarmEvent event : alarms) {
            AlarmEventHandler handler = new AlarmEventHandler(event.getScope(), this);

            if (event.getForExpression() != null) {
                time = event.getForExpression().evaluate(context).getTimeInMillis(calendar);

            } else {
                time = event.getUntilExpression().evaluate(context).toGregorianCalendar().getTimeInMillis() - System.currentTimeMillis();
            }

            if (event.getRepeatEveryExpression() == null) {
                log.info("Installing onAlarmEvent for execution in " + time + "ms.");
                timer.schedule(handler, time);
            } else {
                long repeat = event.getRepeatEveryExpression().evaluate(context).getTimeInMillis(calendar);
                log.info("Installing onAlarmEvent for execution in " + time + "ms and every " + repeat + "ms.");
                timer.schedule(handler, time, repeat);
            }
            this.alarms.add(handler);
        }

        List<OnEvent> events = handlers.getOnEvent();
        for (OnEvent event : events) {
            log.info("Installing onEvent for '" + event.getOperation() + "' on '" + event.getPartnerLink() + "'.");
            MessageEventHandler handler = new MessageEventHandler(event, context.create(this));
            MessageBroker.receive(handler);
        }

    }

    private void uninstallEventHandlers() {

        for (AlarmEventHandler handler : alarms) {
            handler.cancel();
        }

        // TODO uninstall event handlers

    }

    /**
     * Blocking while waiting for events to complete.
     */
    private void joinEvents() {

        for (AlarmEventHandler alarm : this.alarms) {
            alarm.join();
        }

        // TODO: join message events

    }

    /**
     * Used by MessageEventHandler
     * 
     * @param scope
     * @return the execution thread
     */
    public ExecutionThread executeMessageEvent(ScopeActivity scope) {
        return executeThread(scope, context.create(this));
    }

}
