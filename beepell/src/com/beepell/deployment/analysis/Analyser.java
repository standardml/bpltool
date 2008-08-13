package com.beepell.deployment.analysis;

import org.w3c.dom.Document;

/**
 * @author Tim Hallwyl
 */
public class Analyser {

    private ErrorHandler errorHandler;

    /**
     * Set the Error Handler to capture errors, violations and warnings.
     * 
     * @param errorHandler
     */
    public void setErrorHandler(ErrorHandler errorHandler) {
        this.errorHandler = errorHandler;
    }

    /**
     * Gets the Error Handler object.
     * 
     * @return Error Handler object.
     */
    public ErrorHandler getErrorHandler() {
        return this.errorHandler;
    }

    /**
     * Perform Static Analysis on the process description.
     * 
     * @param document
     * @throws Exception
     */
    public void analyse(Document document) throws Exception {

        SA00001(document);
        SA00002(document);
        SA00003(document);
        SA00004(document);
        SA00005(document);
        SA00006(document);
        SA00007(document);
        SA00008(document);
        SA00009(document);
        SA00010(document);
        SA00011(document);
        SA00012(document);
        SA00013(document);
        SA00014(document);
        SA00015(document);
        SA00016(document);
        SA00017(document);
        SA00018(document);
        SA00019(document);
        SA00020(document);
        SA00021(document);
        SA00022(document);
        SA00023(document);
        SA00024(document);
        SA00025(document);
        SA00026(document);
        SA00027(document);
        SA00028(document);
        SA00029(document);
        SA00030(document);
        SA00031(document);
        SA00032(document);
        SA00033(document);
        SA00034(document);
        SA00035(document);
        SA00036(document);
        SA00037(document);
        SA00038(document);
        SA00039(document);
        SA00040(document);
        SA00041(document);
        SA00042(document);
        SA00043(document);
        SA00044(document);
        SA00045(document);
        SA00046(document);
        SA00047(document);
        SA00048(document);

        SA00050(document);
        SA00051(document);
        SA00052(document);
        SA00053(document);
        SA00054(document);
        SA00055(document);
        SA00056(document);
        SA00057(document);
        SA00058(document);
        SA00059(document);
        SA00060(document);
        SA00061(document);
        SA00062(document);
        SA00063(document);
        SA00064(document);
        SA00065(document);
        SA00066(document);
        SA00067(document);
        SA00068(document);
        SA00069(document);
        SA00070(document);
        SA00071(document);
        SA00072(document);
        SA00073(document);
        SA00074(document);
        SA00075(document);
        SA00076(document);
        SA00077(document);
        SA00078(document);
        SA00079(document);
        SA00080(document);
        SA00081(document);
        SA00082(document);
        SA00083(document);
        SA00084(document);
        SA00085(document);
        SA00086(document);
        SA00087(document);
        SA00088(document);
        SA00089(document);
        SA00090(document);
        SA00091(document);
        SA00092(document);
        SA00093(document);
        SA00094(document);
        SA00095(document);

    }

    /**
     * SA00001 A WS-BPEL processor MUST reject a WS-BPEL that refers to
     * solicit-response or notification operations portTypes.
     */
    private void SA00001(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00002 A WS-BPEL processor MUST reject any WSDL portType definition that
     * includes overloaded operation names.
     * 
     * @param document
     * @throws Exception
     */
    private void SA00002(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00003 If the value of exitOnStandardFault of a &lt;scope&gt; or
     * &lt;process&gt; is set to &quot;yes&quot;, then a fault handler that
     * explicitly targets the WS-BPEL standard faults MUST NOT be used in that
     * scope. See section 5.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00003(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00004 If any referenced queryLanguage or expressionLanguage is
     * unsupported by the WS-BPEL processor then the processor MUST reject the
     * submitted WS-BPEL process definition. See section 5.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00004(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00005 If the portType attribute is included for readability, in a
     * &lt;receive&gt;, &lt;reply&gt;, &lt;invoke&gt;, &lt;onEvent&gt; or
     * &lt;onMessage&gt; element, the value of the portType attribute MUST match
     * the portType value implied by the combination of the specified
     * partnerLink and the role implicitly specified by the activity. See
     * section 5.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00005(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00006 The &lt;rethrow&gt; activity MUST only be used within a
     * faultHandler (i.e. &lt;catch&gt; and &lt;catchAll&gt; elements). See
     * section 5.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00006(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00007 The &lt;compensateScope&gt; activity MUST only be used from
     * within a faultHandler, another compensationHandler, or a
     * terminationHandler. See section 5.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00007(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00008 The &lt;compensate&gt; activity MUST only be used from within a
     * faultHandler, another compensationHandler, or a terminationHandler. See
     * section 5.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00008(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00009 In the case of mandatory extensions declared in the
     * &lt;extensions&gt; element not supported by a WS-BPEL implementation, the
     * process definition MUST be rejected. See section 5.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00009(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00010 A WS-BPEL process definition MUST import all XML Schema and WSDL
     * definitions it uses. This includes all XML Schema type and element
     * definitions, all WSDL port types and message types as well as property
     * and property alias definitions used by the process. See section 5.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00010(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00011 If a namespace attribute is specified on an &lt;import&gt; then
     * the imported definitions MUST be in that namespace. See section 5.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00011(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00012 If no namespace is specified then the imported definitions MUST
     * NOT contain a targetNamespace specification. See section 5.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00012(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00013 The value of the importType attribute of element &lt;import&gt;
     * MUST be set to http://www.w3.org/2001/XMLSchema when importing XML Schema
     * 1.0 documents, and to http://schemas.xmlsoap.org/wsdl/ when importing
     * WSDL 1.1 documents. See section 5.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00013(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00014 A WS-BPEL process definition MUST be rejected if the imported
     * documents contain conflicting definitions of a component used by the
     * importing process definition (as could be caused, for example, when the
     * XSD redefinition mechanism is used). See section 5.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00014(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00015 To be instantiated, an executable business process MUST contain
     * at least one &lt;receive&gt; or &lt;pick&gt; activity annotated with a
     * createInstance=&quot;yes&quot; attribute. See section 5.5
     * 
     * @param document
     * @throws Exception
     */
    private void SA00015(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00016 A partnerLink MUST specify the myRole or the partnerRole, or
     * both. See section 6.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00016(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00017 The initializePartnerRole attribute MUST NOT be used on a
     * partnerLink that does not have a partner role. See section 6.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00017(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00018 The name of a partnerLink MUST be unique among the names of all
     * partnerLinks defined within the same immediately enclosing scope. See
     * section 6.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00018(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00019 Either the type or element attributes MUST be present in a
     * &lt;vprop:property&gt; element but not both. See section 7.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00019(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00020 A &lt;vprop:propertyAlias&gt; element MUST use one of the three
     * following combinations of attributes: messageType and part, type or ·
     * element. See section 7.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00020(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00021 Static analysis MUST detect property usages where propertyAliases
     * for the associated variable's type are not found in any WSDL definitions
     * directly imported by the WS-BPEL process. See section 7.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00021(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00022 A WS-BPEL process definition MUST NOT be accepted for processing
     * if it defines two or more propertyAliases for the same property name and
     * WS-BPEL variable type. See section 7.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00022(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00023 The name of a variable MUST be unique among the names of all
     * variables defined within the same immediately enclosing scope. See
     * section 8.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00023(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00024 Variable names are BPELVariableNames, that is, NCNames (as
     * defined in XML Schema specification) but in addition they MUST NOT
     * contain the &quot;.&quot; character. See section 8.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00024(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00025 The messageType, type or element attributes are used to specify
     * the type of a variable. Exactly one of these attributes MUST be used. See
     * section 8.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00025(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00026 Variable initialization logic contained in scopes that contain or
     * whose children contain a start activity MUST only use idempotent
     * functions in the from-spec. See section 8.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00026(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00027 When XPath 1.0 is used as an expression language in WS-BPEL there
     * is no context node available. Therefore the legal values of the XPath
     * Expr (http://www.w3.org/TR/xpath#NT-Expr) production must be restricted
     * in order to prevent access to the context node. Specifically, the
     * &quot;LocationPath&quot; (http://www.w3.org/TR/xpath#NT-LocationPath)
     * production rule of &quot;PathExpr&quot;
     * (http://www.w3.org/TR/xpath#NT-PathExpr) production rule MUST NOT be used
     * when XPath is used as an expression language. See section 8.2.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00027(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00028 WS-BPEL functions MUST NOT be used in joinConditions. Section
     * 8.2.5
     * 
     * @param document
     * @throws Exception
     */
    private void SA00028(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00029 WS-BPEL variables and WS-BPEL functions MUST NOT be used in query
     * expressions of propertyAlias definitions. See section 8.2.6
     * 
     * @param document
     * @throws Exception
     */
    private void SA00029(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00030 The arguments to bpel:getVariableProperty MUST be given as quoted
     * strings. It is therefore illegal to pass into a WS-BPEL XPath function
     * any XPath variables, the output of XPath functions, a XPath location path
     * or any other value that is not a quoted string. See section 8.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00030(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00031 The second argument of the XPath 1.0 extension function
     * bpel:getVariableProperty(string, string) MUST be a string literal
     * conforming to the definition of QName in [XML Namespaces] section 3. See
     * section 8.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00031(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00032 For &lt;assign&gt;, the &lt;from&gt; and &lt;to&gt; element MUST
     * be one of the specified variants. See section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00032(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00033 The XPath expression in &lt;to&gt; MUST begin with an XPath
     * VariableReference. See section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00033(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00034 When the variable used in &lt;from&gt; or &lt;to&gt; is defined
     * using XML Schema types (simple or complex) or element, the part attribute
     * MUST NOT be used. See section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00034(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00035 In the from-spec of the partnerLink variant of &lt;assign&gt; the
     * value &quot;myRole&quot; for attribute endpointReference is only
     * permitted when the partnerLink specifies the attribute myRole. See
     * section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00035(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00036 In the from-spec of the partnerLink variant of &lt;assign&gt; the
     * value &quot;partnerRole&quot; for attribute endpointReference is only
     * permitted when the partnerLink specifies the attribute partnerRole. See
     * section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00036(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00037 In the to-spec of the partnerLink variant of assign only
     * partnerLinks are permitted which specify the attribute partnerRole.
     * See section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00037(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00038 The literal from-spec variant returns values as if it were a
     * from-spec that selects the children of the &lt;literal&gt; element in the
     * WS-BPEL source code. The return value MUST be a single EII or Text
     * Information Item (TII) only. See section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00038(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00039 The first parameter of the XPath 1.0 extension function
     * bpel:doXslTransform(string, node-set, (string, object)*) is an XPath
     * string providing a URI naming the style sheet to be used by the WS-BPEL
     * processor. This MUST take the form of a string literal. See section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00039(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00040 In the XPath 1.0 extension function bpel:doXslTransform(string,
     * node-set, (string, object)*) the optional parameters after the second
     * parameter MUST appear in pairs. An odd number of parameters is not valid.
     * See section 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00040(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00041 For the third and subsequent parameters of the XPath 1.0
     * extension function bpel:doXslTransform(string, node-set, (string,
     * object)*) the global parameter names MUST be string literals conforming
     * to the definition of QName in section 3 of [Namespaces in XML]. Section
     * 8.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00041(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00042 For &lt;copy&gt; the optional keepSrcElementName attribute is
     * provided to further refine the behavior. It is only applicable when the
     * results of both from-spec and to-spec are EIIs, and MUST NOT be
     * explicitly set in other cases. See section 8.4.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00042(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00043 For a copy operation to be valid, the data referred to by the
     * from-spec and the to-spec MUST be of compatible types. The following
     * situations are considered type incompatible: · the selection results of
     * both the from-spec and the to-spec are variables of a WSDL message type,
     * and the two variables are not of the same WSDL message type (two WSDL
     * message types are the same if their QNames are equal). · the selection
     * result of the from-spec is a variable of a WSDL message type and that of
     * the to-spec is not, or vice versa (parts of variables, selections of
     * variable parts, or endpoint references cannot be assigned to/from
     * variables of WSDL message types directly). See section 8.4.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00043(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00044 The name of a &lt;correlationSet&gt; MUST be unique among the
     * names of all &lt;correlationSet&gt; defined within the same immediately
     * enclosing scope. See section 9.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00044(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00045 Properties used in a &lt;correlationSet&gt; MUST be defined using
     * XML Schema simple types. See section 9.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00045(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00046 The pattern attribute used in &lt;correlation&gt; within
     * &lt;invoke&gt; is required for request-response operations, and
     * disallowed when a one-way operation is invoked. See section 9.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00046(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00047 One-way invocation requires only the inputVariable (or its
     * equivalent &lt;toPart&gt; elements) since a response is not expected as
     * part of the operation (see section 10.4. Providing Web Service Operations –
     * Receive and Reply ). Request-response invocation requires both an
     * inputVariable (or its equivalent &lt;toPart&gt; elements) and an
     * outputVariable (or its equivalent &lt;fromPart&gt; elements). If a WSDL
     * message definition does not contain any parts, then the associated
     * attributes variable, inputVariable or outputVariable, MAY be omitted,and
     * the &lt;fromParts&gt; or &lt;toParts&gt; construct MUST be omitted.
     * See section 10.3, 10.4, 10.4, 11.5 and 12.7
     * 
     * @param document
     * @throws Exception
     */
    private void SA00047(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00048 When the optional inputVariable and outputVariable attributes are
     * being used in an &lt;invoke&gt; activity, the variables referenced by
     * inputVariable and outputVariable MUST be messageType variables whose
     * QName matches the QName of the input and output message type used in the
     * operation, respectively, except as follows: if the WSDL operation used in
     * an &lt;invoke&gt; activity uses a message containing exactly one part
     * which itself is defined using an element, then a variable of the same
     * element type as used to define the part MAY be referenced by the
     * inputVariable and outputVariable attributes respectively. See section
     * 10.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00048(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00050 When &lt;toParts&gt; is, it is required to have a &lt;toPart&gt;
     * for every part in the WSDL message definition; the order in which parts
     * are specified is irrelevant. Parts not explicitly represented by
     * &lt;toPart&gt; elements would result in uninitialized parts in the target
     * anonymous WSDL variable used by the &lt;invoke&gt; or &lt;reply&gt;
     * activity. Such processes with missing &lt;toPart&gt; elements MUST be
     * rejected during static analysis. See section 10.3.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00050(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00051 The inputVariable attribute MUST NOT be used on an Invoke
     * activity that contains &lt;toPart&gt; elements. See section 10.3.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00051(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00052 The outputVariable attribute MUST NOT be used on an
     * &lt;invoke&gt; activity that contains a &lt;fromPart&gt; element. Section
     * 10.3.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00052(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00053 For all &lt;fromPart&gt; elements the part attribute MUST
     * reference a valid message part in the WSDL message for the operation.
     * See section 5.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00053(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00054 For all &lt;toPart&gt; elements the part attribute MUST reference
     * a valid message part in the WSDL message for the operation. See section
     * 5.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00054(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00055 For &lt;receive&gt;, if &lt;fromPart&gt; elements are used on a
     * &lt;receive&gt; activity then the variable attribute MUST NOT be used on
     * the same activity. See section 10.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00055(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00056 A &quot;start activity&quot; is a &lt;receive&gt; or &lt;pick&gt;
     * activity that is annotated with a createInstance=&quot;yes&quot;
     * attribute. Activities other than the following: start activities,
     * &lt;scope&gt;, &lt;flow&gt; and &lt;sequence&gt; MUST NOT be performed
     * prior to or simultaneously with start activities. See section 10.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00056(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00057 If a process has multiple start activities with correlation sets
     * then all such activities MUST share at least one common correlationSet
     * and all common correlationSets defined on all the activities MUST have
     * the value of the initiate attribute be set to &quot;join&quot;. See
     * section 10.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00057(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00058 In a &lt;receive&gt; or &lt;reply&gt; activity, the variable
     * referenced by the variable attribute MUST be a messageType variable whose
     * QName matches the QName of the input (for &lt;receive&gt;) or output (for
     * &lt;reply&gt;) message type used in the operation, except as follows: if
     * the WSDL operation uses a message containing exactly one part which
     * itself is defined using an element, then a WS-BPEL variable of the same
     * element type as used to define the part MAY be referenced by the variable
     * attribute of the &lt;receive&gt; or &lt;reply&gt;activity. See section
     * 10.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00058(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00059 For &lt;reply&gt;, if &lt;toPart&gt; elements are used on a
     * &lt;reply&gt; activity then the variable attribute MUST NOT be used on
     * the same activity. See section 10.4
     * 
     * @param document
     * @throws Exception
     */
    private void SA00059(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00060 The explicit use of messageExchange is needed only where the
     * execution can result in multiple IMA-&lt;reply&gt; pairs (e.g.
     * &lt;receive&gt;-&lt;reply&gt; pair) on the same partnerLink and operation
     * being executed simultaneously. In these cases, the process definition
     * MUST explicitly mark the pairing-up relationship. See section 10.4.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00060(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00061 The name used in the optional messageExchange attribute MUST
     * resolve to a messageExchange declared in a scope (where the process is
     * considered the root scope) which encloses the &lt;reply&gt; activity and
     * its corresponding IMA. See section 10.4.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00061(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00062 If &lt;pick&gt; has a createInstance attribute with a value of
     * yes, the events in the &lt;pick&gt; MUST all be &lt;onMessage&gt; events.
     * See section 11.5
     * 
     * @param document
     * @throws Exception
     */
    private void SA00062(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00063 The semantics of the &lt;onMessage&gt; event are identical to a
     * &lt;receive&gt; activity regarding the optional nature of the variable
     * attribute or &lt;fromPart&gt; elements, if &lt;fromPart&gt; elements on
     * an activity then the variable attribute MUST NOT be used on the same
     * activity (see SA00055). See section 11.5
     * 
     * @param document
     * @throws Exception
     */
    private void SA00063(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00064 For &lt;flow&gt;, a declared link’s name MUST be unique among all
     * &lt;link&gt; names defined within the same immediately enclosing
     * &lt;flow&gt;. See section 11.6
     * 
     * @param document
     * @throws Exception
     */
    private void SA00064(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00065 The value of the linkName attribute of &lt;source&gt; or
     * &lt;target&gt; MUST be the name of a &lt;link&gt; declared in an
     * enclosing &lt;flow&gt; activity. See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00065(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00066 Every link declared within a &lt;flow&gt; activity MUST have
     * exactly one activity within the &lt;flow&gt; as its source and exactly
     * one activity within the &lt;flow&gt; as its target. See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00066(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00067 Two different links MUST NOT share the same source and target
     * activities; that is, at most one link may be used to connect two
     * activities. See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00067(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00068 An activity MAY declare itself to be the source of one or more
     * links by including one or more &lt;source&gt; elements. Each
     * &lt;source&gt; element MUST use a distinct link name. See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00068(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00069 An activity MAY declare itself to be the target of one or more
     * links by including one or more &lt;target&gt; elements. Each
     * &lt;target&gt; element associated with a given activity MUST use a link
     * name distinct from all other &lt;target&gt; elements at that activity.
     * See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00069(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00070 A link MUST NOT cross the boundary of a repeatable construct or
     * the &lt;compensationHandler&gt; element. This means, a link used within a
     * repeatable construct (&lt;while&gt;, &lt;repeatUntil&gt;,
     * &lt;forEach&gt;, &lt;eventHandlers&gt;) or a &lt;compensationHandler&gt;
     * MUST be declared in a &lt;flow&gt; that is itself nested inside the
     * repeatable construct or &lt;compensationHandler&gt;. See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00070(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00071 A link that crosses a &lt;catch&gt;, &lt;catchAll&gt; or
     * &lt;terminationHandler&gt; element boundary MUST be outbound only, that
     * is, it MUST have its source activity within the &lt;faultHandlers&gt; or
     * &lt;terminationHandler&gt;, and its target activity outside of the scope
     * associated with the handler. See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00071(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00072 A &lt;link&gt; declared in a &lt;flow&gt; MUST NOT create a
     * control cycle, that is, the source activity must not have the target
     * activity as a logically preceding activity. See section 11.6.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00072(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00073 The expression for a join condition MUST be constructed using
     * only Boolean operators and the activity's incoming links' status values.
     * See section 11.6.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00073(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00074 The expressions in &lt;startCounterValue&gt; and
     * &lt;finalCounterValue&gt; MUST return a TII (meaning they contain at
     * least one character) that can be validated as a xsd:unsignedInt. Static
     * analysis MAY be used to detect this erroneous situation at design time
     * when possible (for example, when the expression is a constant). Section
     * 11.7
     * 
     * @param document
     * @throws Exception
     */
    private void SA00074(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00075 For the &lt;forEach&gt; activity, &lt;branches&gt; is an integer
     * value expression. Static analysis MAY be used to detect if the integer
     * value is larger than the number of directly enclosed activities of
     * &lt;forEach&gt; at design time when possible (for example, when the
     * branches expression is a constant). See section 11.7
     * 
     * @param document
     * @throws Exception
     */
    private void SA00075(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00076 For &lt;forEach&gt; the enclosed scope MUST NOT declare a
     * variable with the same name as specified in the counterName attribute of
     * &lt;forEach&gt;. See section 11.7
     * 
     * @param document
     * @throws Exception
     */
    private void SA00076(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00077 The value of the target attribute on a &lt;compensateScope&gt;
     * activity MUST refer to the name of an immediately enclosed scope of the
     * scope containing the FCT-handler with the &lt;compensateScope&gt;
     * activity. This includes immediately enclosed scopes of an event handler
     * (&lt;onEvent&gt; or &lt;onAlarm&gt;) associated with the same scope.
     * See section 12.4.3.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00077(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00078 The target attribute of a &lt;compensateScope&gt; activity MUST
     * refer to a scope or an invoke activity with a fault handler or
     * compensation handler. See section 12.4.3.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00078(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00079 The root scope inside a FCT-handler MUST not have a compensation
     * handler. See section 12.4.4.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00079(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00080 There MUST be at least one &lt;catch&gt; or &lt;catchAll&gt;
     * element within a &lt;faultHandlers&gt; element. See section 12.5
     * 
     * @param document
     * @throws Exception
     */
    private void SA00080(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00081 For the &lt;catch&gt; construct; to have a defined type
     * associated with the fault variable, the faultVariable attribute MUST only
     * be used if either the faultMessageType or faultElement attributes, but
     * not both, accompany it. The faultMessageType and faultElement attributes
     * MUST NOT be used unless accompanied by faultVariable attribute. Section
     * 12.5
     * 
     * @param document
     * @throws Exception
     */
    private void SA00081(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00082 The peer-scope dependency relation MUST NOT include cycles. In
     * other words, WS-BPEL forbids a process in which there are peer scopes S1
     * and S2 such that S1 has a peer-scope dependency on S2 and S2 has a
     * peer-scope dependency on S1. See section 12.5.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00082(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00083 An event handler MUST contain at least one &lt;onEvent&gt; or
     * &lt;onAlarm&gt; element. See section 12.7
     * 
     * @param document
     * @throws Exception
     */
    private void SA00083(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00084 The partnerLink reference of &lt;onEvent&gt; MUST resolve to a
     * partner link declared in the process in the following order: the
     * associated scope first and then the ancestor scopes. See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00084(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00085 The syntax and semantics of the &lt;fromPart&gt; elements as used
     * on the &lt;onEvent&gt; element are the same as specified for the receive
     * activity. This includes the restriction that if &lt;fromPart&gt; elements
     * are used on an onEvent element then the variable, element and messageType
     * attributes MUST NOT be used on the same element. See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00085(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00086 For &lt;onEvent&gt;, variables referenced by the variable
     * attribute of &lt;fromPart&gt; elements or the variable attribute of an
     * &lt;onEvent&gt; element are implicitly declared in the associated scope
     * of the event handler. Variables of the same names MUST NOT be explicitly
     * declared in the associated scope.. See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00086(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00087 For &lt;onEvent&gt;, the type of the variable (as specified by
     * the messageType attribute) MUST be the same as the type of the input
     * message defined by operation referenced by the operation attribute.
     * Optionally the messageType attribute may be omitted and instead the
     * element attribute substituted if the message to be received has a single
     * part and that part is defined with an element type. That element type
     * MUST be an exact match of the element type referenced by the element
     * attribute. See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00087(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00088 For &lt;onEvent&gt;, the resolution order of the correlation
     * set(s) referenced by &lt;correlation&gt; MUST be first the associated
     * scope and then the ancestor scopes. See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00088(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00089 For &lt;onEvent&gt;, when the messageExchange attribute is
     * explicitly specified, the resolution order of the message exchange
     * referenced by messageExchange attribute MUST be first the associated
     * scope and then the ancestor scopes. See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00089(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00090 If the variable attribute is used in the &lt;onEvent&gt; element,
     * either the messageType or the element attribute MUST be provided in the
     * &lt;onEvent&gt; element. See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00090(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00091 A scope with the isolated attribute set to &quot;yes&quot; is
     * called an isolated scope. Isolated scopes MUST NOT contain other isolated
     * scopes. See section 12.8
     * 
     * @param document
     * @throws Exception
     */
    private void SA00091(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00092 Within a scope, the name of all named immediately enclosed scopes
     * MUST be unique. See section 12.4.3
     * 
     * @param document
     * @throws Exception
     */
    private void SA00092(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00093 Identical &lt;catch&gt; constructs MUST NOT exist within a
     * &lt;faultHandlers&gt; element. See section 12.5
     * 
     * @param document
     * @throws Exception
     */
    private void SA00093(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00094 For &lt;copy&gt;, when the keepSrcElementName attribute is set to
     * &quot;yes&quot; and the destination element is the Document EII of an
     * element-based variable or an element-based part of a WSDL
     * message-type-based variable, the name of the source element MUST belong
     * to the substitutionGroup of the destination element. This checking MAY be
     * enforced through static analysis of the expression/query language. See
     * section 8.4.2
     * 
     * @param document
     * @throws Exception
     */
    private void SA00094(Document document) throws Exception {
        // TODO: Implement this.

    }

    /**
     * SA00095 For &lt;onEvent&gt;, the variable references are resolved to the
     * associated scope only and MUST NOT be resolved to the ancestor scopes.
     * See section 12.7.1
     * 
     * @param document
     * @throws Exception
     */
    private void SA00095(Document document) throws Exception {
        // TODO: Implement this.

    }

}
