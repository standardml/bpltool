using System;
using System.IO;
using System.Linq;
using System.Xml.Linq;
using ITU.DK.DCRS.CommonTypes.Exceptions;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.CommonTypes.Serialization
{
    public class DCRSRuntimeSerializer
    {

        #region 1. Serialization Methods.
        
        public static XElement SerializeDCRSRuntimeToXElement(DCRSRuntime runtime)
        {

            #region 1. Serializing Transitions..
            // Serialize current-state

            // state-transitions
            var leadingTransitionsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "leading-transitions",
                                                         runtime.CurrentState.LeadingTransitions.Select(
                                                             transition =>
                                                             new XElement(
                                                                 GlobalDeclarations.DCRS_NAMESPACE_URI + "transition",
                                                                 new XAttribute("state-id", transition.StateNumber),
                                                                 new XAttribute("action-id", transition.Label))));



            var followingTransitionsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "following-transitions",
                                             runtime.CurrentState.FollowingTransitions.Select(
                                                 transition =>
                                                 new XElement(
                                                     GlobalDeclarations.DCRS_NAMESPACE_URI + "transition",
                                                     new XAttribute("state-id", transition.StateNumber),
                                                     new XAttribute("action-id", transition.Label))));

            var stateTransitionsElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "state-transitions",
                                                       leadingTransitionsElement, followingTransitionsElement);

            #endregion

            #region 2. Serializing State Vector
            // Serializing DCRS State Vector 
            // actions-included
            var actionsIncludedElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-included",
                                                      runtime.CurrentState.StateVector.IncludedActions.Select(
                                                          action =>
                                                          new XElement(
                                                              GlobalDeclarations.DCRS_NAMESPACE_URI + "action",
                                                              new XAttribute("action-id", action))));


            // actions-executed
            var actionsExecutedElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-executed",
                                                      runtime.CurrentState.StateVector.ExecutedActions.Select(
                                                          action =>
                                                          new XElement(
                                                              GlobalDeclarations.DCRS_NAMESPACE_URI + "action",
                                                              new XAttribute("action-id", action))));


            // actions-pending-response
            var actionsPendingElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-pending-response",
                                                      runtime.CurrentState.StateVector.PendingResponseActions.Select(
                                                          action =>
                                                          new XElement(
                                                              GlobalDeclarations.DCRS_NAMESPACE_URI + "action",
                                                              new XAttribute("action-id", action))));

            // state-accepting
            var stateAcceptingElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "state-accepting",
                                                     runtime.CurrentState.StateVector.StateAccepting ? 1 : 0);



            // state-index
            var stateIndexElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "state-index",
                                                 runtime.CurrentState.StateVector.StateRank);


            #endregion

            #region 3. Actions Enabled.

            // actions-enabled
            var actionsEnabledElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-enabled",
                                                      runtime.CurrentState.EnabledActions.Select(
                                                          action =>
                                                          new XElement(
                                                              GlobalDeclarations.DCRS_NAMESPACE_URI + "action",
                                                              new XAttribute("action-id", action))));

            #endregion


            var currentStateElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "current-state",
                                                   new XAttribute("state-id", runtime.CurrentState.StateNumber),
                                                   stateTransitionsElement, actionsIncludedElement,
                                                   actionsExecutedElement, actionsPendingElement, stateAcceptingElement,
                                                   stateIndexElement, actionsEnabledElement);
            // execution-trace
            var exeTraceElement = new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "execution-trace",
                                               runtime.ExecutionTrace);

            // Create dcrs-runtime element.
            return new XElement(GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-runtime",
                                new XAttribute("process-instance-id", runtime.ProcessInstanceId),
                                exeTraceElement, currentStateElement);
        }

        public static string SerializeDCRSRuntimeToString(DCRSRuntime runtime)
        {

            var runtimeElement = SerializeDCRSRuntimeToXElement(runtime);

            var stringWriter = new StringWriter();

            runtimeElement.Save(stringWriter);

            var runtimeXml = stringWriter.ToString();

            stringWriter.Close();

            return runtimeXml;



        }

        #endregion


        #region 2. Deserialization Methods.

        public static DCRSRuntime DeserializeDCRSRuntimeXml(string dcrsXml)
        {
            if (string.IsNullOrEmpty(dcrsXml)) throw new ArgumentNullException("dcrsXml");


            DCRSRuntime runtime;

            try
            {
                DCRSState dcrsState;

                var reader = new StringReader(dcrsXml);

                // Here the root could be dcrs-runtime or any top level element like dcrs-process
                // So we code assuming that it is not the dcrs-runtime element.
                var rootElement = XElement.Load(reader);

                #region 1. Deserialize state-id

                try
                {
                    // Extract the state-id.
                    dcrsState = new DCRSState()
                    {
                        StateNumber = Convert.ToInt64(rootElement.DescendantsAndSelf(
                                GlobalDeclarations.DCRS_NAMESPACE_URI + "current-state").
                            Select(specElement => specElement.Attribute("state-id").Value).Single())
                    };
                }
                catch (Exception exception)
                {
                    throw new DCRSWorkflowException(
                        string.Format(
                            "Failed to extract state-id value. Look for specifically invalid data in attributes on <current-state> section of Xml input. Error message: {0}. ",
                            DCRSWorkflowException.GetCombinedMessage(exception)));
                }

                #endregion

                #region 2. Deserialize state-transitions.

                try
                {
                    // Deserializing state-transitions.
                    dcrsState.LeadingTransitions.AddRange(rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "leading-transitions").Elements().
                        Select(transitionElement => new StateTransition
                        {
                            Label = !string.IsNullOrEmpty(
                                    transitionElement.Attribute("action-id").Value)
                                    ? short.Parse(transitionElement.Attribute("action-id").Value)
                                    : short.MinValue,
                            StateNumber = !string.IsNullOrEmpty(transitionElement.Attribute("state-id").Value)
                                        ? short.Parse(transitionElement.Attribute("state-id").Value)
                                        : long.MinValue

                        }));



                    // Deserializing state-transitions.
                    dcrsState.FollowingTransitions.AddRange(rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "following-transitions").Elements().
                        Select(transitionElement => new StateTransition
                        {
                            Label = !string.IsNullOrEmpty(
                                    transitionElement.Attribute("action-id").Value)
                                    ? short.Parse(transitionElement.Attribute("action-id").Value)
                                    : short.MinValue,
                            StateNumber = !string.IsNullOrEmpty(transitionElement.Attribute("state-id").Value)
                                        ? short.Parse(transitionElement.Attribute("state-id").Value)
                                        : long.MinValue

                        }));


                    //foreach (var transitionElement in rootElement.Descendants(GlobalDeclarations.DCRS_NAMESPACE_URI + "leading-transitions").Elements())
                    //{
                    //    var transition = new StateTransition
                    //                         {
                    //                             Label =
                    //                                 !string.IsNullOrEmpty(
                    //                                     transitionElement.Attribute("action-id").Value)
                    //                                     ? short.Parse(transitionElement.Attribute("action-id").Value)
                    //                                     : short.MinValue,
                    //                             StateNumber =
                    //                                 !string.IsNullOrEmpty(transitionElement.Attribute("state-id").Value)
                    //                                     ? short.Parse(transitionElement.Attribute("state-id").Value)
                    //                                     : long.MinValue
                    //                         };

                    //    //dcrsSpecification.Roles.Add(roleElement.Value);
                    //}

                }
                catch (Exception exception)
                {

                    throw new DCRSWorkflowException(
                        string.Format(
                            "Failed to deserialize state-transitions. Look for specifically invalid data in <state-transitions> section of Xml input. Error message: {0}. ",
                            DCRSWorkflowException.GetCombinedMessage(exception)));

                }

                #endregion

                #region 3. Deserialize DCRS State Vector


                try
                {
                    // actions-included
                    dcrsState.StateVector.IncludedActions.AddRange(rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-included").Elements().
                        Select(actionElement => short.Parse(actionElement.Attribute("action-id").Value)));


                    // actions-executed
                    dcrsState.StateVector.ExecutedActions.AddRange(rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-executed").Elements().
                        Select(actionElement => short.Parse(actionElement.Attribute("action-id").Value)));

                    // actions-pending-response
                    dcrsState.StateVector.PendingResponseActions.AddRange(rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-pending-response").Elements().
                        Select(actionElement => short.Parse(actionElement.Attribute("action-id").Value)));

                    // state-accepting
                    dcrsState.StateVector.StateAccepting = (rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "state-accepting").First() != null)
                                                               ? ((rootElement.Descendants(
                                                                   GlobalDeclarations.DCRS_NAMESPACE_URI +
                                                                   "state-accepting").First().Value == "1")
                                                                      ? true
                                                                      : false)
                                                               : false;


                    // state-index
                    dcrsState.StateVector.StateRank = (rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "state-index").First() != null)
                            ? ((rootElement.Descendants(
                                GlobalDeclarations.DCRS_NAMESPACE_URI +
                                "state-index").First().Value != null)
                                    ? short.Parse((rootElement.Descendants(
                                        GlobalDeclarations.DCRS_NAMESPACE_URI +
                                        "state-index").First().Value))
                                    : (short)0)
                            : (short)0;


                    //actions-enabled
                    dcrsState.EnabledActions.AddRange(rootElement.Descendants(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "actions-enabled").Elements().
                        Select(actionElement => short.Parse(actionElement.Attribute("action-id").Value)));

                }
                catch (Exception exception)
                {

                    throw new DCRSWorkflowException(
                        string.Format(
                            "Failed to deserialize State Vector. Look for specifically invalid data in <current-state> section of Xml input. Error message: {0}. ",
                            DCRSWorkflowException.GetCombinedMessage(exception)));

                }

                #endregion

                #region 4. Deserialize dcrs-runtime

                try
                {
                    //var processInstanceId = rootElement.DescendantsAndSelf(
                    //    GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-runtime").First().Attribute("process-instance-id")
                    //    .Value;


                    var processInstanceId = (rootElement.DescendantsAndSelf(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "dcrs-runtime").First().Attribute("process-instance-id") !=
                        null)
                        ? ((rootElement.DescendantsAndSelf(
                            GlobalDeclarations.DCRS_NAMESPACE_URI +
                            "dcrs-runtime").First().Attribute("process-instance-id").Value !=
                            null)
                                ? int.Parse((rootElement.DescendantsAndSelf(
                                    GlobalDeclarations.DCRS_NAMESPACE_URI +
                                    "dcrs-runtime").First().Attribute("process-instance-id").
                                                Value))
                                : int.MinValue)
                        : int.MinValue;


                    var executionTrace = rootElement.DescendantsAndSelf(
                        GlobalDeclarations.DCRS_NAMESPACE_URI + "execution-trace").First().Value;

                    runtime = new DCRSRuntime(dcrsState, processInstanceId, executionTrace);

                }
                catch (Exception exception)
                {
                    throw new DCRSWorkflowException(
                        string.Format(
                            "Failed to extract process-instance-id or execution-trace value. Look for specifically invalid data in attributes on <dcrs-runtime> section of Xml input. Error message: {0}. ",
                            DCRSWorkflowException.GetCombinedMessage(exception)));
                }

                #endregion












            }
            catch (Exception exception)
            {

                throw new DCRSWorkflowException(
                    string.Format("Failed to deserialize DCRS Runtime. Error message: {0}, DCRS Xml:{1}",
                                  DCRSWorkflowException.GetCombinedMessage(exception), dcrsXml));

            }










            return runtime;
        }

        #endregion

        

    }
}
