using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace ITU.DK.DCRS.WorkflowEngine.Core
{
    public class ProcessExecutionHandler
    {

        #region Public static Methods.
        
        public static int StartNewInstance(int processId)
        {

            //Load the process from Repository.
            var process = DCRSWorkflowEngine.ProcessRepositoryProvider.LoadProcess(processId);

            // Get the default parent statevector for the start state.
            var parentStateVector = GetDefaultParentVectorForStartState(process.Specification);

            // Get max Instance Id for instance.
            var instanceId = DCRSWorkflowEngine.ProcessRepositoryProvider.GetNextProcessInstanceId(processId);


            // Compute start state by using FinitStateProvider.
            var finiteStateProvider = new DCRSFiniteStateProvider(0, -1, parentStateVector,
                                                                                      process.Specification);

            var dcrsState = finiteStateProvider.ComputeState();

            // Change the runtime.
            process.Runtime = new DCRSRuntime(dcrsState, instanceId, string.Empty);

            DCRSWorkflowEngine.ProcessRepositoryProvider.SaveProcessInstance(process);

            return instanceId;

        }


        public static TaskResult ExecuteAction(int processId, int processInstanceId,
            short action, string principal)
        {
            // load the process instance.
            var processInstance = DCRSWorkflowEngine.ProcessRepositoryProvider.LoadProcessInstance(processId,
                                                                                                   processInstanceId);

            
            try
            {

                // Check access rights whether the principal is allowed to execute the action.
                if (!processInstance.Specification.CanExecuteAction(principal, action))
                {

                    return new TaskResult
                               {
                                   Status = false,
                                   Message =
                                       string.Format("The principal: {0} is not authorized to execute action: {1}.",
                                                     principal, processInstance.Specification.ActionList[action])
                               };
                }


                // Check wether the action is enabled.
                if (!processInstance.Runtime.CurrentState.EnabledActions.Contains(action))
                {

                    return new TaskResult
                    {
                        Status = false,
                        Message =
                            string.Format("The action: {0} is not enabled for execution.",
                                          processInstance.Specification.ActionList[action])
                    };
                }



                // Compute start state by using FinitStateProvider.
                // Here the state number should be computed from some algorithm
                // based on the previous states which are same as this.
                // TODO: Develop a algorithm for state number computation. 
                var finiteStateProvider = new DCRSFiniteStateProvider(processInstance.Runtime.CurrentState.StateNumber + 1,
                                                                      action,
                                                                      processInstance.Runtime.CurrentState.StateVector,
                                                                      processInstance.Specification);

                var dcrsUpdatedState = finiteStateProvider.ComputeState();

                var executionTrace = string.IsNullOrEmpty(processInstance.Runtime.ExecutionTrace)
                                         ? action.ToString()
                                         : string.Format("{0},{1}", processInstance.Runtime.ExecutionTrace, action);


                // update the process instance with latest state and save it.
                processInstance.Runtime = new DCRSRuntime(dcrsUpdatedState, processInstanceId, executionTrace);


                DCRSWorkflowEngine.ProcessRepositoryProvider.SaveProcessInstance(processInstance);


                // Return success
                return new TaskResult { Status = true };
            }
            catch (System.Exception exception)
            {

                return new TaskResult
                {
                    Status = false,
                    Message =
                        string.Format("Failed to execute action: {1} with access rights for principal: {0}. Error message: {2}",
                                        principal, processInstance.Specification.ActionList[action],exception.Message)
                };

            }
        }

        public static TaskResult CloseAndArchiveProcessInstance(int processId, int processInstanceId)
        {

            try
            {
                // load the process instance.
                var processInstance = DCRSWorkflowEngine.ProcessRepositoryProvider.LoadProcessInstance(processId,
                                                                                                       processInstanceId);

                // If the state is non accepting, we can not close the instance, so return failure.
                if (!processInstance.Runtime.CurrentState.StateVector.StateAccepting)
                {

                    return new TaskResult
                    {
                        Status = false,
                        Message =
                            string.Format("The current state of the process instance (with process Id: {0} and process instance Id: {1}) is non-accepting. So the process instance cannot be closed and archived!",
                                            processId, processInstanceId)
                    };
                }

                DCRSWorkflowEngine.ProcessRepositoryProvider.ArchiveProcessInstance(processInstance);

                return new TaskResult {Status = true, Message = string.Empty};

            }
            catch (System.Exception exception)
            {
                return new TaskResult
                {
                    Status = false,
                    Message =
                        string.Format("Failed to close and archive process instance with process Id: {0} and process instance Id: {1}. Error message: {2}",
                                        processId,processInstanceId, exception.Message)
                };

                
            }



        }

        #endregion



        #region Private static mehods.
        
        private static DCRSStateVector GetDefaultParentVectorForStartState(DCRSSpecification specification)
        {

            var parentStateVector = new DCRSStateVector();

            // Include all actions from the specification.
            parentStateVector.IncludedActions.AddRange(specification.ActionList.Keys);

            return parentStateVector;

        }

        #endregion

    }
}
