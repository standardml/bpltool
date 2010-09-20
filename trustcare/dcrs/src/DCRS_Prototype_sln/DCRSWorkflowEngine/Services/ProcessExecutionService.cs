using System;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ITU.DK.DCRS.WorkflowEngine.Core;

namespace ITU.DK.DCRS.WorkflowEngine.Services
{
    /// <summary>
    /// 
    /// </summary>
    [ServiceBehavior(ConcurrencyMode = ConcurrencyMode.Multiple)]
    public class ProcessExecutionService : IProcessExecutionServiceContract
    {
        #region Implementation of IProcessExecutionServiceContract

        /// <summary>
        /// This method will start a new instance of given processId.
        /// </summary>
        /// <param name="processId"></param>
        /// <returns>The method will return the process instance id of newly started instance.</returns>
        public int StartNewInstance(int processId)
        {
            return ProcessExecutionHandler.StartNewInstance(processId);
        }

        /// <summary>
        /// This method will try to execute the given action with the given principal
        /// </summary>
        /// <param name="processId">ProcessId of the DCRS Process</param>
        /// <param name="processInstanceId">InstanceId of the DCRS Process</param>
        /// <param name="action">The Id of the action to be executed.</param>
        /// <param name="principal"></param>
        /// <returns></returns>
        public TaskResult ExecuteAction(int processId, int processInstanceId, short action, string principal)
        {
            return ProcessExecutionHandler.ExecuteAction(processId, processInstanceId, action, principal);

        }

        /// <summary>
        /// This method will close the process instance if the current/ending state is accepting and then
        /// archives the process instance by moving it to archive folder.
        /// </summary>
        /// <param name="processId"></param>
        /// <param name="processInstanceId"></param>
        /// <returns></returns>
        public TaskResult CloseAndArchiveProcessInstance(int processId, int processInstanceId)
        {
            return ProcessExecutionHandler.CloseAndArchiveProcessInstance(processId, processInstanceId);
        }

        #endregion
    }
}
