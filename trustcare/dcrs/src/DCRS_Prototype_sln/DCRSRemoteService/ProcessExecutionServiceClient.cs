using System;
using System.Collections.Generic;
using System.Linq;
using System.ServiceModel;
using System.Text;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace ITU.DK.DCRS.RemoteServices
{
    class ProcessExecutionServiceClient : ClientBase<IProcessExecutionServiceContract>, IProcessExecutionServiceContract
    {
        #region Implementation of IProcessExecutionServiceContract

        /// <summary>
        /// This method will start a new instance of given processId.
        /// </summary>
        /// <param name="processId"></param>
        /// <returns>The method will return the process instance id of newly started instance.</returns>
        public int StartNewInstance(int processId)
        {
            return Channel.StartNewInstance(processId);
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
            return Channel.ExecuteAction(processId, processInstanceId, action, principal);
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
            return Channel.CloseAndArchiveProcessInstance(processId, processInstanceId);
        }

        #endregion
    }
}
