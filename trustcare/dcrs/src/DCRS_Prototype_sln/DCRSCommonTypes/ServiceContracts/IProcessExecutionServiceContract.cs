using System.ServiceModel;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{
    [ServiceContract]
    public interface IProcessExecutionServiceContract
    {
        /// <summary>
        /// This method will start a new instance of given processId.
        /// </summary>
        /// <param name="processId"></param>
        /// <returns>The method will return the process instance id of newly started instance.</returns>
        [OperationContract]
        int StartNewInstance(int processId);

        /// <summary>
        /// This method will try to execute the given action with the given principal
        /// </summary>
        /// <param name="processId">ProcessId of the DCRS Process</param>
        /// <param name="processInstanceId">InstanceId of the DCRS Process</param>
        /// <param name="action">The Id of the action to be executed.</param>
        /// <param name="principal"></param>
        /// <returns></returns>
        [OperationContract]
        TaskResult ExecuteAction(int processId, int processInstanceId, 
            short action, string principal);

        /// <summary>
        /// This method will close the process instance if the current/ending state is accepting and then
        /// archives the process instance by moving it to archive folder.
        /// </summary>
        /// <param name="processId"></param>
        /// <param name="processInstanceId"></param>
        /// <returns></returns>
        [OperationContract]
        TaskResult CloseAndArchiveProcessInstance(int processId, int processInstanceId);




    }
}
