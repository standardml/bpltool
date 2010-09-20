using System.ServiceModel;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{
    [ServiceContract]
    public interface IProcessExecutionNotificationContract
    {

        /// <summary>
        /// This notification will be sent when ever a new instance of process is started successfully.
        /// </summary>
        /// <param name="processInstanceXml"></param>
        [OperationContract(IsOneWay = true)]
        void NewProcessInstanceStarted(string processInstanceXml);

        /// <summary>
        /// This notification will be sent to subscribers, for each action executed successfully
        /// </summary>
        /// <param name="updatedprocessInstanceXml"></param>
        /// <param name="processInstanceId"></param>
        /// <param name="executedAction"></param>
        /// <param name="principal"></param>
        /// <param name="processId"></param>
        [OperationContract(IsOneWay = true)]
        void ActionExecuted(string processId, string processInstanceId, short executedAction, string principal, string updatedprocessInstanceXml);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="processId"></param>
        /// <param name="processInstanceId"></param>
        [OperationContract(IsOneWay = true)]
        void ProcessInstanceClosedAndArchived(string processId, string processInstanceId);




    }
}
