using System.Collections.Generic;
using System.ServiceModel;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{

    [ServiceContract]
    public interface IRepositoryNotificationContract
    {
        /// <summary>
        /// This will be a call back function/ client side function called from 
        /// DCRS workflow engine to inform that process repository has been updated.  
        /// </summary>
        [OperationContract(IsOneWay = true)]
        void ProcessRepositoryUpdated(Dictionary<int, string> processLsit);



        /// <summary>
        /// This will be a call back function/ client side function called from 
        /// DCRS workflow engine to inform that a particular process instance has been updated.  
        /// </summary>
        [OperationContract(IsOneWay = true)]
        void ProcessInstanceRepositoryUpdated(int processId, List<int> processInstancesList);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="processXml"></param>
        [OperationContract(IsOneWay = true)]
        void DCRSProcessUpdated(string processXml);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="processInstanceXml"></param>
        [OperationContract(IsOneWay = true)]
        void ProcessInstanceUpdated(string processInstanceXml);


        /// <summary>
        /// 
        /// </summary>
        /// <param name="errorMessage"></param>
        [OperationContract(IsOneWay = true)]
        void ProcessRepositoryException(string errorMessage);



        
    }
}
