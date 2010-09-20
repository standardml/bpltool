using System.Collections.Generic;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{
    [ServiceContract]
    public interface IRepositoryServiceContract
    {
        [OperationContract]
        void ImportSpecification(DCRSProcess process);


        [OperationContract]
        string GetProcess(int processId);

        
        [OperationContract]
        string GetProcessInstance(int processId, int processInstanceId);


        [OperationContract]
        Dictionary<int, string > GetProcessList();


        [OperationContract]
        List<int> GetProcessInstanceList(int processId);

        
    }
}
