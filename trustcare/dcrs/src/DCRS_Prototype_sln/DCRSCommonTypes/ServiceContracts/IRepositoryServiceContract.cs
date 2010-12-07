using System.Collections.Generic;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{
    [ServiceContract]
    public interface IRepositoryServiceContract
    {
        [OperationContract]
        void ImportSpecification(string process);


        [OperationContract]
        string GetProcess(int processId);

        [OperationContract]
        string NewProcess();
        
        [OperationContract]
        void ImportProcessLayout(string processLayout);

        [OperationContract]
        string GetProcessLayout(int processId, string role);
        
        [OperationContract]
        string GetProcessInstance(int processId, int processInstanceId);


        [OperationContract]
        Dictionary<int, string > GetProcessList();


        [OperationContract]
        List<int> GetProcessInstanceList(int processId);

        
    }
}
