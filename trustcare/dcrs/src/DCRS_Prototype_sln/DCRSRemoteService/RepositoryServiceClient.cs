using System.Collections.Generic;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace ITU.DK.DCRS.RemoteServices
{
    internal class RepositoryServiceClient : ClientBase<IRepositoryServiceContract>, IRepositoryServiceContract
    {
        #region Implementation of IRepositoryServiceContract

        public void ImportSpecification(string process)
        {
            Channel.ImportSpecification(process);
        }

        public string GetProcess(int processId)
        {
            return Channel.GetProcess(processId);
        }

        public void ImportProcessLayout(string processLayout)
        {
            Channel.ImportProcessLayout(processLayout);
        }

        public string GetProcessLayout(int processId, string role)
        {
            return Channel.GetProcessLayout(processId, role);
        }
        
        public string NewProcess()
        {
            return Channel.NewProcess();
        }

        public string GetProcessInstance(int processId, int processInstanceId)
        {
            return Channel.GetProcessInstance(processId, processInstanceId);
        }

        public Dictionary<int, string > GetProcessList()
        {
            return Channel.GetProcessList();
        }

        public List<int> GetProcessInstanceList(int processId)
        {
            return Channel.GetProcessInstanceList(processId);
        }

        #endregion
    }
}
