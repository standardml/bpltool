using System.Collections.Generic;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ITU.DK.DCRS.WorkflowEngine.DataAccess;

namespace ITU.DK.DCRS.WorkflowEngine.Services
{
    public class ProcessRepositoryService : IRepositoryServiceContract
    {
        #region Implementation of IRepositoryServiceContract

        public void ImportSpecification(string process)
        {
            XmlRepositoryProvider.RepositoryProvider.SaveProcess(DCRSProcess.Deserialize(process));
        }

        public string GetProcess(int processId)
        {
            return DCRSProcess.Serialize(XmlRepositoryProvider.RepositoryProvider.LoadProcess(processId));
        }

        public string GetProcessInstance(int processId, int processInstanceId)
        {
            return
                DCRSProcess.Serialize(XmlRepositoryProvider.RepositoryProvider.LoadProcessInstance(processId,
                                                                                                   processInstanceId));
        }

        public Dictionary<int, string> GetProcessList()
        {
            return XmlRepositoryProvider.RepositoryProvider.GetProcessList();
        }

        public List<int> GetProcessInstanceList(int processId)
        {
            return XmlRepositoryProvider.RepositoryProvider.GetProcessInstancesList(processId);
        }

        #endregion
    }
}
