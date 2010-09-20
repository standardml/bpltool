using System.Collections.Generic;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.WorkflowEngine.DataAccess
{
    /// <summary>
    /// This interface provides base for persistence providers such as Xml Repository,
    /// Relational databases.
    /// </summary>
    public interface IDataPresitanceProvider
    {

        DCRSProcess LoadProcess(int processId);

        void SaveProcess(DCRSProcess process);

        DCRSProcess LoadProcessInstance(int processId, int instanceId);

        void SaveProcessInstance(DCRSProcess process);

        void ArchiveProcessInstance(DCRSProcess process);

        Dictionary<int, string> GetProcessList();

        List<int> GetProcessInstancesList(int processId);

        int GetNextProcessInstanceId(int processId);

    }
}
