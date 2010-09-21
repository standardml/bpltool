﻿using System.Collections.Generic;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace ExperimentalWebApplication.Services
{
    internal class RepositoryServiceClient : ClientBase<IRepositoryServiceContract>, IRepositoryServiceContract
    {
        #region Implementation of IRepositoryServiceContract

        public void ImportSpecification(DCRSProcess process)
        {
            Channel.ImportSpecification(process);
        }

        public string GetProcess(int processId)
        {
            return Channel.GetProcess(processId);
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
