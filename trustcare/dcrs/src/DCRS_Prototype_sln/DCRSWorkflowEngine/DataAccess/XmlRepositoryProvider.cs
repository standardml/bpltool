using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using ITU.DK.DCRS.CommonTypes.Exceptions;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ITU.DK.DCRS.WorkflowEngine.EventManagement;

namespace ITU.DK.DCRS.WorkflowEngine.DataAccess
{
    /// <summary>
    /// This class is designed as singleton class.
    /// </summary>
    internal class XmlRepositoryProvider : IDataPresitanceProvider
    {

        private static readonly XmlRepositoryProvider REPOSITORY_PROVIDER = new XmlRepositoryProvider();

        private readonly XmlRepositorySettings repositorySettings;

        #region private Constructor

        private XmlRepositoryProvider()
        {
            repositorySettings = new XmlRepositorySettings();

        }

        #endregion



        #region Static Property to supply XmlRepositoryProvider instance.

        public static XmlRepositoryProvider RepositoryProvider
        {
            get { return REPOSITORY_PROVIDER; }
        }

        #endregion

        #region Implementation of IDataPresitanceProvider

        public DCRSProcess LoadProcess(int processId)
        {
            try
            {
                var path = repositorySettings.GetPathByProcessId(processId);

                if (string.IsNullOrEmpty(path))
                    throw new DCRSWorkflowException(
                        string.Format("The process with processId: {0} does not exists in the Repository.", processId));

                return DCRSProcess.Load(path);

            }
            catch (Exception exception)
            {

                var workflowException = new DCRSWorkflowException(
                    string.Format("Failed to load DCRS process with processId: {0} . Error message: {1}",
                                  processId,
                                  exception.Message));

                //InvokeRepositoryErrorEvent(workflowException);

                throw workflowException;

            }
        }


        public DCRSProcess NewProcess()
        {
            try
            {
                var path = repositorySettings.GetPathByProcessId(1);

                if (string.IsNullOrEmpty(path))
                    throw new DCRSWorkflowException(
                        string.Format("The process with processId: {0} does not exists in the Repository.", 1));

                DCRSProcess process = DCRSProcess.Load(path);

                var fileNames = Directory.GetFiles(repositorySettings.ProcessRepositoryPath);

                int newId = 0;

                foreach (var fileName in fileNames)
                {
                    string modelName;

                    int processId;

                    GetProcessIdAndModelName(fileName, out modelName, out processId);

                    if (processId != 0) 
                    {
                        newId = Math.Max(processId, newId);
                    }

                }

                newId++;

                process.Specification.ProcessId = newId;

                path = repositorySettings.GetProcessSavePath(process.Specification.ModelName,
                                                         process.Specification.ProcessId);
                DCRSProcess.Save(process, path);

                // Invoke ProcessRepositoryUpdated event to intimate to the listeners that process repository has been updated.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname("ProcessRepositoryUpdated",
                                                                                        GetProcessList());

                // Invoke DCRSProcessUpdated event to intimate to the listeners.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname("DCRSProcessUpdated",
                                                                                        DCRSProcess.Serialize(process));
                
                return process;

            }
            catch (Exception exception)
            {

                var workflowException = new DCRSWorkflowException(
                    string.Format("Failed to create a new process. Error message: {1}",                                  
                                  exception.Message));

                //InvokeRepositoryErrorEvent(workflowException);

                throw workflowException;

            }
        }

        public void SaveProcess(DCRSProcess process)
        {
            try
            {
                var path = repositorySettings.GetProcessSavePath(process.Specification.ModelName,
                                                         process.Specification.ProcessId);
                DCRSProcess.Save(process, path);

                // Invoke ProcessRepositoryUpdated event to intimate to the listeners that process repository has been updated.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname("ProcessRepositoryUpdated",
                                                                                        GetProcessList());

                // Invoke DCRSProcessUpdated event to intimate to the listeners.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname("DCRSProcessUpdated",
                                                                                        DCRSProcess.Serialize(process));

            }
            catch (Exception exception)
            {
                var workflowException = new DCRSWorkflowException(
                    string.Format("Failed to save DCRS process with processId: {0} . Error message: {1}",
                                  process.Specification.ProcessId,
                                  exception.Message));

                // Invoke Error message event to notify to the listeners.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname("ProcessRepositoryException",
                                                                                        workflowException.Message);

                throw workflowException;


            }
        }

        public DCRSProcess LoadProcessInstance(int processId, int instanceId)
        {

            try
            {
                var path = repositorySettings.GetPathByProcessInstanceId(processId, instanceId);

                if (string.IsNullOrEmpty(path))
                    throw new DCRSWorkflowException(
                        string.Format(
                            "The process with processId: {0} and process instance Id: {1} does not exists in the Repository.",
                            processId, instanceId));

                return DCRSProcess.Load(path);

            }
            catch (Exception exception)
            {

                var workflowException = new DCRSWorkflowException(
                    string.Format(
                        "Failed to load DCRS process instance with processId: {0}, processInstanceId: {1} . Error message: {2}",
                        processId, instanceId,
                        exception.Message));

                //InvokeRepositoryErrorEvent(workflowException);

                throw workflowException;

            }
        }

        public void SaveProcessInstance(DCRSProcess process)
        {

            try
            {
                var path = repositorySettings.GetProcessInstanceSavePath(process.Specification.ModelName,
                                                                 process.Specification.ProcessId,
                                                                 process.Runtime.ProcessInstanceId);
                DCRSProcess.Save(process, path);

                // Invoke static Process Updated event to intimate to the listeners.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname(
                    "ProcessInstanceRepositoryUpdated", process.Specification.ProcessId,
                    GetProcessInstancesList(process.Specification.ProcessId));

                // Invoke DCRSProcessUpdated event to intimate to the listeners.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname(
                    "ProcessInstanceUpdated",
                    DCRSProcess.Serialize(process));


            }
            catch (Exception exception)
            {

                var workflowException = new DCRSWorkflowException(
                    string.Format(
                        "Failed to save DCRS process instance with processId: {0}, processInstanceId: {1} . Error message: {2}",
                        process.Specification.ProcessId, process.Runtime.ProcessInstanceId,
                        exception.Message));

                // Invoke Error message event to notify to the listeners.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname("ProcessRepositoryException",
                                                                                        workflowException.Message);

                throw workflowException;

            }
        }

        public void ArchiveProcessInstance(DCRSProcess process)
        {
            try
            {
                var archivePath = repositorySettings.GetProcessInstanceArchivePath(process.Specification.ModelName,
                                                                 process.Specification.ProcessId,
                                                                 process.Runtime.ProcessInstanceId);
                DCRSProcess.Save(process, archivePath);

                // Delete the file from process instance repository.
                var processInstancePath = repositorySettings.GetProcessInstanceSavePath(process.Specification.ModelName,
                                                                 process.Specification.ProcessId,
                                                                 process.Runtime.ProcessInstanceId);

                // If the process instance path is avialble, delete process instance from the instances repository!
                if (File.Exists(processInstancePath)) File.Delete(processInstancePath);


                // Invoke static Process Updated event to intimate to the listeners that process
                // instance repository has been updated.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname(
                    "ProcessInstanceRepositoryUpdated", process.Specification.ProcessId,
                    GetProcessInstancesList(process.Specification.ProcessId));


            }
            catch (Exception exception)
            {

                var workflowException = new DCRSWorkflowException(
                    string.Format(
                        "Failed to save DCRS process instance with processId: {0}, processInstanceId: {1} . Error message: {2}",
                        process.Specification.ProcessId, process.Runtime.ProcessInstanceId,
                        exception.Message));

                // Invoke Error message event to notify to the listeners.
                PublishService<IRepositoryNotificationContract>.FireEventWithMethodname("ProcessRepositoryException",
                                                                                        workflowException.Message);

                throw workflowException;

            }

        }

        public Dictionary<int, string> GetProcessList()
        {

            try
            {
                var processDictionary = new Dictionary<int, string>();

                var fileNames = Directory.GetFiles(repositorySettings.ProcessRepositoryPath);

                foreach (var fileName in fileNames)
                {
                    string modelName;

                    int processId;

                    GetProcessIdAndModelName(fileName, out modelName, out processId);

                    if ((processId != 0) && (!processDictionary.ContainsKey(processId)))
                        processDictionary.Add(processId, modelName);

                }

                return processDictionary;
            }
            catch (Exception exception)
            {

                var workflowException = new DCRSWorkflowException("Failed to get process list from Repository. Error message: " +
                                                exception.Message);

                //InvokeRepositoryErrorEvent(workflowException);

                throw workflowException;

            }

        }

        public List<int> GetProcessInstancesList(int processId)
        {

            try
            {
                var processInstancesList = new List<int>();

                var format = string.Format("[pid#{0}#]", processId);

                // Get FileNames containing ProcessId...
                var fileNames =
                    Directory.GetFiles(repositorySettings.ProcessInstanceRepositoryPath).Where(path => path.Contains(format));

                foreach (var fileName in fileNames)
                {
                    int processInstanceId;

                    GetProcessInstanceId(fileName, out processInstanceId);

                    if (processInstanceId != 0)
                        processInstancesList.Add(processInstanceId);
                }

                return processInstancesList;
            }
            catch (Exception exception)
            {

                var workflowException =  new DCRSWorkflowException("Failed to get process instances list from Repository. Error message: " +
                                                exception.Message);

                //InvokeRepositoryErrorEvent(workflowException);

                throw workflowException;

            }
        }

        public int GetNextProcessInstanceId(int processId)
        {
            // Return the next instance Id or 1, if we dont find any instances
            return (GetProcessInstancesList(processId).Count > 0) ? GetProcessInstancesList(processId).Max() + 1 : 1;
        }

        #endregion

        #region Private Static Methods.
        
        private static int GetNumericValue(string numericString)
        {
            int returnValue;

            int.TryParse(numericString, out returnValue);

            return returnValue;
        }

        private static void GetProcessIdAndModelName(string filename, out string modelName, out int processId)
        {
            const string modelNamePattern = @"\((?<ModelNameInsideBrackets>\w+)\)";

            modelName = GetPatternMatchValue(modelNamePattern, filename, "ModelNameInsideBrackets");

            const string processIdPattern = @"pid#(?<PIDWithInHashes>\w+)#";

            processId = GetNumericValue(GetPatternMatchValue(processIdPattern, filename, "PIDWithInHashes"));


        }

        private static void GetProcessInstanceId(string filename, out int processId)
        {

            const string processIdPattern = @"iid#(?<IIDWithinHashes>\w+)#";

            processId = GetNumericValue(GetPatternMatchValue(processIdPattern, filename, "IIDWithinHashes"));
        }

        private static string GetPatternMatchValue(string pattern, string inputString, string groupName)
        {
            var regex = new Regex(pattern);

            var match = regex.Match(inputString);

            return !match.Success ? string.Empty : match.Groups[groupName].Value;
        }
        #endregion

    }
}

//#region Static Events & Event Raiser Functions.
///// <summary>
///// This event will be fired when a process is updated in the repository.
///// </summary>
//public static event ProcessEventHandler ProcessUpdated;

//public static void InvokeProcessUpdated(DCRSProcess process)
//{
//    if (ProcessUpdated != null)
//    {
//        ProcessUpdated(null, new ProcessEventArgs { Process = process });
//    }
//}

///// <summary>
///// This event will be fired when the Repository finished loading of Rules.
///// </summary>
//public static event ProcessEventHandler ProcessInstanceUpdated;

//public static void InvokeProcessInstanceUpdated(DCRSProcess processInstance)
//{
//    if (ProcessInstanceUpdated != null)
//    {
//        ProcessInstanceUpdated(null, new ProcessEventArgs { Process = processInstance });
//    }

//}

///// <summary>
///// This event will be fired when the Rule Repostory met with a fetal error.
///// </summary>
//public static event ErrorEventHandler RepositoryErrorEvent;

//public static void InvokeRepositoryErrorEvent(Exception exception)
//{
//    var handler = RepositoryErrorEvent;
//    if (handler != null) handler(null, new ErrorEventArgs(exception));
//}

//#endregion
