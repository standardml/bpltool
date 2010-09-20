using System.Configuration;
using System.IO;
using System.Linq;

namespace ITU.DK.DCRS.WorkflowEngine.DataAccess
{
    internal class XmlRepositorySettings
    {
        private readonly string processRepositoryPath;

        private readonly string processInstanceRepositoryPath;

        private readonly string processArchiveRepositoryPath;

        #region Constructor.
        
        
        public XmlRepositorySettings()
        {
            
            processRepositoryPath = ConfigurationManager.AppSettings.Get("XmlProcessRepositoryPath");

            processInstanceRepositoryPath = ConfigurationManager.AppSettings.Get("XmlProcessInstancesRepositoryPath");

            processArchiveRepositoryPath = ConfigurationManager.AppSettings.Get("XmlProcessArchiveRepositoryPath");
        }

        #endregion

        #region Public Properties.

        public string ProcessArchiveRepositoryPath
        {
            get { return processArchiveRepositoryPath; }
        }

        public string ProcessInstanceRepositoryPath
        {
            get { return processInstanceRepositoryPath; }
        }

        public string ProcessRepositoryPath
        {
            get { return processRepositoryPath; }
        }
        
        #endregion

        #region public Methods.

        public string GetProcessInstanceSavePath(string modelName, int processId, int processInstanceId)
        {
            // (model name).{pid#3#}.[iid#456#].xml
            return string.Format(@"{0}\({1}).[pid#{2}#].[iid#{3}#].Xml", processInstanceRepositoryPath, modelName,
                                 processId, processInstanceId);
        }


        public string GetProcessInstanceArchivePath(string modelName, int processId, int processInstanceId)
        {
            // (model name).{pid#3#}.[iid#456#].xml
            return string.Format(@"{0}\({1}).[pid#{2}#].[iid#{3}#].Xml", processArchiveRepositoryPath, modelName,
                                 processId, processInstanceId);
        }




        public string GetProcessSavePath(string modelName, int processId)
        {
            // (model name).[pid#3#].Xml
            return string.Format(@"{0}\({1}).[pid#{2}#].Xml", processRepositoryPath, modelName,
                                 processId);
        }

        public string GetPathByProcessId(int processId)
        {
            var format = string.Format("[pid#{0}#]", processId);

            return Directory.GetFiles(processRepositoryPath).Where(path => path.Contains(format)).First();

        }

        public string GetPathByProcessInstanceId(int processId, int processInstanceId)
        {
            var format = string.Format("[pid#{0}#].[iid#{1}#]", processId, processInstanceId);

            return Directory.GetFiles(processInstanceRepositoryPath).Where(path => path.Contains(format)).First();

        }


        public string GetArchivePathByProcessInstanceId(int processId, int processInstanceId)
        {
            var format = string.Format("[pid#{0}#].[iid#{1}#]", processId, processInstanceId);

            return Directory.GetFiles(processArchiveRepositoryPath).Where(path => path.Contains(format)).First();

        }




        #endregion

    }
}
