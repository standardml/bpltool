using System;
using System.Collections.Generic;
using System.ServiceModel;
using System.Threading;
using System.Windows.Forms;
using DCRSModelCheckerUI.HelperClasses;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace DCRSModelCheckerUI.Services
{
    [ServiceBehavior(InstanceContextMode = InstanceContextMode.PerCall)]
    class NotificationContract : IRepositoryNotificationContract
    {
        #region Implementation of IRepositoryNotificationContract

        /// <summary>
        /// This will be a call back function/ client side function called from 
        /// DCRS workflow engine to inform that process repository has been updated.  
        /// </summary>
        public void ProcessRepositoryUpdated(Dictionary<int, string> processLsit)
        {

            //ThreadSafeCallHandler.UpdateListWithProcessDictionaryValues(FormMain.GetProcessRepositoryForm().listBoxProcesses, processLsit);

        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="processXml"></param>
        public void DCRSProcessUpdated(string processXml)
        {
            Console.WriteLine("{0}{0}", Environment.NewLine);
            Console.WriteLine("DCRSProcessUpdated() called! Process updated. Xml: {0}", processXml);
            Console.WriteLine("{0}{0}", Environment.NewLine);

            //ThreadSafeCallHandler.SetControlText(FormMain.GetSpecificationForm().textBoxTrace,
            //                               string.Format(
            //                                   "DCRSProcessUpdated() called! Process updated. Xml: {0}", processXml));


        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="processInstanceXml"></param>
        public void ProcessInstanceUpdated(string processInstanceXml)
        {
            //Console.WriteLine("{0}{0}", Environment.NewLine);
            //Console.WriteLine("ProcessInstanceUpdated() called! Process instance updated. Xml: {0}", processInstanceXml);
            //Console.WriteLine("{0}{0}", Environment.NewLine);

            //ThreadSafeCallHandler.SetTextboxText(FormMain.GetSpecificationForm().textBoxTrace,
            //                   string.Format(
            //                       "ProcessInstanceUpdated() called! Process instance updated. Xml: {0}", processInstanceXml));

            var processInstance = DCRSProcess.Deserialize(processInstanceXml);

            var instanceFormName = string.Format("FormInstance{0}.{1}", processInstance.Specification.ProcessId,
                                                    processInstance.Runtime.ProcessInstanceId);

            var form = Application.OpenForms[instanceFormName];

            if((form == null) || !( form is FormProcessInstance))
            {
                var formProcessInstance = new FormProcessInstance(processInstance);

                formProcessInstance.LoadProcessInstance();
            }
            else
            {
                var formProcessInstance = form as FormProcessInstance;

                formProcessInstance.DCRSProcessInstance = processInstance;

                formProcessInstance.LoadProcessInstance();

                
            }

        }


        /// <summary>
        /// This will be a call back function/ client side function called from 
        /// DCRS workflow engine to inform that a particular process instance has been updated.  
        /// </summary>
        public void ProcessInstanceRepositoryUpdated(int processId, List<int> processInstancesList)
        {
            Console.WriteLine("{0}{0}", Environment.NewLine);
            Console.WriteLine(
                "ProcessInstanceRepositoryUpdated() called! There are {0} number of processes instance for ProcessId: {1}",
                 processInstancesList.Count, processId);
            Console.WriteLine("{0}{0}", Environment.NewLine);


            //ThreadSafeCallHandler.SetControlText(FormMain.GetSpecificationForm().textBoxTrace,
            //                               string.Format(
            //                                   "ProcessInstanceRepositoryUpdated() called! There are {0} number of processes instance for ProcessId: {1}",
            //                                   processInstancesList.Count, processId));


        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="errorMessage"></param>
        public void ProcessRepositoryException(string errorMessage)
        {
            Console.WriteLine("{0}{0}", Environment.NewLine);
            Console.WriteLine("ProcessRepositoryException() called. Error message: {0}", errorMessage);
            Console.WriteLine("{0}{0}", Environment.NewLine);

            //ThreadSafeCallHandler.SetControlText(FormMain.GetSpecificationForm().textBoxTrace,
            //                               string.Format("ProcessRepositoryException() called. Error message: {0}",
            //                                             errorMessage
            //                                   ));

        }


        #endregion
    }
}
