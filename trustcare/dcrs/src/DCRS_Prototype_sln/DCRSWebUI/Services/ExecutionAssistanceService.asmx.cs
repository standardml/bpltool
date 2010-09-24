using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;
using AjaxControlToolkit;
using ITU.DK.DCRS.RemoteServices;
using ITU.DK.DCRS.CommonTypes.Process;


namespace DCRSWebUI
{
    /// <summary>
    /// Web service has functions for assisting in the execution of DRCS instances.
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    [System.Web.Script.Services.ScriptService]
    public class ExecutionAssistanceService : System.Web.Services.WebService
    {
        /// <summary>
        /// Helper method for retrieving a processinstance.
        /// </summary>
        private DCRSProcess GetProcessInstance(int processId, int processInstanceId)
        {
            string processInstanceXml = RemoteServicesHandler.GetProcessInstance(processId, processInstanceId);
            DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);
            return DCRSProcessInstance;
        }


        /// <summary>
        /// Method for getting available actions, given a process, process instance and principal.
        /// </summary>
        /// <param name="knownCategoryValues">Contains the selected principal.</param>
        /// <param name="category"></param>
        /// <param name="contextKey">Contains the processId and processInstanceId, formatted as "[processId].[processInstanceId]" </param>
        /// <returns></returns>
        [WebMethod]
        public CascadingDropDownNameValue[] GetActions(
                string knownCategoryValues,
                string category, string contextKey)
        {

            int processId = Int16.Parse(contextKey.Split('.')[0]);
            int processInstanceId = Int16.Parse(contextKey.Split('.')[1]);

            if (processId != -1 && processInstanceId != -1)
            {
                string[] _categoryValues = knownCategoryValues.Split(':', ';');
                string selectedPrincipal =_categoryValues[1];

                DCRSProcess p = GetProcessInstance(processId, processInstanceId);
                List<CascadingDropDownNameValue> values =
                  new List<CascadingDropDownNameValue>();

                foreach (var a in p.Runtime.CurrentState.EnabledActions)
                {
                    bool found = false;
                    foreach (string role in p.Specification.ActionsToRolesDictionary[a])
                    {
                        if (found) break;
                        foreach (string principal in p.Specification.RolesToPrincipalsDictionary[role])
                        {
                            if (principal == selectedPrincipal)
                            {
                                found = true;
                                break;
                            }
                        }
                    }
                    if (found)
                        values.Add(new CascadingDropDownNameValue(p.Specification.ActionList[a], a.ToString()));
                }
                return values.ToArray();
            }
            else
            {
                List<CascadingDropDownNameValue> values = new List<CascadingDropDownNameValue>();
                values.Add(new CascadingDropDownNameValue("Process or instance missing", "Process or instance missing"));
                return values.ToArray();
            }
        }
    }
}
