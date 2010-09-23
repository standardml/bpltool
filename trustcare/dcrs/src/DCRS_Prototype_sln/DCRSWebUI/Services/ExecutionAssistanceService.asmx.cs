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
    /// Summary description for ExecutionAssistanceService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    [System.Web.Script.Services.ScriptService]
    public class ExecutionAssistanceService : System.Web.Services.WebService
    {

        int processId = -1;
        int processInstanceId = -1;
        private DCRSProcess ActiveProcessInstance { get { return GetActiveProcessInstance(); } }
        private DCRSProcess GetActiveProcessInstance()
        {
            string processInstanceXml = RemoteServicesHandler.GetProcessInstance(processId, processInstanceId);
            DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);
            return DCRSProcessInstance;
        }




        [WebMethod]
        public CascadingDropDownNameValue[] GetActions(
                string knownCategoryValues,
                string category, string contextKey)
        {

            processId = Int16.Parse(contextKey.Split('.')[0]);
            processInstanceId = Int16.Parse(contextKey.Split('.')[1]);

            if (processId != -1 && processInstanceId != -1)
            {
                string[] _categoryValues = knownCategoryValues.Split(':', ';');
                string selectedPrincipal =_categoryValues[1];

                DCRSProcess p = ActiveProcessInstance;
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
