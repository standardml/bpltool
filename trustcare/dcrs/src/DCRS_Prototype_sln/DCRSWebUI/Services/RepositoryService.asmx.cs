using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Services;
using AjaxControlToolkit;
using ITU.DK.DCRS.RemoteServices;
using ITU.DK.DCRS.CommonTypes.Process;

namespace DCRSWebUI.Services
{
    /// <summary>
    /// Summary description for RepositoryService
    /// </summary>
    [WebService(Namespace = "http://tempuri.org/")]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
    [System.ComponentModel.ToolboxItem(false)]
    // To allow this Web Service to be called from script, using ASP.NET AJAX, uncomment the following line. 
    [System.Web.Script.Services.ScriptService]
    public class RepositoryService : System.Web.Services.WebService
    {

        [WebMethod]
        public CascadingDropDownNameValue[] GetProcessInstanceIdsByProcessId(
                string knownCategoryValues,
                string category)
        {
            string[] _categoryValues = knownCategoryValues.Split(':', ';');
            int processId = Convert.ToInt16(_categoryValues[1]);

            List<CascadingDropDownNameValue> values =
                new List<CascadingDropDownNameValue>();

            values.Add(new CascadingDropDownNameValue("New Process Instance...", "-2"));
            foreach (var a in RemoteServicesHandler.GetProcessInstancesList(processId))
                values.Add(new CascadingDropDownNameValue(a.ToString(), a.ToString()));

            return values.ToArray();
        }
    }
}
