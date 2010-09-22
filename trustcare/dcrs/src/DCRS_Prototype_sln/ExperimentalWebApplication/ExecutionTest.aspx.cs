using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using ITU.DK.DCRS.RemoteServices;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ExperimentalWebApplication
{
    public partial class ExecutionTest : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            Session.Add("processId", 5);
        }

        protected void Button1_Click(object sender, EventArgs e)
        {
            var processInstanceId = RemoteServicesHandler.StartNewInstance(5);
            Session.Add("processInstanceId", processInstanceId);


            ListBox1.Items.Clear();
            string processInstanceXml = RemoteServicesHandler.GetProcessInstance(5, processInstanceId);
            DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);
            foreach (var a in DCRSProcessInstance.Runtime.CurrentState.EnabledActions)
            {
                ListBox1.Items.Add(new ListItem(DCRSProcessInstance.Specification.ActionList[a], a.ToString()));
            }
            
        }

        protected void ListBox1_Load(object sender, EventArgs e)
        {

        }

        protected void Button2_Click(object sender, EventArgs e)
        {
            int processInstanceId = (int)Session["processInstanceId"];

            short actionId = Int16.Parse(ListBox1.Items[ListBox1.SelectedIndex].Value);
            
            //get the principals name, improve this eventaully in the real version....
            string processInstanceXml = RemoteServicesHandler.GetProcessInstance(5, processInstanceId);
            DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);



            string principal = DCRSProcessInstance.Specification.RolesToPrincipalsDictionary[DCRSProcessInstance.Specification.ActionsToRolesDictionary[actionId][0]][0];
            var actionExecuteResult = RemoteServicesHandler.ExecuteAction(5, processInstanceId, actionId, principal);

            ListBox1.Items.Clear();
            processInstanceXml = RemoteServicesHandler.GetProcessInstance(5, processInstanceId);
            DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);
            foreach (var a in DCRSProcessInstance.Runtime.CurrentState.EnabledActions)
            {
                ListBox1.Items.Add(new ListItem(DCRSProcessInstance.Specification.ActionList[a], a.ToString()));
            }

        }
    }
}