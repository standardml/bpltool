using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using ITU.DK.DCRS.RemoteServices;
using ITU.DK.DCRS.CommonTypes.Process;

namespace DCRSWebUI
{
    public partial class ProcessExecution : System.Web.UI.Page
    {
        //private int processId { get { if (Session["processId"] != null) return (int)Session["processId"]; else return -1; } }
        //private int processInstanceId { get { if (Session["processInstanceId"] != null) return (int)Session["processInstanceId"]; else return -1; } }
        private bool processIdExists { get { return (Session["processId"] != null); } }
        private bool processInstanceIdExists { get { return (Session["processInstanceId"] != null); } }

        private int processId { get { return (int)Session["processId"]; } }
        private int processInstanceId { get { return (int)Session["processInstanceId"]; } }
        private DCRSProcess ActiveProcessInstance { get { return GetActiveProcessInstance(); } }

        protected void Page_Load(object sender, EventArgs e)
        {
            if (processIdExists && processInstanceIdExists)
            {
                DCRSProcess p = ActiveProcessInstance;
                UpdatePrincipals(p);
            }
            else
            {
                Session.Add("returnPage", "ProcessExecution.aspx");
                Server.Transfer("ProcessSelection.aspx");
            }
        }

        private DCRSProcess GetActiveProcessInstance()
        {
            string processInstanceXml = RemoteServicesHandler.GetProcessInstance(processId, processInstanceId);
            DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);
            return DCRSProcessInstance;
        }

        private void UpdatePrincipals(DCRSProcess p)
        {
            if (lbPrincipals.SelectedIndex == -1)
            {
                lbPrincipals.Items.Clear();
                foreach (string principal in p.Specification.Principals)
                {
                    lbPrincipals.Items.Add(principal);
                }
            }
        }

        private void UpdateExecutableActions(DCRSProcess p)
        {

            lbActions.Items.Clear();

            if (lbPrincipals.SelectedIndex == -1) return;
            string selectedPrincipal = lbPrincipals.Items[lbPrincipals.SelectedIndex].Value;
            
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
                    lbActions.Items.Add(new ListItem(p.Specification.ActionList[a], a.ToString()));
            }            
        }

        protected void lbPrincipals_SelectedIndexChanged(object sender, EventArgs e)
        {
            UpdateExecutableActions(ActiveProcessInstance);
        }

        protected void btnExecute_Click(object sender, EventArgs e)
        {
            if (lbPrincipals.SelectedIndex == -1) return;
            if (lbActions.SelectedIndex == -1) return;
            string selectedPrincipal = lbPrincipals.Items[lbPrincipals.SelectedIndex].Value;
            short selectedAction = Int16.Parse(lbActions.Items[lbActions.SelectedIndex].Value);
            var actionExecuteResult = RemoteServicesHandler.ExecuteAction(processId, processInstanceId, selectedAction, selectedPrincipal);
            UpdateExecutableActions(ActiveProcessInstance);
        }

    }
}