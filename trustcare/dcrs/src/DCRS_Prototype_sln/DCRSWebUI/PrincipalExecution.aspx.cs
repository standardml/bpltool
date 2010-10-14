using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using ITU.DK.DCRS.RemoteServices;
using ITU.DK.DCRS.CommonTypes.Process;
using System.Drawing;
using ITU.DK.DCRS.Visualization;

namespace DCRSWebUI
{
    public partial class PrincipalExecution : System.Web.UI.Page
    {
        /// <summary>
        /// Helper properties for checking existance of and retrieving process(Instance)Ids.
        /// </summary>
        private bool processIdExists { get { return (Session["processId"] != null); } }
        private bool processInstanceIdExists { get { return (Session["processInstanceId"] != null); } }
        private int processId { get { return (int)Session["processId"]; } }
        private int processInstanceId { get { return (int)Session["processInstanceId"]; } }
        private string principal { get { return (string)Session["principal"]; } }

        /// <summary>
        /// Helper method and property for retrieving the process instance being executed.
        /// </summary>
        private DCRSProcess ActiveProcessInstance { get { return GetActiveProcessInstance(); } }
        private DCRSProcess GetActiveProcessInstance()
        {
            string processInstanceXml = RemoteServicesHandler.GetProcessInstance(processId, processInstanceId);
            DCRSProcess DCRSProcessInstance = DCRSProcess.Deserialize(processInstanceXml);
            return DCRSProcessInstance;
        }


        /// <summary>
        /// On PageLoad:    if a process ID and process Instance ID have been provided then fill the principals dropdownlist, 
        ///                 otherwise call the process selection page.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected void Page_Load(object sender, EventArgs e)
        {
            if (processIdExists && processInstanceIdExists)
            {
                DCRSProcess p = ActiveProcessInstance;
                UpdatePrincipals(p);
            }
            else
            {
                Session.Add("returnPage", "PrincipalExecution.aspx");
                Server.Transfer("ProcessSelection.aspx");
            }            
        }

        /// <summary>
        /// Updates the principals dropdownlist.
        /// </summary>
        /// <param name="p"></param>
        private void UpdatePrincipals(DCRSProcess p)
        {
            if (lbPrincipals.SelectedIndex == -1)
            {
                lbPrincipals.Items.Clear();
                lbPrincipals.Items.Add(new ListItem("Select Principal", "-1"));
                foreach (string principal in p.Specification.Principals)
                {
                    lbPrincipals.Items.Add(principal);
                }                
            }
        }

        protected void btnChangeProcess_Click(object sender, EventArgs e)
        {
            Session["processId"] = null;
            Session["processInstanceId"] = null;
            Session.Add("returnPage", "ProcessExecution.aspx");
            Server.Transfer("ProcessSelection.aspx");
        }

        protected void btnSelect_Click(object sender, EventArgs e)
        {
            string p = lbPrincipals.Items[lbPrincipals.SelectedIndex].Value;
            if (p != "")
            {
                Session.Add("principal", p);
            }

        }

        protected void ImageButton1_Click(object sender, ImageClickEventArgs e)
        {
            // do some execution here.
            Point p = new Point(e.X, e.Y);
            short action = Visualizer.GetActionByPos(p, ActiveProcessInstance);
            
            var actionExecuteResult = RemoteServicesHandler.ExecuteAction(processId, processInstanceId, action, principal);
            if (!actionExecuteResult.Status)
                errorLabel.Text = actionExecuteResult.Message;
            else
                errorLabel.Text = "";
        }

    }
}