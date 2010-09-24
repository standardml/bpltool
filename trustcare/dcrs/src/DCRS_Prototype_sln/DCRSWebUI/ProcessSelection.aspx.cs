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
    public partial class ProcessSelection : System.Web.UI.Page
    {
        /// <summary>
        /// Helper properties for checking existance of and retrieving process(Instance)Ids and the page to return to.
        /// </summary>
        private bool processIdExists { get { return (Session["processId"] != null); } }
        private bool processInstanceIdExists { get { return (Session["processInstanceId"] != null); } }
        private bool returnPageExists { get { return (Session["returnPage"] != null); } }
        private int processId { get { return (int)Session["processId"]; } }
        private int processInstanceId { get { return (int)Session["processInstanceId"]; } }
        private String returnPage { get { return (string)Session["returnPage"]; } }

        /// <summary>
        /// On PageLoad:    if a process ID and process Instance ID have been selected and a return page exists, then return to the original page,
        ///                 otherwise fill the list of available processes.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected void Page_Load(object sender, EventArgs e)
        {
            if (processIdExists && processInstanceIdExists && returnPageExists)            
                Server.Transfer(returnPage);            
            else
                UpdateProcessList();
        }

        /// <summary>
        /// Updates the processes dropdownlist.
        /// </summary>
        private void UpdateProcessList()
        {
            
            if (ddlProcess.SelectedIndex == -1)
            {
                ddlProcess.Items.Add(new ListItem("Select Process", "-1"));
                foreach (var a in RemoteServicesHandler.GetProcessList())
                    ddlProcess.Items.Add(new ListItem(a.Value.ToString() + " (" + a.Key.ToString() + ")", a.Key.ToString()));                
            }
        }


        /// <summary>
        /// Selects a process and process instance.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        protected void btnSelect_Click(object sender, EventArgs e)
        {
            int processId = Int16.Parse(ddlProcess.Items[ddlProcess.SelectedIndex].Value);
            if (processId != -1)
            {
                Session.Add("processId", processId);
            }

            int processInstanceId = Int16.Parse(ddlProcessInstance.Items[ddlProcessInstance.SelectedIndex].Value);

            if (processInstanceId == -2)
            {
                processInstanceId = RemoteServicesHandler.StartNewInstance(processId);
            }

            if (processInstanceId != -1)
            {
                Session.Add("processInstanceId", processInstanceId);
                if (processIdExists && processInstanceIdExists && returnPageExists) Server.Transfer(returnPage);
            }
        }

    }
}