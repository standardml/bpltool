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
        private bool processIdExists { get { return (Session["processId"] != null); } }
        private bool processInstanceIdExists { get { return (Session["processInstanceId"] != null); } }
        private bool returnPageExists { get { return (Session["returnPage"] != null); } }

        private int processId { get { return (int)Session["processId"]; } }
        private int processInstanceId { get { return (int)Session["processInstanceId"]; } }
        private String returnPage { get { return (string)Session["returnPage"]; } }


        protected void Page_Load(object sender, EventArgs e)
        {
            UpdateProcessList();
            //if (processIdExists) UpdateProcessInstanceList();
            if (processIdExists && processInstanceIdExists && returnPageExists) Server.Transfer(returnPage);
        }

        protected void ddlProcessInstance_SelectedIndexChanged(object sender, EventArgs e)
        {
            int processInstanceId = Int16.Parse(ddlProcessInstance.Items[ddlProcessInstance.SelectedIndex].Value);
            if (processInstanceId != -1)
            {
                Session.Add("processInstanceId", processInstanceId);
                if (processIdExists && processInstanceIdExists && returnPageExists) Server.Transfer(returnPage);
            }
        }

        protected void ddlProcess_SelectedIndexChanged(object sender, EventArgs e)
        {
            int processId = Int16.Parse(ddlProcess.Items[ddlProcess.SelectedIndex].Value);
            if (processId != -1)
            {
                Session.Add("processId", processId);
                UpdateProcessInstanceList();
            }
        }


        private void UpdateProcessList()
        {
            
            if (ddlProcess.SelectedIndex == -1)
            {
                ddlProcess.Items.Add(new ListItem("----", "-1"));
                foreach (var a in RemoteServicesHandler.GetProcessList())
                    ddlProcess.Items.Add(new ListItem(a.Value.ToString() + " (" + a.Key.ToString() + ")", a.Key.ToString()));                
            }


            /*
            ddlProcess.Items.Clear();
            foreach (var a in RemoteServicesHandler.GetProcessInstancesList(processId))
                ListBox2.Items.Add(new ListItem(a.ToString(), a.ToString()));*/
        }

        private void UpdateProcessInstanceList()
        {
            ddlProcessInstance.Items.Clear();
            ddlProcessInstance.Items.Add(new ListItem("----", "-1"));
            foreach (var a in RemoteServicesHandler.GetProcessInstancesList(processId))
                ddlProcessInstance.Items.Add(new ListItem(a.ToString(), a.ToString()));
            

        }

    }
}