using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using ITU.DK.DCRS.RemoteServices;

namespace DCRSWebUI
{
    public partial class RepositoryView : System.Web.UI.Page
    {
        bool initialLoad = true;
                    
        protected void Page_Load(object sender, EventArgs e)
        {
        }

        protected void SelectProcess_Click(object sender, EventArgs e)
        {

            int processId = -1;
            if (ProcessList.SelectedIndex != -1)
            {
                processId = Int16.Parse(ProcessList.Items[ProcessList.SelectedIndex].Value);
                Session.Add("processId", processId);

                InstanceList.Items.Clear();
                foreach (var a in RemoteServicesHandler.GetProcessInstancesList(processId))
                    InstanceList.Items.Add(new ListItem(a.ToString(), a.ToString()));

            }
            ProcessID.Text = "ProcessId: " + processId.ToString();
            //processId = Int16.Parse(ProcessList.SelectedItem.Value);
        }

        protected void Panel1_Load(object sender, EventArgs e)
        {

        }

        protected void ProcessList_SelectedIndexChanged(object sender, EventArgs e)
        {
        }

        protected void ProcessList_Load(object sender, EventArgs e)
        {
            //ProcessList.Items.Clear();
            if (ProcessList.SelectedIndex == -1)
            {
                foreach (var a in RemoteServicesHandler.GetProcessList())
                    ProcessList.Items.Add(new ListItem(a.Value.ToString() + " (" + a.Key.ToString() + ")", a.Key.ToString()));
                initialLoad = false;
            }
        }

        protected void Image1_Load(object sender, EventArgs e)
        {

        }

        protected void SelectInstance_Click(object sender, EventArgs e)
        {
            int processInstanceId = -1;
            if (InstanceList.SelectedIndex != -1)
            {
                processInstanceId = Int16.Parse(InstanceList.Items[InstanceList.SelectedIndex].Value);
                Session.Add("processInstanceId", processInstanceId);
            }
        }

    }
}
