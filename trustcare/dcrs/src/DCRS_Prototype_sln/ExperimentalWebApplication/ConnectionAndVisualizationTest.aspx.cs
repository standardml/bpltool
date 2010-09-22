using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Drawing;
using System.Drawing.Imaging;
using System.Drawing.Drawing2D;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.Visualization;
using ITU.DK.DCRS.RemoteServices;

namespace ExperimentalWebApplication
{
    public partial class ConnectionAndVisualizationTest : System.Web.UI.Page
    {
        bool initialLoad = true;
        protected void Page_Load(object sender, EventArgs e)
        {
        }

        protected void Button1_Click(object sender, EventArgs e)
        {

            int processId = -1;
            if (ListBox1.SelectedIndex != -1)
            {
                processId = Int16.Parse(ListBox1.Items[ListBox1.SelectedIndex].Value);
                Session.Add("processId", processId);

                ListBox2.Items.Clear();
                foreach (var a in RemoteServicesHandler.GetProcessInstancesList(processId))
                    ListBox2.Items.Add(new ListItem(a.ToString(), a.ToString()));

            }
            Label1.Text = "ProcessId: " + processId.ToString();
            //processId = Int16.Parse(ListBox1.SelectedItem.Value);
        }

        protected void Panel1_Load(object sender, EventArgs e)
        {

        }

        protected void ListBox1_SelectedIndexChanged(object sender, EventArgs e)
        {
        }

        protected void ListBox1_Load(object sender, EventArgs e)
        {
            //ListBox1.Items.Clear();
            if (ListBox1.SelectedIndex == -1)
            { 
                foreach (var a in RemoteServicesHandler.GetProcessList())
                    ListBox1.Items.Add(new ListItem(a.Value.ToString() + " (" + a.Key.ToString() + ")", a.Key.ToString()));
                initialLoad = false;
            }
        }

        protected void Image1_Load(object sender, EventArgs e)
        {
            
        }

        protected void Button2_Click(object sender, EventArgs e)
        {
            int processInstanceId = -1;
            if (ListBox2.SelectedIndex != -1)
            {
                processInstanceId = Int16.Parse(ListBox2.Items[ListBox2.SelectedIndex].Value);
                Session.Add("processInstanceId", processInstanceId);
            }
        }

    }
}