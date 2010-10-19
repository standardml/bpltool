using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using DCRSGraphicalEditor.HelperClasses;
using ITU.DK.DCRS.RemoteServices;
using ITU.DK.DCRS.CommonTypes.Process;

namespace DCRSGraphicalEditor
{
    public partial class SelectProcessDialog : Form
    {
        public delegate void ProcessSelectedHandler(DCRSProcess process);
        public event ProcessSelectedHandler ProcessSelected;


        public SelectProcessDialog()
        {
            InitializeComponent();

            // Get the Process list and populate the list box.
            ThreadSafeCallHandler.UpdateListWithProcessDictionaryValues(listBoxProcesses, 
                RemoteServicesHandler.GetProcessList());
        }

        private void ListBoxProcessesSelectedIndexChanged(object sender, EventArgs e)
        {
            if(listBoxProcesses.SelectedIndex == -1) return; 

            var processItem = (ProcessItem) listBoxProcesses.Items[listBoxProcesses.SelectedIndex];          

        }

        
        private void buttonSelectProcess_Click(object sender, EventArgs e)
        {
            

            if (listBoxProcesses.SelectedIndex == -1) 
            {
                MessageBox.Show("Please select a process!", "Process Id is not selected",
                                MessageBoxButtons.OK, MessageBoxIcon.Information);

                return;
            }


            int processId = ((ProcessItem)listBoxProcesses.Items[listBoxProcesses.SelectedIndex]).ProcessId;

            
            DCRSProcess p = DCRSProcess.Deserialize(RemoteServicesHandler.GetProcess(processId));

            // call some delegate with the process.            
            ProcessSelected(p);
            this.Close();
        }
    }
}
