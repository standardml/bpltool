using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using DCRSModelCheckerUI.HelperClasses;
using DCRSModelCheckerUI.Services;

namespace DCRSModelCheckerUI
{
    public partial class FormProcessRepository : Form
    {
        public FormProcessRepository()
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

            var instancesList = RemoteServicesHandler.GetProcessInstancesList(processItem.ProcessId);

            ThreadSafeCallHandler.UpdateListWithListValues(listBoxInstances, instancesList);

        }

        private void ButtonOpenInstanceClick(object sender, EventArgs e)
        {
            LoadProcessInstance();

        }

        private void LoadProcessInstance()
        {

            if ((listBoxInstances.SelectedIndex == -1) || (listBoxProcesses.SelectedIndex == -1))
            {
                MessageBox.Show("Please select an instance in the listbox!", "Instance Id not selected",
                                MessageBoxButtons.OK, MessageBoxIcon.Information);

                return;
            }

            var processId = ((ProcessItem) listBoxProcesses.Items[listBoxProcesses.SelectedIndex]).ProcessId;

            var processInstanceId = int.Parse(listBoxInstances.Items[listBoxInstances.SelectedIndex].ToString());


            var formInstance = new FormProcessInstance(processId, processInstanceId)
                                   {MdiParent = FormMain.GetMdiParent()};

            formInstance.Show();

            formInstance.BringToFront();





        }

        private void buttonStartNewInstance_Click(object sender, EventArgs e)
        {
            if (listBoxProcesses.SelectedIndex == -1) 
            {
                MessageBox.Show("Please select a process for which a new  instance to be started!", "Process Id is not selected",
                                MessageBoxButtons.OK, MessageBoxIcon.Information);

                return;
            }


            var processId = ((ProcessItem)listBoxProcesses.Items[listBoxProcesses.SelectedIndex]).ProcessId;

            var processInstanceId = RemoteServicesHandler.StartNewInstance(processId);

            var formInstance = new FormProcessInstance(processId, processInstanceId) { MdiParent = FormMain.GetMdiParent() };

            formInstance.Show();

            formInstance.BringToFront();

        }
    }
}
