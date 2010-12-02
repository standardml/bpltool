using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using ITU.DK.DCRS.CommonTypes.Process;
using ITU.DK.DCRS.Visualization;
using ITU.DK.DCRS.Visualization.Elements;
using ITU.DK.DCRS.RemoteServices;

namespace DCRSGraphicalEditor
{
    public partial class MainForm : Form
    {
        protected DCRSProcess Process;
        protected DCRSProcessHandler ProcessHandler;
        protected Visualizer Visualizer;
        protected Visualizer ExecutionVisualizer;

        protected Boolean NodeOnlyViewEnabled = false;
        protected Boolean ExecutionEnabled = false;

        protected DCRSProcess ExecutionProcessInstance;
        protected int ExecutionStep;

        public MainForm()
        {
            InitializeComponent();
                        
            DCRSProcess p = DCRSProcess.Deserialize(RemoteServicesHandler.GetProcess(9));            
            spd_ProcessSelected(p);

            RemoteServicesHandler.HostSubscriptionServiceClient();
        }

        private void menuStrip1_ItemClicked(object sender, ToolStripItemClickedEventArgs e)
        {

        }

        private void openToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void openToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            SelectProcessDialog spd = new SelectProcessDialog();
            spd.ProcessSelected += new SelectProcessDialog.ProcessSelectedHandler(spd_ProcessSelected);
            spd.ShowDialog();
        }

        void spd_ProcessSelected(ITU.DK.DCRS.CommonTypes.Process.DCRSProcess process)
        {            
            Process = process;
            Visualizer = new Visualizer(process);
            ProcessHandler = new DCRSProcessHandler(process);
            // multiple handlers will be open after opening multiple processes? check this...
            this.processPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.processPanel_Paint);
            this.processPanel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.processPanel_MouseDown);
            this.processPanel.MouseMove += new System.Windows.Forms.MouseEventHandler(this.processPanel_MouseMove);
            this.processPanel.MouseUp += new System.Windows.Forms.MouseEventHandler(this.processPanel_MouseUp);
            processPanel.Refresh();
            
            clbRoles.Items.Clear();
            foreach (var a in process.Specification.Roles)
                clbRoles.Items.Add(a);

            /*dgRoles.Rows.Clear();
            foreach (var a in process.Specification.Roles)
            { 
                int i = dgRoles.Rows.Add();
                dgRoles
            }*/

            //BindingList<String> BindingLstVars = new BindingList<String>(process.Specification.Roles);
            //dgRoles.DataSource = BindingLstVars;
            //dgRoles

            //dgvRoles.DataSource = process.Specification.Roles.Select(x => new { Value = x }).ToList();
            //dgvRoles.DataSource = process.Specification.Roles;

            /*
            foreach (var a in process.Specification.Roles)
            { 
                int i = dgvRoles.Rows.Add(new Object[] {a});                
            }*/

            dgvRoles.DataSource = ProcessHandler.Roles;
            dgvRoles.Columns[0].Visible = false;

            dgvPrincipals.DataSource = ProcessHandler.Principals;
            dgvPrincipals.Columns[0].Visible = false;
            /*for (int i = 0; i < dgvPrincipals.Columns.Count; i = i + 2)
            {
                dgvPrincipals.Columns[i].Visible = false;
            }*/

            tbName_def.Text = process.Specification.ModelName;
            lblID.Text = process.Specification.ProcessId.ToString();

            ExecutionProcessInstance = null;
            ExecutionVisualizer = null;            
            executionPanel.Refresh();
            enableExecutionToolStripMenuItem.Text = "Start Execution";
        }

        private void processPanel_Paint(object sender, PaintEventArgs e)
        {
            if (Visualizer != null)
            {
                /*
                if (NodeOnlyViewEnabled)
                    Visualizer.DrawNodeOnlyView(e.Graphics);
                else*/
                    Visualizer.Draw(e.Graphics);

                
                //Bitmap temp = Visualizer.Visualize();
                //e.Graphics.DrawImageUnscaled(temp, 0, 0);
            }
        }



        protected short selectedAction = -1;
        protected bool dragging = false;
        protected Point dragOffset;
        protected Point contextLocation;

        protected short contextRequestAction = -1;
        protected Arrow contextRequestArrow = null;

        private void processPanel_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                if (!dragging)
                {
                    selectedAction = Visualizer.GetActionByPos(e.Location);
                    UpdateSelectedAction(selectedAction);
                    if (selectedAction != -1)
                    {
                        dragging = true;
                        dragOffset = new Point(e.Location.X - Visualizer.Placement.NodeLocations[selectedAction].X, e.Location.Y - Visualizer.Placement.NodeLocations[selectedAction].Y);
                    }
                    Visualizer.SelectedAction = selectedAction;
                    processPanel.Refresh();
                }
            }
            else if (e.Button == MouseButtons.Right)
            {
                contextRequestAction = Visualizer.GetActionByPos(e.Location);
                contextRequestArrow = Visualizer.GetArrowByPos(e.Location);
                contextLocation = e.Location;
            }
        }


        private void UpdateSelectedAction(short a)
        {
            if (a == -1)
            {
                tbName.Text = "";
                cbEnabled.Checked = false;
                cbIncluded.Checked = false;
                for (int i = 0; i < clbRoles.Items.Count; i++)
                    clbRoles.SetItemChecked(i, false);
                btnStoreActionDetails.Enabled = false;
            }
            else
            {
                tbName.Text = Process.Specification.ActionList[a];
                cbEnabled.Checked = Process.Runtime.CurrentState.EnabledActions.Contains(selectedAction);
                cbIncluded.Checked = Process.Runtime.CurrentState.StateVector.IncludedActions.Contains(selectedAction);                
                btnStoreActionDetails.Enabled = true;

                for (int i = 0; i < clbRoles.Items.Count; i++)
                    clbRoles.SetItemChecked(i, false);

                foreach (string x in Process.Specification.ActionsToRolesDictionary[a])
                {
                    clbRoles.SetItemChecked(clbRoles.Items.IndexOf(x), true);
                }
            }

        }

        private void processPanel_MouseUp(object sender, MouseEventArgs e)
        {            
            if (dragging)
            {
                //Visualizer.MoveNode(selectedAction, e.Location);
                Visualizer.MoveNode(selectedAction, new Point(Visualizer.Placement.AlignX(selectedAction, e.Location.X - dragOffset.X), Visualizer.Placement.AlignY(selectedAction, e.Location.Y - dragOffset.Y)));
                dragging = false;
                processPanel.Refresh();
            }            
        }


        DateTime lastUpdate;
        private void processPanel_MouseMove(object sender, MouseEventArgs e)
        {
            if (dragging)
            {
                if (true) //((DateTime.Now - lastUpdate).TotalMilliseconds > 50)
                {
                    //Visualizer.MoveNode(selectedAction, e.Location);
                    Visualizer.MoveNode(selectedAction, new Point(Visualizer.Placement.AlignX(selectedAction, e.Location.X - dragOffset.X), Visualizer.Placement.AlignY(selectedAction, e.Location.Y - dragOffset.Y)));
                    processPanel.Refresh();
                    lastUpdate = DateTime.Now;
                }
            }            
        }

        private void storePlacementToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Visualizer.StorePlacement();
        }

        private void cmProcessPanel_Opening(object sender, CancelEventArgs e)
        {
            if ((contextRequestAction != -1) && (ExecutionEnabled))
            {
                addConditionToolStripMenuItem.Visible = false;
                addResponseToolStripMenuItem.Visible = false;
                addIncludeToolStripMenuItem.Visible = false;
                addExcludeToolStripMenuItem.Visible = false;
                addNodeToolStripMenuItem.Visible = false;
                removeNodeToolStripMenuItem.Visible = false;
                removePrimitiveToolStripMenuItem.Visible = false;
            }
            else if (contextRequestArrow != null)
            {
                addConditionToolStripMenuItem.Visible = false;
                addResponseToolStripMenuItem.Visible = false;
                addIncludeToolStripMenuItem.Visible = false;
                addExcludeToolStripMenuItem.Visible = false;
                addNodeToolStripMenuItem.Visible = false;
                removeNodeToolStripMenuItem.Visible = false;
                removePrimitiveToolStripMenuItem.Visible = true;
            }
            else if ((selectedAction == -1) && (contextRequestAction != -1))
            {
                addConditionToolStripMenuItem.Visible = false;
                addResponseToolStripMenuItem.Visible = false;
                addIncludeToolStripMenuItem.Visible = false;
                addExcludeToolStripMenuItem.Visible = false;
                addNodeToolStripMenuItem.Visible = false;
                removeNodeToolStripMenuItem.Visible = true;
                removePrimitiveToolStripMenuItem.Visible = false;
            }
            else if (contextRequestAction == -1)
            {
                addConditionToolStripMenuItem.Visible = false;
                addResponseToolStripMenuItem.Visible = false;
                addIncludeToolStripMenuItem.Visible = false;
                addExcludeToolStripMenuItem.Visible = false;
                addNodeToolStripMenuItem.Visible = true;
                removeNodeToolStripMenuItem.Visible = false;
                removePrimitiveToolStripMenuItem.Visible = false;
            }
            else
            {
                addConditionToolStripMenuItem.Visible = true;
                addResponseToolStripMenuItem.Visible = true;
                addIncludeToolStripMenuItem.Visible = true;
                addExcludeToolStripMenuItem.Visible = true;
                addNodeToolStripMenuItem.Visible = false;
                removeNodeToolStripMenuItem.Visible = false;
                removePrimitiveToolStripMenuItem.Visible = false;
            }
        }

        private void testToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ProcessHandler.AddCondition(contextRequestAction, selectedAction); 
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void testToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            ProcessHandler.AddResponse(selectedAction, contextRequestAction); 
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void addIncludeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ProcessHandler.AddInclude(selectedAction, contextRequestAction);
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void addExcludeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            
            ProcessHandler.AddExclude(selectedAction, contextRequestAction);
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }


        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            RemoteServicesHandler.ImportSpecification(Process);
            Visualizer.StorePlacement();
        }

        private void addNodeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            short newNode = ProcessHandler.AddAction();            
            Visualizer.Placement.Add(newNode, contextLocation);
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void btnStoreActionDetails_Click(object sender, EventArgs e)
        {
            Process.Specification.ActionList[selectedAction] = tbName.Text;

            if (cbEnabled.Checked)
                if (!Process.Runtime.CurrentState.EnabledActions.Contains(selectedAction))
                    Process.Runtime.CurrentState.EnabledActions.Add(selectedAction);

            if (!cbEnabled.Checked)
                if (Process.Runtime.CurrentState.EnabledActions.Contains(selectedAction))
                    Process.Runtime.CurrentState.EnabledActions.Remove(selectedAction);


            if (cbIncluded.Checked)
                if (!Process.Runtime.CurrentState.StateVector.IncludedActions.Contains(selectedAction))
                    Process.Runtime.CurrentState.StateVector.IncludedActions.Add(selectedAction);

            if (!cbIncluded.Checked)
                if (Process.Runtime.CurrentState.StateVector.IncludedActions.Contains(selectedAction))
                    Process.Runtime.CurrentState.StateVector.IncludedActions.Remove(selectedAction);


            Process.Specification.ActionsToRolesDictionary[selectedAction].Clear();
            foreach (string s in clbRoles.CheckedItems)
                Process.Specification.ActionsToRolesDictionary[selectedAction].Add(s);            

            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void removeNodeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ProcessHandler.RemoveAction(contextRequestAction);
            Visualizer.Placement.Remove(contextRequestAction);
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void removePrimitiveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ProcessHandler.RemovePrimitive(contextRequestArrow);
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void enableNodeOnlyViewToolStripMenuItem_Click(object sender, EventArgs e)
        {
            if (NodeOnlyViewEnabled)
            {
                NodeOnlyViewEnabled = false;
                enableNodeOnlyViewToolStripMenuItem.Checked = false;
            }
            else
            {
                NodeOnlyViewEnabled = true;
                enableNodeOnlyViewToolStripMenuItem.Checked = true;
            }
            executionPanel.Refresh();
        }

        private void enableExecutionToolStripMenuItem_Click(object sender, EventArgs e)
        {
            StartExecution();
        }

        private void StartExecution()
        {
            //ExecutionEnabled = true;
            int instanceId = RemoteServicesHandler.StartNewInstance(Process.Specification.ProcessId);

            ExecutionProcessInstance = DCRSProcess.Deserialize(RemoteServicesHandler.GetProcessInstance(Process.Specification.ProcessId, instanceId));

            ExecutionVisualizer = new Visualizer(ExecutionProcessInstance);
            //Visualizer.UpdateProcess(Process);
            executionPanel.Refresh();
            enableExecutionToolStripMenuItem.Text = "Restart Execution";

            /*
            Bitmap bm;
            if (NodeOnlyViewEnabled)
                bm = Visualizer.VisualizeNodeOnlyView();
            else
                bm = Visualizer.Visualize();
            bm.Save(
                Application.StartupPath
                + "\\Execution_"
                + Process.Specification.ProcessId.ToString()
                + "_"
                + Process.Runtime.ProcessInstanceId.ToString()
                + "_("
                + ExecutionStep.ToString()
                + ").bmp");
            ExecutionStep++;*/
        }

        private void MainForm_FormClosed(object sender, FormClosedEventArgs e)
        {   
            //RemoteServicesHandler.CloseNotificationService();            
        }

        private void executionPanel_Paint(object sender, PaintEventArgs e)
        {
            if (ExecutionVisualizer != null)
            {
                if (NodeOnlyViewEnabled)
                    ExecutionVisualizer.DrawNodeOnlyView(e.Graphics);
                else
                    ExecutionVisualizer.Draw(e.Graphics);


                //Bitmap temp = Visualizer.Visualize();
                //e.Graphics.DrawImageUnscaled(temp, 0, 0);
            }
            else
            {
                e.Graphics.DrawString("Click to start execution instance.", SystemFonts.MessageBoxFont, Brushes.Red, new Point((executionPanel.Width-130) / 2, (executionPanel.Height-7) / 2));
            }
        }

        private void executionPanel_MouseUp(object sender, MouseEventArgs e)
        {
            if (ExecutionVisualizer != null)
            {
                short clickedAction = ExecutionVisualizer.GetActionByPos(new Point(e.X, e.Y));
                if (clickedAction != -1)
                {
                    if (ExecutionProcessInstance.Specification.ActionsToRolesDictionary[clickedAction].Count < 1) return;
                    if (ExecutionProcessInstance.Specification.RolesToPrincipalsDictionary[ExecutionProcessInstance.Specification.ActionsToRolesDictionary[clickedAction][0]].Count < 1) return;
                    string principal = ExecutionProcessInstance.Specification.RolesToPrincipalsDictionary[ExecutionProcessInstance.Specification.ActionsToRolesDictionary[clickedAction][0]][0];
                    var actionExecuteResult = RemoteServicesHandler.ExecuteAction(ExecutionProcessInstance.Specification.ProcessId, ExecutionProcessInstance.Runtime.ProcessInstanceId, clickedAction, principal);
                    ExecutionProcessInstance = DCRSProcess.Deserialize(RemoteServicesHandler.GetProcessInstance(ExecutionProcessInstance.Specification.ProcessId, ExecutionProcessInstance.Runtime.ProcessInstanceId));
                    ExecutionVisualizer.UpdateProcess(ExecutionProcessInstance);
                    executionPanel.Refresh();
                    if (actionExecuteResult.Status == false)
                        MessageBox.Show(actionExecuteResult.Message);
                    /*
                    Bitmap bm;
                    if (NodeOnlyViewEnabled)
                        bm = Visualizer.VisualizeNodeOnlyView();
                    else
                        bm = Visualizer.Visualize();
                    bm.Save(
                        Application.StartupPath
                        + "\\Execution_"
                        + ExecutionProcessInstance.Specification.ProcessId.ToString()
                        + "_"
                        + ExecutionProcessInstance.Runtime.ProcessInstanceId.ToString()
                        + "_("
                        + ExecutionStep.ToString()
                        + ").bmp");
                    ExecutionStep++;
                     */
                }
            }
            else
            {
                StartExecution();
            }

        }

        private void dgvPrincipals_ColumnAdded(object sender, DataGridViewColumnEventArgs e)
        {
            if (e.Column.Name.Contains("src_"))
            {
                e.Column.Visible = false;
            }
        }

        private void tpProcessModel_Enter(object sender, EventArgs e)
        {
            clbRoles.Items.Clear();
            foreach (var a in Process.Specification.Roles)
                clbRoles.Items.Add(a);
        }

        private void dgvRoles_DataError(object sender, DataGridViewDataErrorEventArgs e)
        {
            MessageBox.Show(e.Exception.Message, "Invalid Entry", MessageBoxButtons.OK, MessageBoxIcon.Warning);
        }

        private void dgvPrincipals_DataError(object sender, DataGridViewDataErrorEventArgs e)
        {
            MessageBox.Show(e.Exception.Message, "Invalid Entry", MessageBoxButtons.OK, MessageBoxIcon.Warning);
        }
    }
}
