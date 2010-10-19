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
using ITU.DK.DCRS.RemoteServices;

namespace DCRSGraphicalEditor
{
    public partial class MainForm : Form
    {
        protected DCRSProcess Process;
        protected Visualizer Visualizer;

        public MainForm()
        {
            InitializeComponent();                        
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
            this.processPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.processPanel_Paint);
            this.processPanel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.processPanel_MouseDown);
            this.processPanel.MouseMove += new System.Windows.Forms.MouseEventHandler(this.processPanel_MouseMove);
            this.processPanel.MouseUp += new System.Windows.Forms.MouseEventHandler(this.processPanel_MouseUp);
            processPanel.Refresh();
        }

        private void processPanel_Paint(object sender, PaintEventArgs e)
        {
            if (Visualizer != null)
            {
                Visualizer.Draw(e.Graphics);
                //Bitmap temp = Visualizer.Visualize();
                //e.Graphics.DrawImageUnscaled(temp, 0, 0);
            }
        }



        protected short selectedAction = -1;
        protected bool dragging = false;
        protected Point dragOffset;

        protected short contextRequestAction = -1;

        private void processPanel_MouseDown(object sender, MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                if (!dragging)
                {
                    selectedAction = Visualizer.GetActionByPos(e.Location);
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
                if ((DateTime.Now - lastUpdate).TotalMilliseconds > 50)
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
            if ((selectedAction == -1) || (contextRequestAction == -1))
                e.Cancel = true;
        }

        private void testToolStripMenuItem_Click(object sender, EventArgs e)
        {
            short[,] temp;

            temp = new short[Process.Specification.Conditions.GetLength(0) + 1, 2];

            for (var index = 0; index < Process.Specification.Conditions.GetLength(0); index++)
            {                
                temp[index, 0] = Process.Specification.Conditions[index, 0];
                temp[index, 1] = Process.Specification.Conditions[index, 1];
            }

            temp[Process.Specification.Conditions.GetLength(0), 1] = selectedAction;
            temp[Process.Specification.Conditions.GetLength(0), 0] = contextRequestAction;

            Process.Specification.Conditions = temp;
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void testToolStripMenuItem1_Click(object sender, EventArgs e)
        {
            short[,] temp;

            temp = new short[Process.Specification.Responses.GetLength(0) + 1, 2];

            for (var index = 0; index < Process.Specification.Responses.GetLength(0); index++)
            {
                temp[index, 0] = Process.Specification.Responses[index, 0];
                temp[index, 1] = Process.Specification.Responses[index, 1];
            }

            temp[Process.Specification.Responses.GetLength(0), 0] = selectedAction;
            temp[Process.Specification.Responses.GetLength(0), 1] = contextRequestAction;

            Process.Specification.Responses = temp;
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void addIncludeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            short[,] temp;

            temp = new short[Process.Specification.Includes.GetLength(0) + 1, 2];

            for (var index = 0; index < Process.Specification.Includes.GetLength(0); index++)
            {
                temp[index, 0] = Process.Specification.Includes[index, 0];
                temp[index, 1] = Process.Specification.Includes[index, 1];
            }

            temp[Process.Specification.Includes.GetLength(0), 0] = selectedAction;
            temp[Process.Specification.Includes.GetLength(0), 1] = contextRequestAction;

            Process.Specification.Includes = temp;
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void addExcludeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            short[,] temp;

            temp = new short[Process.Specification.Excludes.GetLength(0) + 1, 2];

            for (var index = 0; index < Process.Specification.Excludes.GetLength(0); index++)
            {
                temp[index, 0] = Process.Specification.Excludes[index, 0];
                temp[index, 1] = Process.Specification.Excludes[index, 1];
            }

            temp[Process.Specification.Excludes.GetLength(0), 0] = selectedAction;
            temp[Process.Specification.Excludes.GetLength(0), 1] = contextRequestAction;

            Process.Specification.Excludes = temp;
            Visualizer.ProcessUpdate();
            processPanel.Refresh();
        }

        private void saveToolStripMenuItem_Click(object sender, EventArgs e)
        {
            //RemoteServicesHandler.ImportSpecification(Process);            
        }
    }
}
