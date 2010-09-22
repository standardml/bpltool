using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.Visualization;

namespace DCRSVisualizationTest
{
    public partial class Form1 : Form
    {

        //ITU.DK.DCRS.Visualization.TestVisualizer vis;
        public Form1()
        {
            InitializeComponent();
            //vis = new TestVisualizer();
            //vis.SetupPlacement(ITU.DK.DCRS.CommonTypes.Samples.DCRSSampleModels.GetGiveMedicineSpecification());
        }


        private void button1_Click(object sender, EventArgs e)
        {
          panel1.Refresh();
        }

        private void panel1_Paint(object sender, PaintEventArgs e)
        {          
          //ITU.DK.DCRS.Visualization.Visualizer.Draw(ITU.DK.DCRS.CommonTypes.Samples.DCRSSampleModels.GetGiveMedicineSpecification(), panel1.CreateGraphics());
          //vis.Draw(ITU.DK.DCRS.CommonTypes.Samples.DCRSSampleModels.GetGiveMedicineSpecification(), panel1.CreateGraphics());            
            //Graphics g = panel1.CreateGraphics();
            //g.DrawImage(Visualizer.Visualize(ITU.DK.DCRS.CommonTypes.Samples.DCRSSampleModels.GetGiveMedicineSpecification()), new Point(0,0));

            //most recent:
            ITU.DK.DCRS.Visualization.Visualizer.Draw(ITU.DK.DCRS.CommonTypes.Samples.DCRSSampleModels.GetGiveMedicineSpecification(), panel1.CreateGraphics());
        }


    }
}
