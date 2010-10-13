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

            ITU.DK.DCRS.CommonTypes.Process.DCRSSpecification spec = ITU.DK.DCRS.CommonTypes.Samples.DCRSSampleModels.GetGiveMedicineSpecification();
            //spec.Excludes[1, 0] = 2;
            //spec.Excludes[1, 1] = 2;           
            ITU.DK.DCRS.Visualization.Visualizer.Draw(spec, panel1.CreateGraphics());
        }

        private void button2_Click(object sender, EventArgs e)
        {/*
            Placement<short> p = new Placement<short>();
            //p.Locations.Add(0, new Point(100, 100));
            //p.Locations.Add(1, new Point(100, 200));
            //p.Locations.Add(2, new Point(100, 300));
            //p.Locations.Add(3, new Point(100, 400));

            //p.Locations.Add(new KeyValuePair<short, Point>(0, new Point(100, 100)));
            //p.Locations.Add(new KeyValuePair<short, Point>(1, new Point(100, 200)));
            //p.Locations.Add(new KeyValuePair<short, Point>(2, new Point(100, 300)));
            //p.Locations.Add(new KeyValuePair<short, Point>(3, new Point(100, 400)));
            p.Add(0, new Point(100, 100));
            p.Add(1, new Point(100, 200));
            p.Add(2, new Point(100, 300));
            p.Add(3, new Point(100, 400));

            Placement<short>.SerializeToXML(p);
          */
        }


    }
}
