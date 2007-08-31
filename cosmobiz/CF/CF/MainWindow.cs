using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;

namespace CF
{
    public partial class MainWindow : Form
    {
        ListedElements elements;
        private Drawable start;

        public MainWindow()
        {
            InitializeComponent();
            //PerformReadXML();

        }
        private void PerformReadXML(string file)
        {
            string englishPath = "\\Program files\\CF\\";
            string danishPath = "\\Programmer\\CF\\";
            if (Directory.Exists(englishPath))
            {
                englishPath += file;
                elements = ReadXML(englishPath);
            }
            else if (Directory.Exists(danishPath))
            {
                danishPath += file;
                elements = ReadXML(danishPath);
            }
            else
            {
                ShowDialog();
            }
        }

        private ListedElements ReadXML(string path)
        {
            ListedElements TempElements = new ListedElements();
            XmlTextReader xreader = new XmlTextReader(path);
            while (xreader.Read())
            {
                if (xreader.HasAttributes)
                {
                    TempElements.Add(new Element(xreader[0], xreader.Name, xreader[1]));
                }
                if (xreader.NodeType == XmlNodeType.EndElement)
                {

                    TempElements.Add(new Element("end" + xreader.LocalName, "end" + xreader.LocalName, ""));
                }

            }
            return TempElements;
        }

        private void menuItem2_Click(object sender, EventArgs e)
        {
            PerformReadXML("Flow.xml");
            ProcessAndDraw();
        }

        private void menuItem3_Click(object sender, EventArgs e)
        {
            PerformReadXML("Flow2.xml");
            ProcessAndDraw();
        }

        private void menuItem4_Click(object sender, EventArgs e)
        {
            PerformReadXML("Flow3.xml");
            ProcessAndDraw();
        }

        private void menuItem5_Click(object sender, EventArgs e)
        {
            PerformReadXML("Flow4.xml");
            ProcessAndDraw();

        }

        private void menuItem6_Click(object sender, EventArgs e)
        {
            PerformReadXML("Flow5.xml");
            ProcessAndDraw();
        }

        private void ProcessAndDraw()
        {
            this.Controls.Clear();
            Processor proc = new Processor(elements);
            proc.ProcessElements(0);

            Point point = new Point(0, 0); //Adjusts 45 px from left side of screen
            start = proc.StartSequence;

            Size size = start.CollectSize();

            start.Draw(this, point, size.Width);
        }




    }
}