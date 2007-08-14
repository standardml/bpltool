using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
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
            PerformReadXML();
        }
        private void PerformReadXML()
        {
            string englishPath = "\\Program files\\CF\\";
            string danishPath = "\\Programmer\\CF\\";
            if (Directory.Exists(englishPath))
            {
                englishPath += "Flow2.xml";
                elements = ReadXML(englishPath);
            }
            else if (Directory.Exists(danishPath))
            {
                danishPath += "Flow.xml";
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

        private void button1_Click(object sender, EventArgs e)
        {
            Processor proc = new Processor(elements);
            proc.ProcessElements(0);
            //Drawable seq = proc.StartSequence;
            //seq.Draw();
            Point point = new Point(10, 10);
            start = proc.DrawableObjects[0];
            start.Draw(this, point);
            //Counting of width must be performed through calling child elements.
            //Otherwise each element in proc.DrawableObjects is counting, which makes sequential flows appear parallel,
            //since each add a width.

        }

        public void UserControlCounter(int number)
        {
            number_la.Text = number.ToString();
        }
    }
}