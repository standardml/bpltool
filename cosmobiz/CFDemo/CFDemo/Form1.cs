using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;

namespace CFDemo
{
    public partial class Form1 : Form
    {
        ListedElements elements;

        private string path;

        Draw draw;
        string englishPath = "\\Program files\\CFDemo\\";
        string danishPath = "\\Programmer\\CFDemo\\";
        bool english = false;
        bool danish = false;
        Bitmap img;

        public Form1()
        {
            InitializeComponent();
            pictureBox1.Width = Screen.PrimaryScreen.WorkingArea.Width - 6;
            draw = new Draw();
            english = Directory.Exists(englishPath);
            danish = Directory.Exists(danishPath);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            //graph.Clear(Color.Transparent);
            if (english)
            {
                path = englishPath + "Flow.xml";
            }
            if (danish)
            {
                path = danishPath + "Flow.xml";
            }

            elements = ReadXML(path);

            CallDraw();

        }

        private void button2_Click(object sender, EventArgs e)
        {
            //graph.Clear(Color.Transparent);
            if (english)
            {
                path = englishPath + "Flow2.xml";
            }
            if (danish)
            {
                path = danishPath + "Flow2.xml";
            }

            elements = ReadXML(path);
            CallDraw();
        }

        public void CallDraw()
        {
            img = draw.DoDraw(elements);
            AdjustPictureBox();
            pictureBox1.Image = img;
            draw.PointX = Screen.PrimaryScreen.WorkingArea.Width / 2;
            draw.PointY = 10;
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

        private void AdjustPictureBox()
        {
            /////---------------------kontrolleres et andet sted---------

            if (pictureBox1.Height < draw.StartPoint.Y)
            {
                pictureBox1.Height = draw.StartPoint.Y + 2;
            }


            /////--------------------------------------------------------
        }

        private void pictureBox1_MouseDown(object sender, MouseEventArgs e)
        {
            int x = e.X;
            int y = e.Y;


            foreach (DrawableObject obj in draw.DrawableObjectList)
            {
                if (obj is Box)
                {
                    Box box = (Box)obj;
                    if (box.TopPoint.X <= x && box.TopPoint.Y <= y && box.BottomPoint.X >= x && box.BottomPoint.Y >= y)
                    {

                        draw.PointX = Screen.PrimaryScreen.WorkingArea.Width / 2;
                        draw.PointY = 10;
                        DetailedView dv = new DetailedView(box.Name, box.Owner);
                        dv.Show();
                    }
                }


            }
            /*
            foreach (Split split in draw.SplitList)
            {
                if (split.TopPoint.X <= x && split.TopPoint.Y <= y && split.BottomPoint.X >= x && split.BottomPoint.Y >= y)
                {
                    split.Collapsed = split.Collapsed ^ true;
                    
                }

            }
            CallDraw();
            */
        }



    }
}