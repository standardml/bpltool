using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;

namespace FlowDemo
{
    public partial class Form1 : Form
    {
        private enum Side { left, right, center };
        private Point exitPoint;
        private string path;
        private Bitmap img;
        private Graphics graph;
        ListedElements elements;

        public Form1()
        {
            InitializeComponent();
            exitPoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 2, 10);
            pictureBox1.Width = Screen.PrimaryScreen.WorkingArea.Width - 6;
            img = new Bitmap(Screen.PrimaryScreen.WorkingArea.Width, 10000);
            graph = Graphics.FromImage(img);
            graph.Clear(Color.Transparent);
            
        }

        private void button1_Click(object sender, EventArgs e)
        {
            graph.Clear(Color.Transparent);
            path = "\\Program files\\FlowDemo\\Flow.xml";
            
            elements = ReadXML(path);
            DoDraw();
            exitPoint.X = Screen.PrimaryScreen.WorkingArea.Width / 2;
            exitPoint.Y = 10;
        }

        private void button2_Click(object sender, EventArgs e)
        {
            graph.Clear(Color.Transparent);
            path = "\\Program files\\FlowDemo\\Flow2.xml";
            elements = ReadXML(path);
            DoDraw();
            exitPoint.X = Screen.PrimaryScreen.WorkingArea.Width / 2;
            exitPoint.Y = 10;
        }

        private ListedElements ReadXML(string path)
        {
            ListedElements TempElements = new ListedElements();
            XmlTextReader xreader = new XmlTextReader(path);
            while (xreader.Read())
            {
                if (xreader.HasAttributes)
                {
                    TempElements.Add(new Element(xreader[0], xreader.Name));
                }
                if (xreader.NodeType == XmlNodeType.EndElement)
                {

                    TempElements.Add(new Element("end" + xreader.LocalName, "end" + xreader.LocalName));
                }
            }
            return TempElements;
        }

        private void DoDraw()
        {
            Point splitPoint = exitPoint;
            bool changeSide = false;
            bool flow = false;
            Side currentSide = Side.center;
            Point leftDrawPoint = new Point(0, 0);
            Point rightDrawPoint = new Point(0, 0);


            foreach (Element elem in elements)
            {
                switch (elem.Type)
                {
                    case "sequence":
                        if (flow && currentSide == Side.center) //Betyder at seq er første i et flow
                        {
                            currentSide = Side.left;
                        }

                        else if (flow && currentSide == Side.left) //Betyder at seq er anden i et flow
                        {
                            currentSide = Side.right;
                        }

                        else if (flow && currentSide == Side.right) //Betyder at seq er første efter et flow
                        {
                            flow = false;
                            currentSide = Side.center;
                        }


                        break;

                    case "endsequence":
                        if (flow && currentSide == Side.left)
                        {
                            changeSide = true;
                            leftDrawPoint.Y = exitPoint.Y;
                            leftDrawPoint.X = Screen.PrimaryScreen.WorkingArea.Width / 4;
                            break;
                        }
                        if (flow && currentSide == Side.right)
                        {
                            rightDrawPoint.Y = exitPoint.Y;
                            rightDrawPoint.X = (Screen.PrimaryScreen.WorkingArea.Width / 4) * 3;
                            break;
                        }
                        break;

                    case "activity":
                        if (changeSide)
                        {
                            DrawBox(splitPoint, currentSide);
                            changeSide = false;
                        }
                        else
                            DrawBox(exitPoint, currentSide);

                        break;
                    case "Flow":

                        splitPoint.Y = exitPoint.Y + 31;
                        DrawSplit(exitPoint, currentSide);
                        flow = true;

                        break;

                    case "endFlow":
                        flow = false;
                        currentSide = Side.center;
                        if (leftDrawPoint.Y < rightDrawPoint.Y)
                        {
                            DrawFlowAdjustment(leftDrawPoint, new Point(leftDrawPoint.X, rightDrawPoint.Y));
                        }
                        else if (leftDrawPoint.Y > rightDrawPoint.Y)
                        {
                            DrawFlowAdjustment(rightDrawPoint, new Point(rightDrawPoint.X, leftDrawPoint.Y));
                        }
                        
                        DrawJoin(exitPoint, currentSide);

                        break;

                    default:
                        break;
                }
            }

            pictureBox1.Image = img;
        }

        private void DrawBox(Point point, Side side)
        {
            Pen pen = new Pen(Color.Black);
            int height = 50;
            int width = 100;
            int linelength = 30;
            Rectangle rect;

            switch (side)
            {
                case Side.left:
                    point.X = Screen.PrimaryScreen.WorkingArea.Width / 4;
                    break;
                case Side.right:
                    point.X = (Screen.PrimaryScreen.WorkingArea.Width / 4) * 3;
                    break;
                case Side.center:
                    point.X = Screen.PrimaryScreen.WorkingArea.Width / 2;
                    break;
                default:
                    break;
            }

            rect = new Rectangle(point.X - width / 2, point.Y + linelength, width, height);
            graph.DrawLine(pen, point.X, point.Y, point.X, point.Y + linelength);
            graph.DrawRectangle(pen, rect);

            exitPoint.Y = point.Y + height + linelength;


            /////---------------------kontrolleres et andet sted---------
            if (pictureBox1.Height < exitPoint.Y)
            {
                pictureBox1.Height = exitPoint.Y + 2;
            }
            if (img.Size.Height < pictureBox1.Height)
            {

            }
            /////--------------------------------------------------------
        }

        private void DrawSplit(Point point, Side side)
        {
            Pen pen = new Pen(Color.Black);
            int linelength = 30;
            Point leftpoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 4, point.Y + linelength);
            Point rightpoint = new Point((Screen.PrimaryScreen.WorkingArea.Width / 4) * 3, point.Y + linelength);
            //Graphics graph = CreateGraphics();
            graph.DrawLine(pen, point.X, point.Y, point.X, point.Y + linelength);
            graph.DrawLine(pen, leftpoint.X, leftpoint.Y, rightpoint.X, rightpoint.Y);


            exitPoint.Y = point.Y + linelength;
        }

        private void DrawJoin(Point point, Side side)
        {
            Pen pen = new Pen(Color.Black);
            int linelength = 30;
            Point leftpoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 4, point.Y + linelength);
            Point rightpoint = new Point((Screen.PrimaryScreen.WorkingArea.Width / 4) * 3, point.Y + linelength);
            //Graphics graph = CreateGraphics();
            graph.DrawLine(pen, leftpoint.X, point.Y, leftpoint.X, leftpoint.Y);
            graph.DrawLine(pen, rightpoint.X, point.Y, rightpoint.X, rightpoint.Y);
            graph.DrawLine(pen, leftpoint.X, leftpoint.Y, rightpoint.X, rightpoint.Y);


            exitPoint.Y = point.Y + linelength;
        }

        private void DrawFlowAdjustment(Point startPoint, Point endPoint)
        {
            Pen pen = new Pen(Color.Black);
            graph.DrawLine(pen, startPoint.X, startPoint.Y, endPoint.X, endPoint.Y);

            exitPoint.Y = endPoint.Y;
        }

        
    }
}