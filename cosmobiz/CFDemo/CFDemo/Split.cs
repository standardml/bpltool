using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CFDemo
{
    public class Split : DrawableObject
    {
        private bool collapsed = false;
        private Graphics graph;
        private Point topPoint;
        private Point bottomPoint;

        private Pen pen;
        int linelength = 30;
        int length = 0;

        private int lineLength = 30;

        public Point TopPoint
        {
            get { return topPoint; }
            set { topPoint = value; }
        }

        public Point BottomPoint
        {
            get { return bottomPoint; }
            set { bottomPoint = value; }
        }

        public int LineLength
        {
            get { return lineLength; }
            set { lineLength = value; }
        }

        public bool Collapsed
        {
            get { return collapsed; }
            set { collapsed = value; }
        }

        public Split(Graphics graph, int length)
        {
            this.graph = graph;
            this.length = length;
        }

        public override Point Draw(Point point)
        {
            topPoint.X = point.X - (length / 2);
            topPoint.Y = point.Y;
            bottomPoint.X = point.X + (length / 2);
            bottomPoint.Y = point.Y + linelength;

            pen = new Pen(Color.Black);
            graph.DrawLine(pen, point.X, point.Y, point.X, point.Y + linelength);
            graph.DrawLine(pen, topPoint.X, topPoint.Y + linelength, bottomPoint.X, bottomPoint.Y);

            return new Point(point.X, point.Y + linelength + 1);
            //Point leftpoint = new Point(Screen.PrimaryScreen.WorkingArea.Width / 4, point.Y + lineLength);
            //Point rightpoint = new Point((Screen.PrimaryScreen.WorkingArea.Width / 4) * 3, point.Y + lineLength);
            //Graphics graph = CreateGraphics();
            //graph.DrawLine(pen, point.X, point.Y, point.X, point.Y + lineLength);
            //graph.DrawLine(pen, leftpoint.X, leftpoint.Y, rightpoint.X, rightpoint.Y);
        }
    }
}
