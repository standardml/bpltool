using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CFDemo
{
    class Join : DrawableObject
    {
        private Point point;
        private Graphics graph;
        private Pen pen;
        private int linelength = 30;
        private int length = 0;
        private Point leftpoint;
        private Point rightpoint;

        public Point LeftPoint
        {
            get { return leftpoint; }
            set { leftpoint = value; }
        }

        public Point RightPoint
        {
            get { return rightpoint; }
            set { rightpoint = value; }
        }

        public int LineLength
        {
            get { return linelength; }
        }


        public Join(Graphics graph, int length)
        {
            this.graph = graph;
            this.length = length;
            
        }

        public override Point Draw(Point point)
        {
            leftpoint.X = point.X - (length);
            leftpoint.Y = point.Y;
            rightpoint.X = point.X;
            rightpoint.Y = leftpoint.Y + linelength + 1;

            pen = new Pen(Color.Black);

            graph.DrawLine(pen, leftpoint.X, leftpoint.Y, leftpoint.X, leftpoint.Y + linelength);
            graph.DrawLine(pen, rightpoint.X, point.Y, rightpoint.X, rightpoint.Y);
            graph.DrawLine(pen, leftpoint.X, leftpoint.Y+linelength, rightpoint.X, rightpoint.Y);

            return new Point(point.X - length / 2, rightpoint.Y);
        }

        /*
        private void DrawFlowAdjustment(Point startPoint, Point endPoint)
        {
            Pen pen = new Pen(Color.Black);

            if (leftDrawPoint.Y < rightDrawPoint.Y)
            {
                DrawFlowAdjustment(leftDrawPoint, new Point(leftDrawPoint.X, rightDrawPoint.Y));
            }
            else if (leftDrawPoint.Y > rightDrawPoint.Y)
            {
                DrawFlowAdjustment(rightDrawPoint, new Point(rightDrawPoint.X, leftDrawPoint.Y));
            }

            graph.DrawLine(pen, startPoint.X, startPoint.Y, endPoint.X, endPoint.Y);

            exitPoint.Y = endPoint.Y;
        }
         */
    }
}
