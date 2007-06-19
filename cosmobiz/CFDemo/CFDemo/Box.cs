using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CFDemo
{
    class Box : DrawableObject
    {
        private Graphics graph;
        private Point startPoint;
        private Point topPoint;
        private Point bottomPoint;
        private bool selected;

        private Rectangle rect;
        private Pen pen;

        private int height = 50;
        private int width = 100;
        private int linelength = 30;
        private int x;

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

        public bool Selected
        {
            get { return selected; }
        }

        public int Width
        {
            get { return width; }
        }

        public int Height
        {
            get { return height; }
        }

        public int Linelength
        {
            get { return linelength; }
        }

        private string name;
        public string Name
        {
            get { return name; }
        }

        private string owner;

        public string Owner
        {
            get { return owner; }
            set { owner = value; }
        }

        public Box(Point point, Graphics graph, string name, string owner, int x)
        {
            this.graph = graph;
            this.name = name;
            this.owner = owner;
            this.x = x;
            pen = new Pen(Color.Black);
        }

        public override Point Draw(Point point)
        {
            point.X = x;
            startPoint = point;
            topPoint.X = point.X - width / 2;
            topPoint.Y = point.Y + linelength;
            bottomPoint.X = point.X + width / 2;
            bottomPoint.Y = point.Y + linelength + height;
            
            rect = new Rectangle(topPoint.X, topPoint.Y, width, height);
            graph.DrawLine(pen, point.X, point.Y, point.X, point.Y + linelength);
            graph.DrawString(name,new Font("Verdana",5, FontStyle.Regular),new SolidBrush(Color.Tomato),topPoint.X+2,topPoint.Y);
            graph.DrawString(owner,new Font("Verdana",5,FontStyle.Regular),new SolidBrush(Color.Blue),topPoint.X+2,topPoint.Y+10);
            graph.DrawRectangle(pen, rect);

            return new Point(point.X, bottomPoint.Y);
        }

        public void ReDraw()
        {
            if (selected)
            {
                pen.Color = Color.Black;
            }
            else
            {
                pen.Color = Color.Red;
            }

            rect = new Rectangle(topPoint.X, topPoint.Y, width, height);
            //graph.DrawLine(pen, startPoint.X, startPoint.Y, startPoint.X, startPoint.Y + linelength);
            graph.DrawRectangle(pen, rect);

            selected = selected ^ true; //toggles selected (XOR)
        }
    }
}
