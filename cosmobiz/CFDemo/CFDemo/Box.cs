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



        public Box(Graphics graph, Point point)
        {
            this.graph = graph;
            startPoint = point;
            topPoint.X = point.X - width / 2;
            topPoint.Y = point.Y + linelength;
            bottomPoint.X = point.X + width / 2;
            bottomPoint.Y = point.Y + linelength + height;
        }

        public override void Draw()
        {
            pen = new Pen(Color.Black);
            rect = new Rectangle(topPoint.X, topPoint.Y, width, height);
            graph.DrawLine(pen, startPoint.X, startPoint.Y, startPoint.X, startPoint.Y + linelength);
            //g.DrawString("Welcome C#",new Font("Verdana",20),new SolidBrush(Color.Tomato),40,40);

            //graph.DrawString("Box",new Font("Verdana",20),new SolidBrush(Color.Blue),topPoint.X,topPoint.Y);
            graph.DrawRectangle(pen, rect);
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
