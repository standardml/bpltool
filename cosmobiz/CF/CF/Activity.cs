using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CF
{
    public class Activity : Drawable
    {
        // For testing
        private string name;

        public override void setName(string name)
        {
            this.name = name;
        }

        public override string getName()
        {
            return name;
        }
        //

        Point point;
        //Set default size for Activity
        private Size size;
        public override Size Size()
        {
            return size;
        }

        private Drawable parent;

        public Activity()
        {
            size = new Size(1, 1);
            point = new Point(0, 0);
        }

        public override Point Draw(MainWindow main, Point point, float maxWidth)
        {
            this.point.X = point.X;
            this.point.Y = point.Y;

            VisualActivity vis = new VisualActivity();
            vis.VisualClicked += new VisualActivity.ClickedHandler(vis_VisualClicked);
            
            //Draws the image
            int imgWidth = 90;
            int imgHeight = 70;
            vis.ClientSize = new Size(imgWidth, imgHeight);

            Pen pen = new Pen(Color.Black, 1);

            Image img = new Bitmap(imgWidth, imgHeight, System.Drawing.Imaging.PixelFormat.Format32bppRgb);
            Graphics graph = Graphics.FromImage(img);

            graph.Clear(Color.Transparent);
            graph.DrawLine(pen, imgWidth / 2, 0, imgWidth / 2, 30);
            Rectangle rect = new Rectangle(0, 30, imgWidth - 1, 39);
            graph.DrawRectangle(pen, rect);

            vis.AddImage(img);
            //


            System.Drawing.Point p = new System.Drawing.Point(Convert.ToInt32(point.X) - vis.Width / 2, Convert.ToInt32(point.Y));

            this.point.X = p.X;
            vis.Location = p;


            //-vis.Width / 2; // May need adjustment to align with connector line
            this.point.Y += vis.Height;  //Adds the height of the visual (y-position)
            //x-position doesnt change when drawing an Activity
            point.Y = this.point.Y;
            main.Controls.Add(vis);

            return point;
        }

        void vis_VisualClicked()
        {
            Console.WriteLine("ddd");
        }

        public override void AddChild(Drawable child)
        {
            //this.child = child;
            //throw new Exception("The method or operation is not implemented.");
        }



        public override void AddParent(Drawable parent)
        {
            this.parent = parent;
        }

        public override Drawable GetParent()
        {
            return parent;
        }

        public override Size CollectSize()
        {
            return size;
        }
    }
}
