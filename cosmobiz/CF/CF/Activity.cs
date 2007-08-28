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

        public override Point Draw(MainWindow main, Point point)
        {
            this.point.X = point.X;
            this.point.Y = point.Y;
           
            VisualActivity vis = new VisualActivity();
            vis.VisualClicked += new VisualActivity.ClickedHandler(vis_VisualClicked);

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
