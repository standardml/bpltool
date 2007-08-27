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
            //vis.Location = point; // May need adjustment to align with connector line
            point.Y += size.Height; //Adds the height of the visual

            main.Controls.Add(vis);

            return this.point;
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
