using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CF
{
    public class Activity : Drawable
    {
        System.Drawing.Point point;
        private Size size; //Set default size for Activity

        private Drawable parent;

        public Activity()
        {
            size = new Size(1, 1);
        }

        public override System.Drawing.Point Draw(MainWindow main, System.Drawing.Point point)
        {
            this.point = point;
            VisualActivity vis = new VisualActivity();
            vis.VisualClicked += new VisualActivity.ClickedHandler(vis_VisualClicked);
            vis.Location = point; // May need adjustment to align with connector line
            point.Y += 38; //Adds the height of the visual

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
