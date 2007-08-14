using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Activity : Drawable
    {
        System.Drawing.Point point;

        private Drawable child;

        public Drawable Child
        {
            get { return child; }
        }

        public override int CollectWidths()
        {
            return 0;
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
            this.child = child;
            //throw new Exception("The method or operation is not implemented.");
        }


    }
}
