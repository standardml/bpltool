using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CF
{
    public class Flow : Drawable
    {
        private Size size;
        private List<Drawable> children = new List<Drawable>(); //always sequences
        private Drawable parent;

        private System.Drawing.Point point;

        private bool visible = true;

        public bool Visible
        {
            get { return visible; }
            set { visible = value; }
        }

        private int width;
        public int Width
        {
            get { return width; }
        }

        public override Size CollectSize()
        {
            Size w = new Size(0,0);
            foreach (Drawable seq in children)
            {
                w.Width += seq.CollectSize().Width;
                if (seq.CollectSize().Height > w.Height)
                {
                    w.Height = seq.CollectSize().Height;
                }//Must find the largest depth of the children (height)
            }
            size = w;
            return w;
        }

        public Flow()
        {
            size = new Size(0, 2);
        }


        public override System.Drawing.Point Draw(MainWindow main, System.Drawing.Point point)
        {
            this.point = point;
            //Calculate exitpoint

            if (visible)
            {
                VisualFlow flow = new VisualFlow();
                flow.Location = point;
                main.Controls.Add(flow);


                foreach (Drawable seq in children)
                {
                    point = seq.Draw(main, point); //needs adjustment, or sequences will not appear in parallel.
                }
            }
            else
            {
                //draw replacement
            }
            return point;
        }

        public override void AddChild(Drawable child)
        {
            children.Add(child);  //should make sure child is Sequence
        }



        public override void AddParent(Drawable parent)
        {
            this.parent = parent;
        }

        public override Drawable GetParent()
        {
            return parent;
        }
    }
}
