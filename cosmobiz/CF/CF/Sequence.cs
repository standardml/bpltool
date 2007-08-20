using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CF
{
    public class Sequence : Drawable
    {
        private Size size;
        private List<Drawable> children = new List<Drawable>();

        private Drawable parent;

        System.Drawing.Point point;

        private int width;
        public int Width
        {
            get { return width; }
        }

        public Sequence()
        {
            size = new Size(0, 0);
        }

        public override Size CollectSize() //Not correct, since each Act will add to the width (which should be 1 no matter how many Acts there are in a seq)
        {
            Size w = new Size(0, 0);
            foreach (Drawable obj in children)
            {
                if (obj is Flow)
                {
                    w.Width += obj.CollectSize().Width;
                }
                
                w.Height += obj.CollectSize().Height;
                /*if (obj.CollectSize().Height > w.Height)
                {
                    w.Height += obj.CollectSize().Height;
                }*/

            }
            if (w.Width == 0)
            {
                w.Width = 1;
            }
            size = w;

            return w;

        }

        public override System.Drawing.Point Draw(MainWindow main, System.Drawing.Point point)
        {
            this.point = point; // + Calculate exitpoint (if needed at all)

            VisualSequence vis = new VisualSequence();

            main.Controls.Add(vis);

            foreach (Drawable obj in children)
            {
                point = obj.Draw(main, point); //The increase in point needs to be handled (may need to return point instead of void)
            }
            return point;
        }

        public override void AddChild(Drawable child)
        {
            children.Add(child);
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