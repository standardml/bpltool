using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CF
{
    public class Sequence : Drawable
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

        private Size size;
        public override Size Size()
        {
            return size;
        }
        private List<Drawable> children = new List<Drawable>();

        private Drawable parent;

        private Point point;
        private int l;

        public Sequence()
        {
            size = new Size(0, 0);
            point = new Point(0, 0);
        }

        public override Size CollectSize() //Not correct, since each Act will add to the width (which should be 1 no matter how many Acts there are in a seq)
        {
            Size w = new Size(0, 0);
            foreach (Drawable obj in children)
            {
                if (obj is Flow)
                {
                    if (obj.CollectSize().Width > w.Width)
                    {
                        w.Width = obj.CollectSize().Width;
                    }
                }

                w.Height += obj.CollectSize().Height;

            }
            if (w.Width == 0)
            {
                w.Width = 1;
            }
            size = w;

            return w;

        }

        public override Point Draw(MainWindow main, Point point)
        {
            this.point.X = point.X;
            this.point.Y = point.Y;
            // + Calculate exitpoint (if needed at all)
            //VisualSequence vis = new VisualSequence();
            //main.Controls.Add(vis);

            if (parent == null)
            {
                l = size.Width / 2 - 1 / 2;
                this.point.X = l;
            }

            foreach (Drawable obj in children)
            {
                point.X = this.point.X;
                
                this.point.Y = obj.Draw(main, point).Y; //The increase in point needs to be handled (may need to return point instead of void)
                
            }

            return this.point;
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