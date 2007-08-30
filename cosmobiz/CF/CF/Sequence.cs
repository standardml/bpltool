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
        private double l;

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

        public override Point Draw(MainWindow main, Point point, float maxWidth)
        {
            this.point.X = point.X;
            this.point.Y = point.Y;
            // + Calculate exitpoint (if needed at all)
            //VisualSequence vis = new VisualSequence();
            //main.Controls.Add(vis);

            if (parent == null)
            {
                float w = size.Width;
                l = w / 2 - (0.5); //Equals: l = w/2-(1/2) - (w is width of child element, l is drawing index)
                this.point.X = ((float)l * 135) + 45; //Position * drawing index width in px + adjusting for screens left side
            }

            foreach (Drawable obj in children)
            {
                this.point = obj.Draw(main, this.point, size.Width);
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