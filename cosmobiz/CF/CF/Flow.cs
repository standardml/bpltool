using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;

namespace CF
{
    public class Flow : Drawable
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
        private float maxDepth;
        private Size size;
        public override Size Size()
        {
            return size;
        }
        private List<Drawable> children = new List<Drawable>(); //always sequences
        private Drawable parent;

        private Point point;

        private bool visible = true;

        public bool Visible
        {
            get { return visible; }
            set { visible = value; }
        }

        /*
        private int width;
        public int Width
        {
            get { return width; }
        }
        */

        public override Size CollectSize()
        {
            Size w = new Size(0, 0);
            foreach (Drawable seq in children)
            {
                w.Width += seq.CollectSize().Width;
                if (seq.CollectSize().Height > w.Height)
                {
                    w.Height = seq.CollectSize().Height;
                }//Must find the largest depth of the children (height)
            }
            
            w.Height += 2; //Adding the height of standard visualization of a flow (Split and unsplit)
            size = w;
            return w;
        }

        public Flow()
        {
            size = new Size(0, 2);
            point = new Point(0, 0);
        }


        public override Point Draw(MainWindow main, Point point)
        {
            this.point.X = point.X;
            this.point.Y = point.Y;
            

            //Calculate exitpoint

            if (visible)
            {
                VisualFlow flow = new VisualFlow();

                CreateFlowImages();
                point.Y += 70; //Replace with header.height

                //insert header image into main.Controls.Add();

                

                float a = 0;
                float b = 0;
                float c = 0;
                float tmp;
                for (int i = 0; i < children.Count; i++)
                {
                    b = children[i].Size().Width - 1;
                    c = 2;
                    tmp = a + (b / c);
                    point.X = (tmp * 135) + 45;

                    Point tmpP = children[i].Draw(main, point);

                    if (maxDepth < tmpP.Y)
                    {
                        maxDepth = tmpP.Y;
                    }
                    else
                    {
                        //adjust this child line
                    }
                    a += children[i].Size().Width;
                }

                //insert footer image into main.Controls.Add();
                this.point.Y = (maxDepth + 70); //Replace with footer.height

            }
            else
            {
                //draw replacement
            }
            return this.point;
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

        /// <summary>
        /// The method handles the creation of the header and footer image for the flow,
        /// depending on the width and children of the flow.
        /// </summary>
        private void CreateFlowImages()
        {
        }
    }
}
