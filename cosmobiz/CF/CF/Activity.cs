using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Activity : Drawable
    {
        private Drawable child;

        public Drawable Child
        {
            get { return child; }
        }

        public override void Draw()
        {
            VisualActivity vis = new VisualActivity();
            //vis.Size = new System.Drawing.Size(30, 30);
            vis.Bounds = new System.Drawing.Rectangle(10, 20, 30, 30);
            
            //throw new Exception("The method or operation is not implemented.");
        }

        public override void AddChild(Drawable child)
        {
            this.child = child;
            //throw new Exception("The method or operation is not implemented.");
        }
    }
}
