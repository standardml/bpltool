using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Sequence : Drawable
    {
        List<Drawable> drawableObjects = new List<Drawable>();
        System.Drawing.Point point;
        

        private Sequence parent;
        public Sequence Parent
        {
            get { return parent; }
        }


        public override System.Drawing.Point Draw(MainWindow main, System.Drawing.Point point)
        {
            this.point = point; // + Calculate exitpoint (if needed at all)
            
            VisualSequence vis = new VisualSequence();
            
            main.Controls.Add(vis);

            foreach (Drawable obj in drawableObjects)
            {
                point = obj.Draw(main, point); //The increase in point needs to be handled (may need to return point instead of void)
            }
            return point;
        }

        public override void AddChild(Drawable child)
        {
            drawableObjects.Add(child);
            //throw new Exception("The method or operation is not implemented.");
        }

        public void AddParent(Sequence parent)
        {
            this.parent = parent;
        }
    }
}