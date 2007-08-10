using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Sequence : Drawable
    {
        List<Drawable> drawableObjects = new List<Drawable>();

        private Sequence parent;
        public Sequence Parent
        {
            get { return parent; }
        }


        public override void Draw()
        {
            foreach (Drawable obj in drawableObjects)
            {
                obj.Draw();
            }
            //throw new Exception("The method or operation is not implemented.");
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