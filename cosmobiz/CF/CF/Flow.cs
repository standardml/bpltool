using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Flow : Drawable
    {
        private List<Drawable> sequences = new List<Drawable>();

        public override void Draw()
        {
            //throw new Exception("The method or operation is not implemented.");
        }

        public override void AddChild(Drawable child)
        {
            sequences.Add(child);  //should make sure child is Sequence
            //throw new Exception("The method or operation is not implemented.");
        }
    }
}
