using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Flow : Drawable
    {
        private List<Drawable> sequences = new List<Drawable>();

        private bool visible = true;

        public bool Visible
        {
            get { return visible; }
            set { visible = value; }
        }


        public override void Draw()
        {

            if (visible)
            {
                Console.WriteLine("DrawMe");
                foreach (Drawable seq in sequences)
                {
                    seq.Draw();
                } 
            }
            else
            {
                //draw replacement
            }
        }

        public override void AddChild(Drawable child)
        {
            sequences.Add(child);  //should make sure child is Sequence
            //throw new Exception("The method or operation is not implemented.");
        }
    }
}
