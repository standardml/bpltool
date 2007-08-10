using System;
using System.Collections.Generic;
using System.Text;

namespace CF
{
    public class Flow : Drawable
    {
        private List<Drawable> sequences = new List<Drawable>();
        private System.Drawing.Point point;

        private bool visible = true;

        public bool Visible
        {
            get { return visible; }
            set { visible = value; }
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
                

                foreach (Drawable seq in sequences)
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
            sequences.Add(child);  //should make sure child is Sequence
            //throw new Exception("The method or operation is not implemented.");
        }
    }
}
