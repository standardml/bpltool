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
        private float maxWidth;
        private float localWidth;
        private Point point;
        private Point entryPoint;
        private Point exitPoint;

        private List<float> attachmentPoints;

        private Size size;
        public override Size Size()
        {
            return size;
        }

        private List<Drawable> children = new List<Drawable>(); //always sequences
        private Drawable parent;

        private bool visible = true;

        public bool Visible
        {
            get { return visible; }
            set { visible = value; }
        }

        VisualSplit vSplit;
        VisualUnSplit uSplit;

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

        }


        public override Point Draw(MainWindow main, Point point, float maxWidth)
        {
            entryPoint = new Point(point.X, point.Y);
            exitPoint = new Point(point.X, point.Y);
            this.point = new Point(point.X, point.Y);

            this.maxWidth = (maxWidth - 1) * 135; //Converts maxWidth to px
            localWidth = (size.Width - 1) * 135; //Converts the width of the flow to px

            attachmentPoints = new List<float>();

            //Calculate exitpoint

            //if (visible) //must be built into the draw method, or the graphics will be impossible to create
            //{
            //VisualFlow flow = new VisualFlow();


            point.Y += 70; //Replace with header.height

            //insert header image into main.Controls.Add();


            float firstDrawPoint = point.X - (this.maxWidth / 2);
            float a = firstDrawPoint + (this.maxWidth - localWidth) / 2; //locates the first drawing point. adjusts for wider flows in the lower parts of the structure.
            float b = 0;
            float c = 0;
            float tmp;
            for (int i = 0; i < children.Count; i++)
            {
                b = (children[i].Size().Width - 1) * 135;
                c = 2;
                tmp = a + (b / c);
                point.X = tmp;
                attachmentPoints.Add(tmp);

                Point tmpP = children[i].Draw(main, point, size.Width);

                if (maxDepth < tmpP.Y)
                {
                    maxDepth = tmpP.Y;
                }
                else
                {
                    //adjust this child line
                }
                a += children[i].Size().Width * 135;
            }
            //insert footer image into main.Controls.Add();
            exitPoint.Y = maxDepth; //Replace with footer.height
            this.point.Y = exitPoint.Y + 70;

            CreateFlowImages(entryPoint, exitPoint);
            main.Controls.Add(vSplit);
            main.Controls.Add(uSplit);
            //Add the usercontrols to main

            //}
            //else
            //{
            //draw replacement
            //}
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
        private void CreateFlowImages(Point entryPoint, Point exitPoint)
        {
            vSplit = new VisualSplit();
            uSplit = new VisualUnSplit();

            int imageWidth = (int)attachmentPoints[attachmentPoints.Count - 1] - (int)attachmentPoints[0];
            int imageHeight = 70;

            vSplit.ClientSize = new Size(imageWidth, imageHeight);//Så virker det.. ><
            uSplit.ClientSize = new Size(imageWidth, imageHeight);

            Pen pen = new Pen(Color.Black, 1);

            int connecterPoint = (int)entryPoint.X - (int)attachmentPoints[0]; //the x-coordinate of the vertical line connecting the previous element with the split

            //Header
            Image headerImg = new Bitmap(imageWidth, imageHeight, System.Drawing.Imaging.PixelFormat.Format32bppRgb);
            Graphics headerGraph = Graphics.FromImage(headerImg);

            headerGraph.Clear(Color.Transparent);
            headerGraph.DrawLine(pen, connecterPoint, 0, connecterPoint, 69);
            headerGraph.DrawLine(pen, 0, imageHeight - 1, imageWidth, imageHeight - 1);
            //----------
            vSplit.Top = (int)entryPoint.Y;
            vSplit.Left = (int)attachmentPoints[0];// (int)entryPoint.X - (int)headerWidth / 2;
            vSplit.AddImage(headerImg);
            //

#warning - this is not done yet:
            //Footer
            Image footerImage = new Bitmap(imageWidth, imageHeight, System.Drawing.Imaging.PixelFormat.Format32bppRgb);
            Graphics footerGraph = Graphics.FromImage(footerImage);

            footerGraph.Clear(Color.Transparent);
            foreach (float pPoint in attachmentPoints)
            {
                footerGraph.DrawLine(pen, (int)pPoint, 0, (int)pPoint, imageHeight - 1);
            }
            footerGraph.DrawLine(pen, 0, imageHeight, imageWidth, imageHeight);

            uSplit.Top = (int)exitPoint.Y;
            uSplit.Left = (int)attachmentPoints[0];
            uSplit.AddImage(footerImage);
            //
        }
    }
}
