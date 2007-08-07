using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace CF
{
    public partial class VisualActivity : UserControl, IDrawable
    {
        public VisualActivity()
        {
            InitializeComponent();
            /*
            if (File.Exists("\\Program Files\\CF\\Images\\Activity.bmp"))
            {
                Image bitmap = new Bitmap("\\Program Files\\CF\\Images\\Activity.bmp");
                Image bitmap2 = new Bitmap(50, 32);
                Graphics graph = Graphics.FromImage(bitmap);
                Graphics graph2 = Graphics.FromImage(bitmap2);
                
                //PictureBox box = new PictureBox();

                //graph.Clear(Color.Red);
                graph.DrawImage(bitmap, new Rectangle(0, 0, 50, 32), new Rectangle(0, 0, bitmap.Width, bitmap.Height), GraphicsUnit.Pixel);
                graph2.Clear(Color.Red);
                //graph.DrawLine(new Pen(Color.Black), 10, 10, 30, 10);
                this.pictureBox1.Image = bitmap2;
            }
            else
            {
                Console.WriteLine("crap");
            }
            */
        }

        #region IDrawable Members

        public void Draw()
        {
            throw new Exception("The method or operation is not implemented.");
        }

        public void AddChild()
        {
            throw new Exception("The method or operation is not implemented.");
        }

        #endregion
    }
}
