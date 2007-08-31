using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace CF
{
    public partial class VisualUnSplit : UserControl
    {
        public VisualUnSplit()
        {
            InitializeComponent();
        }

        public void AddImage(Image img)
        {
            pictureBox1.Height = img.Height;
            pictureBox1.Width = img.Width;
            pictureBox1.Image = img;
        }
    }
}
