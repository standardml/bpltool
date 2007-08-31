using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace CF
{
    public partial class VisualSequence : UserControl
    {
        private int number;

        public int Number
        {
            get { return number; }
            set { number = value; }
        }

        //MainWindow main;

        public VisualSequence()//, MainWindow main)
        {
            //this.number = number;
            //this.main = main;
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
