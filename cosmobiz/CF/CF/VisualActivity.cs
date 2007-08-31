using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace CF
{
    public partial class VisualActivity : UserControl
    {
        public delegate void ClickedHandler();

        public VisualActivity()
        {
            InitializeComponent();
            //this.Top = 400;

        }

        public event ClickedHandler VisualClicked;

        protected virtual void OnVisualClicked()
        {
            if (VisualClicked != null)
            {
                VisualClicked();
            }
        }

        private void VisualActivity_Click(object sender, EventArgs e)
        {
            OnVisualClicked();
        }

        public void AddImage(Image img)
        {
            pictureBox1.Height = img.Height;
            pictureBox1.Width = img.Width;
            pictureBox1.Image = img;
        }
    }
}
