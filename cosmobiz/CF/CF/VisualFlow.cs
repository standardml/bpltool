using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace CF
{
    public partial class VisualFlow : UserControl
    {
        public VisualFlow()
        {
            InitializeComponent();
            //this.Top = 500;
        }

        private void VisualFlow_Click(object sender, EventArgs e)
        {
            this.Visible = false;
        }


    }
}
