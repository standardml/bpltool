using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace CF
{
    public partial class Sequence : UserControl, IDrawable
    {
        private int number;

        public int Number
        {
            get { return number; }
            set { number = value; }
        }

        //MainWindow main;

        public Sequence()//, MainWindow main)
        {
            //this.number = number;
            //this.main = main;
            InitializeComponent();
        }

        private void pictureBox1_Click(object sender, EventArgs e)
        {
            //main.UserControlCounter(this.Location.X);
        }




        #region IDrawable Members

        public void Draw()
        {
            throw new Exception("The method or operation is not implemented.");
        }

        #endregion
    }
}
