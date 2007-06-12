using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace CFDemo
{
    public partial class DetailedView : Form
    {
        public DetailedView(string name, string owner)
        {
            InitializeComponent();
            NameLA.Text = name;
            OwnerLA.Text = owner;

        }

        private void OK_Click(object sender, EventArgs e)
        {
            this.Close();
        }
    }
}