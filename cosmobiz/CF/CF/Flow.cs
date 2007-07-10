using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace CF
{
    public partial class Flow : UserControl, IDrawable
    {
        public Flow()
        {
            InitializeComponent();
        }

        #region IDrawable Members

        public void Draw()
        {
            throw new Exception("The method or operation is not implemented.");
        }

        #endregion
    }
}
