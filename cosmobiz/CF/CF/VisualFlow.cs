using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace CF
{
    public partial class VisualFlow : UserControl, IDrawable
    {
        public VisualFlow()
        {
            InitializeComponent();
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
