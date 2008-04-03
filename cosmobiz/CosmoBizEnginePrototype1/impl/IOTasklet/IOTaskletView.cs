using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace IOTasklet
{
  public partial class IOTaskletView : TaskletView, IIOTaskletView
  {
    private IOTasklet presenter;

    public IOTasklet Presenter
    {
      set { presenter = value; }
    }

    public IOTaskletView()
    {
      InitializeComponent();
    }

    public void Initialize()
    {
      this.tbInput.Text = "--";
      this.tbInput.Text = presenter.ToShow;
    }

    private void tbOutput_TextChanged(object sender, EventArgs e)
    {
      presenter.Result = tbOutput.Text;
    }
  }
}
