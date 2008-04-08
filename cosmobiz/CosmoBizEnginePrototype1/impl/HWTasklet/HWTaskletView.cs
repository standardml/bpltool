using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace HWTasklet
{
  public partial class HWTaskletView : TaskletView, IHWTaskletView
  {
    private HWTasklet presenter;

    public HWTasklet Presenter
    {
      set { presenter = value; }
    }

    public HWTaskletView()
    {
      InitializeComponent();
    }
  }
}
