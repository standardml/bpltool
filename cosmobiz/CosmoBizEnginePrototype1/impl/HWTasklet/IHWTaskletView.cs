using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dynamics.Mobile.Framework;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace HWTasklet
{
  public interface IHWTaskletView : ITaskletView
  {
    HWTasklet Presenter { set; }
  }
}
