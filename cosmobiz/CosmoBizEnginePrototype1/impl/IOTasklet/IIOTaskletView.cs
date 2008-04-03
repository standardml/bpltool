using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dynamics.Mobile.Framework;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace IOTasklet
{
  public interface IIOTaskletView : ITaskletView
  {
    IOTasklet Presenter { set; }
    void Initialize();
  }
}
