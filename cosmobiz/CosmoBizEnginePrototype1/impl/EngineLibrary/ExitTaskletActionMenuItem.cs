using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using Microsoft.Dynamics.Mobile.Framework.Entities;

namespace CosmoBiz.EngineLibrary
{
  class ExitTaskletActionMenuItem : MenuItem
  {
    private ExitResult result;
    public ExitResult Result
    {
      get { return result; }
      set { result = value; }
    }
  }
}
