using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace CosmoBiz.EngineLibrary
{
  class OpenActionMenuItem : MenuItem
  {
    private openType action;
    public openType Action
    {
      get { return action; }
      set { action = value; }
    }
  }
}
