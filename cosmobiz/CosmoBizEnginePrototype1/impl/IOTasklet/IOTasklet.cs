using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using Microsoft.Dynamics.Mobile.Framework;

namespace IOTasklet
{
  public class IOTasklet : Tasklet
  {
    IIOTaskletView view;

    public IOTasklet()
    {
      view = new IOTaskletView();
      view.Presenter = this;
    }
    string toShow;

    [InputParameter(InputParameterType.Required)]
    public string ToShow
    {
      get { return toShow; }
      set { toShow = value; }
    }

    string result;

    [OutputParameter]
    public string Result
    {
      get { return result; }
      set
      {
        result = value;
        OnOutputChanged();
      }
    }


    protected override void OnStarted()
    {
      base.OnStarted();

      this.PopulateView(view);
      this.Container.Show((Control)view);
      this.view.Initialize();
    }
  }
}
