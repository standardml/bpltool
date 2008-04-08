using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using Microsoft.Dynamics.Mobile.Framework;

namespace HWTasklet
{
  public class HWTasklet : Tasklet
  {
    IHWTaskletView view;

    public HWTasklet()
    {
      view = new HWTaskletView();
      view.Presenter = this;
    }

    //private DatabaseCatalog databaseCatalog;

    //[RolePadService]
    //public DatabaseCatalog DatabaseCatalog
    //{
    //    get { return databaseCatalog; }
    //    set { databaseCatalog = value; }
    //}


    //string inputParameter;
    //
    //[InputParameter(InputParameterType.Required)]
    //public string InputParameter
    //{
    //    get { return inputParameter; }
    //    set { inputParameter = value; }
    //}


    //string outputParameter;
    //
    //[OutputParameter]
    //public string OutputParameter
    //{
    //    get { return outputParameter; }
    //    set
    //    {
    //        outputParameter = value;
    //        OnOutputChanged();
    //    }
    //}

    protected override void OnStarted()
    {
      base.OnStarted();

      this.PopulateView(view);
      this.Container.Show((Control)view);
    }
  }
}
