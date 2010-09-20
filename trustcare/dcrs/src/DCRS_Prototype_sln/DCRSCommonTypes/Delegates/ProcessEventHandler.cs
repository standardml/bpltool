using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.EventArguments;

namespace ITU.DK.DCRS.CommonTypes.Delegates
{
    /// <summary>
    /// This delegate is an event handler where DCRSProcess will be passed as a argument.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="args"></param>
    public delegate void ProcessEventHandler(object sender, ProcessEventArgs args);

}
