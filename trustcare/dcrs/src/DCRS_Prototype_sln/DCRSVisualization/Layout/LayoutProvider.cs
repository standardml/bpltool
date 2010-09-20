using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ITU.DK.DCRS.Visualization.Layout
{
  interface LayoutProvider<ST, LT> where ST : IEquatable<ST>
  {    
    void Run();
    Dictionary<ST, Vector2> GetNodePositions();
  }
}
