using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.Process;
using System.IO;

namespace ITU.DK.DCRS.Visualization.Layout
{
    public class DCRSLayoutProvider: LayoutProvider<short, bool>
    {
        DCRSProcessLayout placement;

        public DCRSLayoutProvider(DCRSProcessLayout dpl)
        {
            placement = dpl;
        }

        public void Run()
        {            
        }

        public Dictionary<short, Vector2> GetNodePositions()
        {
            Dictionary<short, Vector2> result = new Dictionary<short, Vector2>();

            foreach (var a in placement.NodeLocations)
            {
                result.Add(a.Key, new Vector2(a.Value));
            }

            return result;
        }
    }
}
