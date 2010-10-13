using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ITU.DK.DCRS.Visualization.Layout
{
    public class StoredLayout<ST, LT> : LayoutProvider<ST, LT> where ST : IEquatable<ST>
    {
        Placement<ST> placement;
        
        public void Run()
        {
            placement = Placement<ST>.DeserializeFromXMLOld();
        }

        public Dictionary<ST, Vector2> GetNodePositions()
        {
            Dictionary<ST, Vector2> result = new Dictionary<ST, Vector2>();
            /*
            foreach (var x in placement.Locations)
                result.Add(x.Key, new Vector2(x.Value));
            return result;
            */

            for (int i = 0; i < placement.Keys.Count; i++)
            {
                result.Add(placement.Keys.ElementAt(i), new Vector2(placement.Values.ElementAt(i)));
            }
            return result;
        }
    }
}
