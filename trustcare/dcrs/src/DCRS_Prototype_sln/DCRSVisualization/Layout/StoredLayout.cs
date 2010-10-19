using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes.Process;
using System.IO;

namespace ITU.DK.DCRS.Visualization.Layout
{
    public class StoredLayout<ST, LT> : LayoutProvider<ST, LT> where ST : IEquatable<ST>
    {
        Placement<ST> placement;
        

        public StoredLayout(DCRSProcess p, Graph<ST, LT> g)
        {
            if (p.Runtime != null)            
                OpenProcessInstancePlacement(p, g);
            else
                OpenProcessPlacement(p, g);
        }

        public void OpenProcessInstancePlacement(DCRSProcess p, Graph<ST, LT> g)
        {
            try
            {
                placement = Placement<ST>.DeserializeFromXML(p.Specification.ProcessId, p.Runtime.ProcessInstanceId);
            }
            catch (FileNotFoundException e)
            {
                OpenProcessPlacement(p, g);
            }
        }

        public void OpenProcessPlacement(DCRSProcess p, Graph<ST, LT> g)
        {
            try
            {
                placement = Placement<ST>.DeserializeFromXML(p.Specification.ProcessId);
            }
            catch (FileNotFoundException e)
            {
                GBFBALayoutProvider<ST, LT> gbfba = new GBFBALayoutProvider<ST, LT>(g);
                gbfba.Run();
                placement = new Placement<ST>();
                placement.processID = p.Specification.ProcessId;
                foreach (var x in gbfba.GetNodePositions())
                {
                    placement.Add(x.Key, x.Value.ToPoint);                    
                }
                placement.SerializeToXML();
            }
        }


        public void Run()
        {            
        }

        public Dictionary<ST, Vector2> GetNodePositions()
        {
            Dictionary<ST, Vector2> result = new Dictionary<ST, Vector2>();
            /*
            for (int i = 0; i < placement.Keys.Count; i++)
            {
                result.Add(placement.Keys.ElementAt(i), new Vector2(placement.Values.ElementAt(i)));
            }
            */
            foreach (var a in placement.NodeLocations)
            {
                result.Add(a.Key, new Vector2(a.Value));
            }

            return result;
        }
    }
}
