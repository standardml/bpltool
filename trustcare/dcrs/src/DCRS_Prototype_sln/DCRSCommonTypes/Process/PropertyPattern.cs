using System.Collections.Generic;
using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace ITU.DK.DCRS.CommonTypes.Process
{
    public class PropertyPattern
    {

        public PropertyScopeTypeEnum PropertyScope { get; set; }

        public PropertyFunctionTypeEnum PropertyFunction { get; set; }

        public List<short> PrametersList { get; private set; }

        public PropertyPattern()
        {
            PrametersList = new List<short>();
        }
    }
}
