using System.Collections.Generic;

namespace ITU.DK.DCRS.CommonTypes.Process
{
    public class DCRSStateVector
    {
        public List<short> IncludedActions = new List<short>();

        public List<short> ExecutedActions = new List<short>();

        public List<short> PendingResponseActions = new List<short>();

        public short StateRank;

        public bool StateAccepting;

    }
}
