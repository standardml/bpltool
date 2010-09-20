using System.Collections.Generic;

namespace ITU.DK.DCRS.CommonTypes.Process
{
    public class DCRSState
    {
        public long StateNumber;

        public StateTransitionList LeadingTransitions = new StateTransitionList();

        public StateTransitionList FollowingTransitions = new StateTransitionList();
      
        public DCRSStateVector StateVector = new DCRSStateVector();

        public List<short> EnabledActions = new List<short>();

    }
}
