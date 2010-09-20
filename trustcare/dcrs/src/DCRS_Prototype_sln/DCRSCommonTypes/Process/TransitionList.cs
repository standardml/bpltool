using System.Collections.Generic;
using System.Linq;

namespace ITU.DK.DCRS.CommonTypes.Process
{
    public class StateTransitionList : List<StateTransition>
    {
        public StateTransition GetTransition(short transitionLabel)
        {
            return this.FirstOrDefault(transition => transitionLabel == transition.Label);
        }

        public StateTransition GetTransition(long stateNumber)
        {
            return this.FirstOrDefault(transition => stateNumber == transition.StateNumber);
        }
    }
}
