using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;

namespace DCRStoFiniteAutomaton
{
    public class FiniteAutamatonState : AtomicState
    {
        public FiniteAutamatonState(long stateNumber, short transition, StateVector parentState) : base(stateNumber, transition, parentState)
        {
        }


       public override StateVector ComputeState()
       {
           base.ComputeState();

           // Remove Tau action from the set of enabled actions.
           StateVector.EnabledTransitions.Remove(Utilities.TAU_ACTION);

           StateVector.StateAccepting = (StateVector.IncludedPendingResponseEvents.Count == 0);

           // StateRank is always 0 for finite runs, as we have only one copy of the state.
           StateVector.StateRank = 0;

           return StateVector;

       }


        
    }
}
