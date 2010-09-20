using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace DCRStoFiniteAutomaton
{
    public class StateRepository
    {
        public readonly Dictionary<long, AtomicState> ComputedStates = new Dictionary<long, AtomicState>();

        public readonly List<AtomicState> TobeComputedStates = new List<AtomicState>();




        public AtomicState FindMatchingState(StateVector searchVector)
        {

            foreach (var computedState in ComputedStates)
            {
                if (computedState.Value.StateVector.Equals(searchVector))
                {
                    return computedState.Value;
                }
            }

            return null;
        }

        public bool ContainsMatchingState(StateVector searchVector)
        {
            foreach (var computedState in ComputedStates)
            {
                if (computedState.Value.StateVector.Equals(searchVector))
                {
                    return true;
                }
            }

            return false;
            
        }


    }
}
