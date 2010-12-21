using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;

namespace DCRStoFiniteAutomaton
{
    public class BuchiAutomatonState : AtomicState
    {
        public BuchiAutomatonState(long stateNumber, short transition, StateVector parentState) : base(stateNumber, transition, parentState)
        {
        }



        public override StateVector ComputeState()
        {
            base.ComputeState();

            ComputeCurrentState();

            return StateVector;

        }




        private void ComputeCurrentState()
        {
            // This set corresponds to I^R \ (I'^R') U {e}
            var allowableResponses = new List<short>();

            foreach (short pendingResponseEvent in ParentState.IncludedPendingResponseEvents)
            {
                if (!StateVector.IncludedPendingResponseEvents.Contains(pendingResponseEvent))
                    allowableResponses.Add(pendingResponseEvent);
            }

            //// Finally Check whether existing transition is added
            //if (!allowableResponses.Contains(LeadingTransition)) allowableResponses.Add(LeadingTransition);


            // NOTE: Chnages for new TAU action.. Start
            // Finally Check whether existing transition can be added in case if it is not the TAU action.
            if (!allowableResponses.Contains(LeadingTransition) && (LeadingTransition != Utilities.TAU_ACTION)) allowableResponses.Add(LeadingTransition);
            // NOTE: Chnages for new TAU action.. End.


            // Case 1: when M set is not empty..
            if (ParentState.HigherPendingResponseEvents.Count > 0)
            {

                ParentState.HigherPendingResponseEvents.Sort();

                // if min(M set) belongs to allowable responses, Assign the reank of min(Mset) to staterank.
                // else inherit rank from the parent.
                StateVector.StateRank = (allowableResponses.Contains(ParentState.HigherPendingResponseEvents[0]))
                                            ? ParentState.HigherPendingResponseEvents[0]
                                            : ParentState.StateRank;

                StateVector.StateAccepting = (allowableResponses.Contains(ParentState.HigherPendingResponseEvents[0]))
                                                 ? true
                                                 : false;
            }
            else if (ParentState.IncludedPendingResponseEvents.Count > 0)
            {
                // Check whether parent min(I^R) belongs to allowable responses.
                ParentState.IncludedPendingResponseEvents.Sort();

                // if min(I^R) belongs to allowable responses, Assign the reank of min(I^R) to staterank.
                // else inherit rank from the parent.
                StateVector.StateRank = (allowableResponses.Contains(ParentState.IncludedPendingResponseEvents[0]))
                                            ? ParentState.IncludedPendingResponseEvents[0]
                                            : ParentState.StateRank;

                StateVector.StateAccepting = (allowableResponses.Contains(ParentState.IncludedPendingResponseEvents[0]))
                                                 ? true
                                                 : false;
            }
            else
            {
                StateVector.StateRank = ParentState.StateRank;

                StateVector.StateAccepting = false;
            }

            // Finally if we I^R = Ø then we assign the state to be accepting.
            if (StateVector.IncludedPendingResponseEvents.Count == 0) StateVector.StateAccepting = true;


            // Now it is time to compute the HigherPendingResponseEvents (M-set) for current state, since we have computed the
            // state rank for the current state.
            foreach (var pendingResponseEvent in StateVector.IncludedPendingResponseEvents)
            {
                if (pendingResponseEvent > StateVector.StateRank)
                    StateVector.HigherPendingResponseEvents.Add(pendingResponseEvent);
            }

            StateVector.HigherPendingResponseEvents.Sort();

        }

    }
}
