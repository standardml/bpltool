using System.Collections.Generic;
using System.Linq;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.WorkflowEngine.Core
{
    class DCRSFiniteStateProviderBuchi : DCRSFiniteStateProvider
    {

        protected DCRSFiniteStateProviderBuchi(long stateNumber, short leadingtransition, DCRSStateVector parentState, DCRSSpecification specification) 
            : base(stateNumber, leadingtransition, parentState, specification)
        {}

        public override DCRSState ComputeState()
        {
            base.ComputeState();

            ComputeStateForGeneralizedBuchi();

            return StateInternal;
        }


        private void ComputeStateForGeneralizedBuchi()
        {
            // This set corresponds to I^R \ (I'^R') U {e}

            // Calculate ParentState.IncludedPendingResponseEvents
            var parentIncludedPendingResponseActions = (List<short>)
                ParentState.PendingResponseActions.Intersect(ParentState.IncludedActions);

            // Caluclulate ParentState.HigherPendingResponseEvents
            var parentHigerPendingResponsesActions = parentIncludedPendingResponseActions.Where(
                pendingResponseAction => pendingResponseAction > ParentState.StateRank).ToList();

            parentHigerPendingResponsesActions.Sort();

            // Calculate StateInternal.StateVector.IncludedPendingResponseEvents..
            var currentIncludedPendingResponseActions = (List<short>)
                StateInternal.StateVector.PendingResponseActions.Intersect(ParentState.IncludedActions);


            //I^R \ (I'^R')
            var allowableResponses =
                parentIncludedPendingResponseActions.Where(
                    pendingResponseEvent => !currentIncludedPendingResponseActions.Contains(pendingResponseEvent)).
                    ToList();

            // Finally Check whether existing transition is added (U {e})
            if (!allowableResponses.Contains(PrimeLeadingtransition)) allowableResponses.Add(PrimeLeadingtransition);


            // Case 1: when M set is not empty..
            if (parentHigerPendingResponsesActions.Count > 0)
            {

                parentHigerPendingResponsesActions.Sort();

                // if min(M set) belongs to allowable responses, Assign the reank of min(Mset) to staterank.
                // else inherit rank from the parent.
                StateInternal.StateVector.StateRank = (allowableResponses.Contains(parentHigerPendingResponsesActions[0]))
                                            ? parentHigerPendingResponsesActions[0]
                                            : ParentState.StateRank;

                StateInternal.StateVector.StateAccepting = (allowableResponses.Contains(parentHigerPendingResponsesActions[0]))
                                                 ? true
                                                 : false;
            }
            else if (parentIncludedPendingResponseActions.Count > 0)
            {
                // Check whether parent min(I^R) belongs to allowable responses.
                parentIncludedPendingResponseActions.Sort();

                // if min(I^R) belongs to allowable responses, Assign the reank of min(I^R) to staterank.
                // else inherit rank from the parent.
                StateInternal.StateVector.StateRank = (allowableResponses.Contains(parentIncludedPendingResponseActions[0]))
                                            ? parentIncludedPendingResponseActions[0]
                                            : ParentState.StateRank;

                StateInternal.StateVector.StateAccepting = (allowableResponses.Contains(parentIncludedPendingResponseActions[0]))
                                                 ? true
                                                 : false;
            }
            else
            {
                StateInternal.StateVector.StateRank = ParentState.StateRank;

                StateInternal.StateVector.StateAccepting = false;
            }

            // Finally if we I^R = Ø then we assign the state to be accepting.
            if (currentIncludedPendingResponseActions.Count == 0) StateInternal.StateVector.StateAccepting = true;


            //// Now it is time to compute the HigherPendingResponseEvents (M-set) for current state, since we have computed the
            //// state rank for the current state.
            //// Not necessary, as we always calculate this in the child state..... 
            //foreach (var pendingResponseEvent in currentIncludedPendingResponseActions)
            //{
            //    if (pendingResponseEvent > StateVectorInternal.StateRank)
            //        StateVectorInternal.HigherPendingResponseEvents.Add(pendingResponseEvent);
            //}

            //StateVectorInternal.HigherPendingResponseEvents.Sort();

        }


    }
}
