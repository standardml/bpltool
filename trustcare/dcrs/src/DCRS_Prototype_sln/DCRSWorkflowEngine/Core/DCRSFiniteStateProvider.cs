using System.Collections.Generic;
using System.Linq;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.WorkflowEngine.Core
{
    public class DCRSFiniteStateProvider
    {
        #region Internal Data

        protected DCRSState StateInternal = new DCRSState();
      
        protected readonly short PrimeLeadingtransition;

        protected readonly DCRSStateVector ParentState;

        protected readonly DCRSSpecification SpecificationInternal;

        protected bool IsStateComputed;
        
        #endregion

        #region Constructor.
        public DCRSFiniteStateProvider(long stateNumber, short leadingtransition, 
            DCRSStateVector parentState, DCRSSpecification specification)
        {
            StateInternal.StateNumber = stateNumber;

            PrimeLeadingtransition = leadingtransition;

            ParentState = parentState;

            SpecificationInternal = specification;

            StateInternal.StateVector = new DCRSStateVector();

            StateInternal.EnabledActions = new List<short>();


        } 
        #endregion
        
        #region Public Properties.

        public short PrimeLeadingTransition
        {
            get { return PrimeLeadingtransition; }
        }

        public List<short> IncludedPendingResponse
        {
            get
            {
                return new List<short>(StateInternal.StateVector.PendingResponseActions.Intersect(StateInternal.StateVector.IncludedActions));
                    
            }
        }
        
        

        #endregion

        #region Public and Virtual Methods

        public DCRSStateVector GetStateVector()
        {
            if (!IsStateComputed) ComputeState();

            return StateInternal.StateVector;
        }

        


        public virtual DCRSState ComputeState()
        {

            UpdateExecutedActionsSet();

            UpdateIncludedActionsSet();

            UpdatePendingResponseActions();

            ComputeEnabledTransitions();

            // Assign wehther the state is accepting or not.
            // In case of infinite runs, it will be overwritten by DCRSAtomicStateBuchi.
            StateInternal.StateVector.StateAccepting = (IncludedPendingResponse.Count == 0);
            // StateRank is always 0 for finite runs, as we have only one copy of the state.
            StateInternal.StateVector.StateRank = 0;

            // The job of updating transitions will be done by container (like StateManager), 
            // who knows much better how to link states.
            IsStateComputed = true;

            return StateInternal;

        }

        
        #endregion

        #region Private State Computation Methods

        private void UpdateExecutedActionsSet()
        {
            // First Copy the set of executed Events (E).
            StateInternal.StateVector.ExecutedActions.AddRange(ParentState.ExecutedActions.ToArray());

            // Add the current transition to the E set.
            if (!StateInternal.StateVector.ExecutedActions.Contains(PrimeLeadingtransition) && (PrimeLeadingtransition != -1))
                StateInternal.StateVector.ExecutedActions.Add(PrimeLeadingtransition);

            StateInternal.StateVector.ExecutedActions.Sort();
        }

        private void UpdateIncludedActionsSet()
        {
            // First Copy the set of Included Events (E).
            StateInternal.StateVector.IncludedActions.AddRange(ParentState.IncludedActions.ToArray());

            // Conditions Relation in Specification is in the following format.
            // PM (0) >> S(1) := 1,0
            // S (1) >> GM (2):= 2,1

            // Update for Includes.
            for (var index = 0; index < SpecificationInternal.Includes.GetLength(0); index++)
            {
                if (SpecificationInternal.Includes[index, 0] != PrimeLeadingtransition) continue;
                if (!StateInternal.StateVector.IncludedActions.Contains(SpecificationInternal.Includes[index, 1]))
                {
                    StateInternal.StateVector.IncludedActions.Add(
                        SpecificationInternal.Includes[index, 1]);
                }
            }


            // Update for Excludes.
            for (var index = 0; index < SpecificationInternal.Excludes.GetLength(0); index++)
            {
                if (SpecificationInternal.Excludes[index, 0] != PrimeLeadingtransition) continue;
                if (StateInternal.StateVector.IncludedActions.Contains(SpecificationInternal.Excludes[index, 1]))
                {
                    StateInternal.StateVector.IncludedActions.Remove(
                        SpecificationInternal.Excludes[index, 1]);
                }
            }


            StateInternal.StateVector.IncludedActions.Sort();
        }

        private void ComputeEnabledTransitions()
        {

            // First copy the included Events to enabled transitions set
            StateInternal.EnabledActions.AddRange(StateInternal.StateVector.IncludedActions.ToArray());


            foreach (var enabledEvent in StateInternal.EnabledActions.ToArray())
            {
                for (var index = 0; index < SpecificationInternal.Conditions.GetLength(0); index++)
                {
                    if (SpecificationInternal.Conditions[index, 0] != enabledEvent)
                        continue;
                    // Conditions only hold incase the required state is currently included.
                    if (!StateInternal.StateVector.IncludedActions.Contains(SpecificationInternal.Conditions[index, 1]))
                        continue;
                    if (!StateInternal.StateVector.ExecutedActions.Contains(SpecificationInternal.Conditions[index, 1]))
                    {
                        StateInternal.EnabledActions.Remove(
                            enabledEvent);
                    }
                }
            }

            StateInternal.EnabledActions.Sort();

        }

        private void UpdatePendingResponseActions()
        {

            // First Copy the set of Included Events (E).
            StateInternal.StateVector.PendingResponseActions.AddRange(ParentState.PendingResponseActions.ToArray());

            // Response Relation in Specification is in the following format.
            // PM (0) >> S(1)  := 0,1
            // PM (0) >> GM (2):= 0,2

            // Remove the current transition...
            StateInternal.StateVector.PendingResponseActions.Remove(PrimeLeadingtransition);

            // Update for responses.
            for (var index = 0; index < SpecificationInternal.Responses.GetLength(0); index++)
            {
                if (SpecificationInternal.Responses[index, 0] != PrimeLeadingtransition) continue;

                if (!StateInternal.StateVector.PendingResponseActions.Contains(SpecificationInternal.Responses[index, 1]))
                {
                    StateInternal.StateVector.PendingResponseActions.Add(
                        SpecificationInternal.Responses[index, 1]);
                }
            }

            StateInternal.StateVector.PendingResponseActions.Sort();

        }
        
        #endregion

    }
}
