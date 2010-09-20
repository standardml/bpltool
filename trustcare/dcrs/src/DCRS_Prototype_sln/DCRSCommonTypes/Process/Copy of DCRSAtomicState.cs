//using System.Collections.Generic;
//using System.Linq;

//namespace DCRSCommonTypes.Process
//{
//    public class Backup_DCRSAtomicState
//    {
//        #region Internal Data
//        protected DCRSStateVector StateVectorInternal;

//        public List<short> EnabledActions { get; protected set; }

//        public StateTransitionList LeadingTransitions = new StateTransitionList();

//        public StateTransitionList FollowingTransitions = new StateTransitionList();

//        public long StateNumber { get; set; }

//        protected readonly short primeLeadingtransition;

//        protected readonly DCRSStateVector ParentState;

//        protected readonly DCRSSpecification SpecificationInternal;

//        protected bool IsStateComputed;
        
//        #endregion

//        #region Constructor.
//        public Backup_DCRSAtomicState(long stateNumber, short leadingtransition, 
//            DCRSStateVector parentState, DCRSSpecification specification)
//        {
//            StateNumber = stateNumber;

//            primeLeadingtransition = leadingtransition;

//            ParentState = parentState;

//            SpecificationInternal = specification;

//            StateVectorInternal = new DCRSStateVector();

//            EnabledActions = new List<short>();


//        } 
//        #endregion
        
//        #region Public Properties.

//        public short PrimeLeadingTransition
//        {
//            get { return primeLeadingtransition; }
//        }

//        public List<short> IncludedPendingResponse
//        {
//            get
//            {
//                return
//                    (List<short>)
//                    StateVectorInternal.PendingResponseActions.Intersect(StateVectorInternal.IncludedActions);
//            }
//        }
        
        

//        #endregion

//        #region Public and Virtual Methods

//        public DCRSStateVector GetStateVector()
//        {
//            if (!IsStateComputed) ComputeState();

//            return StateVectorInternal;
//        }

        


//        public virtual DCRSStateVector ComputeState()
//        {

//            UpdateExecutedActionsSet();

//            UpdateIncludedActionsSet();

//            UpdatePendingResponseActions();

//            ComputeEnabledTransitions();

//            // Assign wehther the state is accepting or not.
//            // In case of infinite runs, it will be overwritten by DCRSAtomicStateBuchi.
//            StateVectorInternal.StateAccepting = (IncludedPendingResponse.Count == 0);
//            // StateRank is always 0 for finite runs, as we have only one copy of the state.
//            StateVectorInternal.StateRank = 0;

//            // The job of updating transitions will be done by container (like StateManager), 
//            // who knows much better how to link states.

//            IsStateComputed = true;

//            return StateVectorInternal;

//        }

        
//        #endregion

//        #region Private State Computation Methods

//        private void UpdateExecutedActionsSet()
//        {
//            // First Copy the set of executed Events (E).
//            StateVectorInternal.ExecutedActions.AddRange(ParentState.ExecutedActions.ToArray());

//            // Add the current transition to the E set.
//            if (!StateVectorInternal.ExecutedActions.Contains(primeLeadingtransition) && (primeLeadingtransition != -1))
//                StateVectorInternal.ExecutedActions.Add(primeLeadingtransition);

//            StateVectorInternal.ExecutedActions.Sort();
//        }

//        private void UpdateIncludedActionsSet()
//        {
//            // First Copy the set of Included Events (E).
//            StateVectorInternal.IncludedActions.AddRange(ParentState.IncludedActions.ToArray());

//            // Conditions Relation in Specification is in the following format.
//            // PM (0) >> S(1) := 1,0
//            // S (1) >> GM (2):= 2,1

//            // Update for Includes.
//            for (var index = 0; index < SpecificationInternal.Includes.GetLength(0); index++)
//            {
//                if (SpecificationInternal.Includes[index, 0] != primeLeadingtransition) continue;
//                if (!StateVectorInternal.IncludedActions.Contains(SpecificationInternal.Includes[index, 1]))
//                {
//                    StateVectorInternal.IncludedActions.Add(
//                        SpecificationInternal.Includes[index, 1]);
//                }
//            }


//            // Update for Excludes.
//            for (var index = 0; index < SpecificationInternal.Excludes.GetLength(0); index++)
//            {
//                if (SpecificationInternal.Excludes[index, 0] != primeLeadingtransition) continue;
//                if (StateVectorInternal.IncludedActions.Contains(SpecificationInternal.Excludes[index, 1]))
//                {
//                    StateVectorInternal.IncludedActions.Remove(
//                        SpecificationInternal.Excludes[index, 1]);
//                }
//            }


//            StateVectorInternal.IncludedActions.Sort();
//        }

//        private void ComputeEnabledTransitions()
//        {

//            // First copy the included Events to enabled transitions set
//            EnabledActions.AddRange(StateVectorInternal.IncludedActions.ToArray());


//            foreach (var enabledEvent in EnabledActions.ToArray())
//            {
//                for (var index = 0; index < SpecificationInternal.Conditions.GetLength(0); index++)
//                {
//                    if (SpecificationInternal.Conditions[index, 0] != enabledEvent)
//                        continue;
//                    if (!StateVectorInternal.ExecutedActions.Contains(SpecificationInternal.Conditions[index, 1]))
//                    {
//                        EnabledActions.Remove(
//                            enabledEvent);
//                    }
//                }
//            }

//            EnabledActions.Sort();

//        }

//        private void UpdatePendingResponseActions()
//        {

//            // First Copy the set of Included Events (E).
//            StateVectorInternal.PendingResponseActions.AddRange(ParentState.PendingResponseActions.ToArray());

//            // Response Relation in Specification is in the following format.
//            // PM (0) >> S(1)  := 0,1
//            // PM (0) >> GM (2):= 0,2

//            // Remove the current transition...
//            StateVectorInternal.PendingResponseActions.Remove(primeLeadingtransition);

//            // Update for responses.
//            for (var index = 0; index < SpecificationInternal.Responses.GetLength(0); index++)
//            {
//                if (SpecificationInternal.Responses[index, 0] != primeLeadingtransition) continue;

//                if (!StateVectorInternal.PendingResponseActions.Contains(SpecificationInternal.Responses[index, 1]))
//                {
//                    StateVectorInternal.PendingResponseActions.Add(
//                        SpecificationInternal.Responses[index, 1]);
//                }
//            }

//            StateVectorInternal.PendingResponseActions.Sort();

//        }
        
//        #endregion

//    }
//}
