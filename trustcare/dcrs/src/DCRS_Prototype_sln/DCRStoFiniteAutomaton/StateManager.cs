using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;
using ITU.DK.DCRS.CommonTypes.OldProcessDefinition;

namespace DCRStoFiniteAutomaton
{
    public class StateManager
    {

        private static readonly StateManager Manager = new StateManager();

        private long _stateNumber;

        private StateRepository _stateRepository;



        public DCRSModel Specification;

        public AutomatonMode StateMode;

        

        private Dictionary<long, AtomicState> reformatedStates;
        
        
        private StateManager()
        {
            
        }

        public Dictionary<long, AtomicState> ComputedStates
        {
            get { return _stateRepository.ComputedStates; }
        }


        public static StateManager GetStateManagerInstance()
        {
            return Manager;
        }


        public Dictionary<long, AtomicState> ComputeStateSpace()
        {
            // Initialize _stateRepository
            _stateRepository = new StateRepository();
            // Reset the state number.
            _stateNumber = 1000;


            var parentStateVectorForInitialState = InitializeParentStateVectorForInitialState();

            // Call Initial state
            AtomicState state0;
            


            // Call the respective automaton state class based on StateMode.
            if(StateMode == AutomatonMode.Buchi)
            {
                state0 = new BuchiAutomatonState(_stateNumber, -1, parentStateVectorForInitialState);
            }
            else
            {
                state0 = new FiniteAutamatonState(_stateNumber, -1, parentStateVectorForInitialState);
            }



            //var stateVector = state0.ComputeState();

            // Add the initial state to to be computed states.
            _stateRepository.TobeComputedStates.Add(state0);
            

            while (_stateRepository.TobeComputedStates.Count > 0)
            {
                var atomicState = _stateRepository.TobeComputedStates.ElementAt(0);

                // Claer the state from list.
                _stateRepository.TobeComputedStates.RemoveAt(0);

                atomicState.ComputeState();

                var matchingState = _stateRepository.FindMatchingState(atomicState.StateVector);

                if (matchingState != null)
                {
                    UpdateMatchingStateFound(atomicState, matchingState);
                }
                else
                {
                    GenerateTransitionStates(atomicState);
                }
            }


            // Print the states.
            foreach (var keyValuePair in _stateRepository.ComputedStates)
            {

                Console.WriteLine(keyValuePair.Value);

            }

            //var keysArray = _stateRepository.ComputedStates.Keys.ToArray();


            //ReformatStateSpace();


            //// Print the states.
            //foreach (var keyValuePair in reformatedStates)
            //{

            //    Console.WriteLine(keyValuePair.Value);

            //}

            long newstateNumber = 0;

            foreach (var keyValPair in _stateRepository.ComputedStates)
            {

                RenameStates(keyValPair.Key, newstateNumber);

                newstateNumber++;

            }

            // Print the states.
            foreach (var keyValuePair in _stateRepository.ComputedStates)
            {

                Console.WriteLine(keyValuePair.Value);

            }


            
            return _stateRepository.ComputedStates;







        }

        private void RenameStates(long oldStateNumber, long newStateNumber)
        {

            foreach (var keyValPair in _stateRepository.ComputedStates)
            {
                var state = keyValPair.Value;

                if(state.StateNumber == oldStateNumber)
                {
                    state.StateNumber = newStateNumber;
                }

                foreach (var transition in state.Transitions)
                {
                    
                    if(transition.StateNumber == oldStateNumber)
                    {
                        transition.StateNumber = newStateNumber;
                    }

                }
            }

        }


        private void ReformatStateSpace()
        {

            
            reformatedStates = new Dictionary<long, AtomicState>();

            var keysArray = _stateRepository.ComputedStates.Keys.ToArray();

            long newstateNumber = 0;

            foreach (var stateNo in keysArray)
            {
                
                // First get the state from computed states.
                var state = _stateRepository.ComputedStates[stateNo];

                var reNamedState = ReNameState(state, newstateNumber);

                ComputedStates.Add(newstateNumber, reNamedState);

                newstateNumber++;

            }


            //return reformatedStates;

        }


        private AtomicState ReNameState(AtomicState state, long newSN)
        {

            foreach (var transition in state.Transitions)
            {
                //var childState = _stateRepository.ComputedStates[transition.StateNumber] ??
                //                 reformatedStates[transition.StateNumber];


                var otherState = (_stateRepository.ComputedStates.ContainsKey(transition.StateNumber))
                                     ? _stateRepository.ComputedStates[transition.StateNumber]
                                     : ComputedStates[transition.StateNumber];


                if(transition.Direction == TransitionDirection.Incoming)
                {
                    
                    // First Try getting it from computed states, otherwise get it from renamed states.

                    
                    //(_stateRepository.ComputedStates.ContainsKey(transition.StateNumber))


                    var transition1 = otherState.Transitions.GetTransition(state.StateNumber, TransitionDirection.Outgoing);

                    transition1.StateNumber = newSN;
                }
                else
                {
                    var transition2 = otherState.Transitions.GetTransition(state.StateNumber, TransitionDirection.Incoming);

                    transition2.StateNumber = newSN;

                }
            }

            state.StateNumber = newSN;

            return state;

        }


        private void GenerateTransitionStates(AtomicState state)
        {
            foreach (var enabledTransition in state.StateVector.EnabledTransitions)
            {
                // Create a new state for each tranistion.
                AtomicState childstate;

                // Call the respective automaton state class based on StateMode.
                if (StateMode == AutomatonMode.Buchi)
                {
                    childstate = new BuchiAutomatonState(++_stateNumber, enabledTransition, state.StateVector);
                }
                else
                {
                    childstate = new FiniteAutamatonState(++_stateNumber, enabledTransition, state.StateVector);
                }


                //childstate = new AtomicState(++_stateNumber, enabledTransition, state.StateVector);


                // Update out going transition from state
                state.Transitions.Add(new Transition
                {
                    Direction = TransitionDirection.Outgoing,
                    StateNumber = childstate.StateNumber,
                    Label = enabledTransition
                });


                // Update the child state with incoming transition.
                childstate.Transitions.Add(new Transition
                {
                    Direction = TransitionDirection.Incoming,
                    StateNumber = state.StateNumber,
                    Label = enabledTransition
                });

                // Add the newly created state to to be computed states.
                _stateRepository.TobeComputedStates.Add(childstate);
            }


            //// NOTE: Implementation for TAU action -- Start
            //// Add an explicit TAU Action to all the states.

            //// Create a new state for each tranistion.
            //var newTauState = new AtomicState(++_stateNumber, Utilities.TAU_ACTION, state.StateVector);

            //// Update out going transition from state
            //state.Transitions.Add(new Transition
            //{
            //    Direction = TransitionDirection.Outgoing,
            //    StateNumber = newTauState.StateNumber,
            //    Label = Utilities.TAU_ACTION
            //});


            //// Update the child state with incoming transition.
            //newTauState.Transitions.Add(new Transition
            //{
            //    Direction = TransitionDirection.Incoming,
            //    StateNumber = state.StateNumber,
            //    Label = Utilities.TAU_ACTION
            //});

            //// Add the newly created state to to be computed states.
            //_stateRepository.TobeComputedStates.Add(newTauState);

            //// NOTE: Implementation for TAU action -- End


            
            // Finally Add the state to the computed states.
            
            _stateRepository.ComputedStates.Add(state.StateNumber, state);


        }

        private void UpdateMatchingStateFound(AtomicState state, AtomicState matchingSate)
        {
            var leadingtransition = state.Transitions.GetTransition(state.LeadingTransition,
                                                                    TransitionDirection.Incoming);
            // Get the parent state from the atomic state and update state number.
            var parentState = _stateRepository.ComputedStates[leadingtransition.StateNumber];

            var outgoingTransition = parentState.Transitions.GetTransition(state.StateNumber, TransitionDirection.Outgoing);

            outgoingTransition.StateNumber = matchingSate.StateNumber;


            matchingSate.Transitions.Add(new Transition
                                             {
                                                 Direction = TransitionDirection.Incoming,
                                                 Label = state.LeadingTransition,
                                                 StateNumber = parentState.StateNumber
                                             });
            
        }


        private StateVector InitializeParentStateVectorForInitialState()
        {

            // Start with Initial state.
            var parentVectorForIniatialState = new StateVector();

            parentVectorForIniatialState.IncludedEvents.AddRange(Specification.ActionList.Keys.ToArray());

            for (var index = 0; index < Specification.InitialState.GetLength(0); index++)
            {
                if(Specification.InitialState[index,1] == 0)
                {
                    parentVectorForIniatialState.IncludedEvents.Remove((short) index);
                }
            }


            parentVectorForIniatialState.IncludedEvents.Sort();

            parentVectorForIniatialState.StateRank = 0;
 
            return parentVectorForIniatialState;

        }

        //private AtomicState FindMatchingState(StateVector searchVector)
        //{

        //    foreach (var computedState in _stateRepository.ComputedStates)
        //    {
        //        if(computedState.Value.StateVector.Equals(searchVector))
        //        {
        //            return computedState.Value;
        //        }
        //    }

        //    return null;
        //}







    }
}
