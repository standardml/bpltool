﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ITU.DK.DCRS.CommonTypes;

namespace DCRStoFiniteAutomaton
{
    public class AtomicState
    {
        private readonly short transition;

        private readonly StateVector parentState;
        
        public StateVector StateVector  = new StateVector();

        //public string Name = string.Empty;

        StringBuilder printBuilder = new StringBuilder();

        //public Dictionary<short, long> IncomingTranssitions = new Dictionary<short, long>();

        //public Dictionary<short, long> OutgoingTranssitions = new Dictionary<short, long>();

        public TransitionList Transitions = new TransitionList();

        public AtomicState(long stateNumber, short transition, StateVector parentState)
        {
            StateNumber = stateNumber;

            this.transition = transition;

            this.parentState = parentState;
        }


        public long StateNumber { get; set; }

        public short LeadingTransition
        {
            get { return transition; }
        }


        public StateVector ComputeState()
        {

            UpdateExecutedEventsSet();


            UpdateIncludedEventSet();

            ComputeEnabledTransitions();

            UpdateResponseEvents();

            UpdateIncludedResponseEvents();

            ComputeCurrentState();


            return StateVector;
        }


        private void UpdateExecutedEventsSet()
        {
            // First Copy the set of executed Events (E).
            StateVector.ExecutedEvents.AddRange(parentState.ExecutedEvents.ToArray());

            //// Add the current transition to the E set.
            //if (!StateVector.ExecutedEvents.Contains(LeadingTransition) && (LeadingTransition != -1))
            //    StateVector.ExecutedEvents.Add(LeadingTransition);

            // NOTE: Chnages for new TAU action.. Start
            // Add the current transition to the E set, if it is not a TAU action and not dummy action at the start state.
            if (!StateVector.ExecutedEvents.Contains(LeadingTransition) && (LeadingTransition != -1) && (LeadingTransition != Utilities.TAU_ACTION ))
                StateVector.ExecutedEvents.Add(LeadingTransition);
            // NOTE: Chnages for new TAU action.. End.


            StateVector.ExecutedEvents.Sort();
            
        }

        private void UpdateIncludedEventSet()
        {
            
            // First Copy the set of Included Events (E).
            StateVector.IncludedEvents.AddRange(parentState.IncludedEvents.ToArray());

            // Conditions Relation in Specification is in the following format.
            // PM (0) >> S(1) := 1,0
            // S (1) >> GM (2):= 2,1


            // NOTE: Chnages for new TAU action.. Start
            // If the leading transition is a TAU action, then dont go further to check
            if(LeadingTransition == Utilities.TAU_ACTION)
            {
                StateVector.IncludedEvents.Sort();

                return;
            }
            
            
            // NOTE: Chnages for new TAU action.. End.



            // Update for Includes.
            for (var index = 0; index < StateManager.GetStateManagerInstance().Specification.Includes.GetLength(0); index++)
            {
                if (StateManager.GetStateManagerInstance().Specification.Includes[index, 0] != LeadingTransition) continue;
                if (!StateVector.IncludedEvents.Contains(StateManager.GetStateManagerInstance().Specification.Includes[index, 1]))
                {
                    StateVector.IncludedEvents.Add(
                        StateManager.GetStateManagerInstance().Specification.Includes[index, 1]);
                }
            }


            // Update for Excludes.
            for (var index = 0; index < StateManager.GetStateManagerInstance().Specification.Excludes.GetLength(0); index++)
            {
                if (StateManager.GetStateManagerInstance().Specification.Excludes[index, 0] != LeadingTransition) continue;
                if (StateVector.IncludedEvents.Contains(StateManager.GetStateManagerInstance().Specification.Excludes[index, 1]))
                {
                    StateVector.IncludedEvents.Remove(
                        StateManager.GetStateManagerInstance().Specification.Excludes[index, 1]);
                }
            }



            //// First include the included set from the parent
            //for (int index = 0; index < parentState.IncludedEvents.Count; index++)
            //{

            //    for (int index2 = 0; index2 < StateManager.GetStateManagerInstance().Specification.Conditions.GetLength(0); index2++)
            //    {
            //        if (StateManager.GetStateManagerInstance().Specification.Conditions[index2, 0] == parentState.IncludedEvents[index])
            //        {
            //            if (!StateVector.ExecutedEvents.Contains(StateManager.GetStateManagerInstance().Specification.Conditions[index2, 1]))
            //            {
            //                StateVector.IncludedEvents.Remove(
            //                    StateManager.GetStateManagerInstance().Specification.Conditions[index2, 0]);
            //            }
            //        }
            //    }
            //}


            StateVector.IncludedEvents.Sort();



        }

        private void ComputeEnabledTransitions()
        {

            // First copy the included Events to enabled transitions set
            StateVector.EnabledTransitions.AddRange(StateVector.IncludedEvents.ToArray());


            foreach (var enabledEvent in StateVector.EnabledTransitions.ToArray())
            {
                for (int index = 0; index < StateManager.GetStateManagerInstance().Specification.Conditions.GetLength(0); index++)
                {
                    if (StateManager.GetStateManagerInstance().Specification.Conditions[index, 0] != enabledEvent)
                        continue;
                    if (!StateVector.ExecutedEvents.Contains(StateManager.GetStateManagerInstance().Specification.Conditions[index, 1]))
                    {
                        StateVector.EnabledTransitions.Remove(
                            enabledEvent);
                    }
                }
            }



            // NOTE: Chnages for new TAU action.. Start
            // Add a TAU action so that there will be always a TAU action!
            if (!StateVector.EnabledTransitions.Contains(Utilities.TAU_ACTION))
                StateVector.EnabledTransitions.Add(Utilities.TAU_ACTION);
            // NOTE: Chnages for new TAU action.. End.


            StateVector.EnabledTransitions.Sort();


        }

        private void UpdateResponseEvents()
        {

            // First Copy the set of Included Events (E).
            StateVector.PendingResponseEvents.AddRange(parentState.PendingResponseEvents.ToArray());

            // Response Relation in Specification is in the following format.
            // PM (0) >> S(1)  := 0,1
            // PM (0) >> GM (2):= 0,2


            // NOTE: Chnages for new TAU action.. Start
            // If the leading transition is a TAU action, then dont go further to compute changes to pending response events set.
            if (LeadingTransition == Utilities.TAU_ACTION)
            {
                StateVector.PendingResponseEvents.Sort();

                return;
            }
            // NOTE: Chnages for new TAU action.. End.


            // Remove the current transition...
            StateVector.PendingResponseEvents.Remove(LeadingTransition);

            // Update for responses.
            for (var index = 0; index < StateManager.GetStateManagerInstance().Specification.Responses.GetLength(0); index++)
            {
                if (StateManager.GetStateManagerInstance().Specification.Responses[index, 0] != LeadingTransition) continue;

                if (!StateVector.PendingResponseEvents.Contains(StateManager.GetStateManagerInstance().Specification.Responses[index, 1]))
                {
                    StateVector.PendingResponseEvents.Add(
                        StateManager.GetStateManagerInstance().Specification.Responses[index, 1]);
                }
            }

            StateVector.PendingResponseEvents.Sort();


        }

        private void UpdateIncludedResponseEvents()
        {

            foreach (var response in StateVector.PendingResponseEvents)
            {

                if (StateVector.IncludedEvents.Contains(response))
                    StateVector.IncludedPendingResponseEvents.Add(response); 
            }

            StateVector.IncludedPendingResponseEvents.Sort();


        }


        private void ComputeCurrentState()
        {
            // This set corresponds to I^R \ (I'^R') U {e}
            var allowableResponses = new List<short>();

            foreach (short pendingResponseEvent in parentState.IncludedPendingResponseEvents)
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
            if(parentState.HigherPendingResponseEvents.Count > 0)
            {

                parentState.HigherPendingResponseEvents.Sort();

                // if min(M set) belongs to allowable responses, Assign the reank of min(Mset) to staterank.
                // else inherit rank from the parent.
                StateVector.StateRank = (allowableResponses.Contains(parentState.HigherPendingResponseEvents[0]))
                                            ? parentState.HigherPendingResponseEvents[0]
                                            : parentState.StateRank;

                StateVector.StateAccepting = (allowableResponses.Contains(parentState.HigherPendingResponseEvents[0]))
                                                 ? true
                                                 : false;
            }
            else if (parentState.IncludedPendingResponseEvents.Count > 0)
            {
                // Check whether parent min(I^R) belongs to allowable responses.
                parentState.IncludedPendingResponseEvents.Sort();

                // if min(I^R) belongs to allowable responses, Assign the reank of min(I^R) to staterank.
                // else inherit rank from the parent.
                StateVector.StateRank = (allowableResponses.Contains(parentState.IncludedPendingResponseEvents[0]))
                                            ? parentState.IncludedPendingResponseEvents[0]
                                            : parentState.StateRank;

                StateVector.StateAccepting = (allowableResponses.Contains(parentState.IncludedPendingResponseEvents[0]))
                                                 ? true
                                                 : false;
            }
            else
            {
                StateVector.StateRank = parentState.StateRank;

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

        public override string ToString()
        {

            printBuilder.Length = 0;

            //printBuilder.Append(Environment.NewLine);

            printBuilder.Append(string.Format("s{0};", StateNumber));

            //printBuilder.Append(string.Format("{0};",
            //                                  ((LeadingTransition != -1)
            //                                       ? StateManager.GetStateManagerInstance().Specification.ActionList[
            //                                             LeadingTransition]
            //                                       : string.Empty)));


            printBuilder.Append(string.Format("{0};",
                                              Utilities.GetTransitionLabel(
                                                  StateManager.GetStateManagerInstance().Specification.ActionList,
                                                  LeadingTransition)));
 
            printBuilder.Append(string.Format("{0};", PrintList(StateVector.ExecutedEvents)));

            printBuilder.Append(string.Format("{0};", PrintList(StateVector.IncludedEvents)));

            printBuilder.Append(string.Format("{0};", PrintList(StateVector.PendingResponseEvents)));

            printBuilder.Append(string.Format("{0};", PrintList(StateVector.IncludedPendingResponseEvents)));

            printBuilder.Append(StateVector.StateRank + ";");

            printBuilder.Append(string.Format("{0};", PrintList(StateVector.HigherPendingResponseEvents)));

            printBuilder.Append(StateVector.StateAccepting + ";");

            printBuilder.Append(string.Format("{0};", PrintList(StateVector.EnabledTransitions)));

            printBuilder.Append(PrintTransitions());
       

            return printBuilder.ToString();

        }

        private static string PrintList(IEnumerable<short> list)
        {
            
            //foreach (var listmember in list)
            //{
            //    //val += StateManager.GetStateManagerInstance().Specification.ActionList[listmember] + ",";

            //    val += Utilities.GetTransitionLabel(
            //        StateManager.GetStateManagerInstance().Specification.ActionList,
            //        listmember) + ",";

            //}

            var val = list.Aggregate(string.Empty, (current, listmember) => current + (Utilities.GetTransitionLabel(StateManager.GetStateManagerInstance().Specification.ActionList, listmember) + ","));

            if (val.Length > 0) val.Remove(val.Length - 1);

            return val;

        }

        private string PrintTransitions()
        {

            string transitionStr = string.Empty;

            foreach (Transition trans in Transitions)
            {
                if(trans.Direction == TransitionDirection.Incoming)
                {
                    //transitionStr += string.Format("s{0}--{1}--s{2},", trans.StateNumber,
                    //                               StateManager.GetStateManagerInstance().Specification.ActionList[
                    //                                   trans.Label], StateNumber);
                }
                else
                {
                    //transitionStr += string.Format("s{0}--{1}--s{2},", StateNumber,
                    //                               StateManager.GetStateManagerInstance().Specification.ActionList[
                    //                                   trans.Label], trans.StateNumber);

                    transitionStr += string.Format("s{0}--{1}--s{2},", StateNumber,
                                                   Utilities.GetTransitionLabel(
                                                       StateManager.GetStateManagerInstance().Specification.ActionList,
                                                       trans.Label)
                                                   , trans.StateNumber);
                }
            }

            return transitionStr;

        }


    }
}