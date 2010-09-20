using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace DCRStoFiniteAutomaton
{
    public class TransitionList : List<Transition>
    {
        
        
        public Transition GetTransition(short transitionLabel, TransitionDirection direction)
        {
            foreach (var transition in this)
            {
                if ((transition.Direction == direction) && (transitionLabel == transition.Label))
                    return transition;

            }

            return null;
        }

        public Transition GetTransition(long stateNumber, TransitionDirection direction)
        {
            foreach (var transition in this)
            {
                if ((transition.Direction == direction) && (stateNumber == transition.StateNumber))
                    return transition;

            }

            return null;
        }






    }
}
