using System.Collections.Generic;
using System.Linq;

namespace DCRStoFiniteAutomaton
{
    public class StateVector
    {

        public List<short> IncludedEvents = new List<short>();

        public List<short> ExecutedEvents = new List<short>();

        public List<short> PendingResponseEvents = new List<short>();

        public short StateRank;

        public bool StateAccepting;

        /// <summary>
        /// This corresponds to I And R
        /// </summary>
        public List<short> IncludedPendingResponseEvents = new List<short>();
        /// <summary>
        /// This corresponds to M set containing events higher than state rank.
        /// </summary>
        public List<short> HigherPendingResponseEvents = new List<short>();

        public List<short> EnabledTransitions = new List<short>();

        public bool Equals(StateVector gueststate)
        {
            if((IncludedEvents.Count != gueststate.IncludedEvents.Count)||
                (ExecutedEvents.Count != gueststate.ExecutedEvents.Count)||
                (PendingResponseEvents.Count != gueststate.PendingResponseEvents.Count)||
                (StateRank != gueststate.StateRank)||
                (StateAccepting != gueststate.StateAccepting))
            {
                return false;
            }

            return CompareLists(IncludedEvents, gueststate.IncludedEvents) &&
                   CompareLists(ExecutedEvents, gueststate.ExecutedEvents) &&
                   CompareLists(PendingResponseEvents, gueststate.PendingResponseEvents);

        }

        private static bool CompareLists(ICollection<short> list1, ICollection<short> list2)
        {
            return list1.Count == list2.Count && list1.SequenceEqual(list2);
        }
    }
}
