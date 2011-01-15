namespace DCRStoFiniteAutomaton
{
    public class AutamatonSettings
    {

        public AutamatonSettings()
        {
            // The defaultmode is finite state.
            StateMode = AutomatonMode.FiniteSate;

        }


        public AutomatonMode StateMode;


        public bool ExcludeNonConditionalEvents;


    }
}
