using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Response
{
    public class BeforeResponds : PropertyMonitorBase
    {

        // before (responds (P, S), R)
        // before (responds (12, 5), 15)

        // State Tracking Variables.
        // Tracking var for P
        private bool propertyParam0;
        // Tracking var for S
        private bool propertyParam1;
        // Tracking var for R
        private bool propertyParam2;

        private readonly short actionS;
        private readonly short actionP;
        private readonly short actionR;


        #region Constructor.

        public BeforeResponds(short actionS, short actionP, short actionR)
        {
            this.actionS = actionS;
            this.actionP = actionP;
            this.actionR = actionR;
        }

        #endregion





        #region Overrides of PropertyMonitorBase

        /// <summary>
        /// This method will check if the property is voilated by the execution instance of workflow.
        /// </summary>
        /// <param name="processId"></param>
        /// <param name="processInstanceId"></param>
        /// <param name="actionId"></param>
        /// <param name="principal"></param>
        /// <param name="executionTrace"></param>
        /// <returns></returns>
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal, string executionTrace)
        {

            // If we reach R already, then simply exit as we dont need to monitor the 
            // property after R.
            if (propertyParam2) return true;


            if (actionId == actionP)
            {
                // Action is P (12)
                // Set P to true and reset S to flase
                propertyParam0 = true;

                propertyParam1 = false;
            }
            else if (actionId == actionS)
            {
                // Action is S (5)
                // Here we count S only if P is true, otherwise we ignore.
                // That means we only count happenings of S after P
                if (propertyParam0) propertyParam1 = true;
            }
            else if (actionId == actionR)
            {
                // Action is R (15)
                // Here check for the case where P happened, but not followed by S,
                // then throw error otherwise just set R to true.

                if ((propertyParam0) && (!propertyParam1))
                {
                    // Prepare the voilation message by sending execution trace and other info.
                    GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                    // Return false to say that property has been voilated.
                    return false;
                }

                propertyParam2 = true;
            }


            return true;
        }

        #endregion
    }
}
