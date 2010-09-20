using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Response
{
    public class BetweenResponds : PropertyMonitorBase
    {

        // between (precedes (S, P), Q, R)
        // between (precedes (12, 5), 15, 6)

        // State Tracking Variables.
        // Tracking var for S
        private bool propertyParam0;
        // Tracking var for P
        private bool propertyParam1;
        // Tracking var for Q
        private bool propertyParam2;
        // Tracking var for R
        //private bool propertyParam3;


        private readonly short actionS;
        private readonly short actionP;
        private readonly short actionQ;
        private readonly short actionR;

        #region Constructor.

        public BetweenResponds(short actionS, short actionP, short actionQ, short actionR)
        {
            this.actionS = actionS;
            this.actionP = actionP;
            this.actionQ = actionQ;
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
            if (actionId == actionP)
            {
                // Action is P (12)
                // Set P to true and reset S to flase if Q is true only...
                if (propertyParam2)
                {
                    propertyParam0 = true;

                    propertyParam1 = false;
                }
            }
            else if (actionId == actionS)
            {
                // Action is S (5)
                // Do the check only if Q is true.    
                // Here we count S only if P is true, otherwise we ignore.
                // That means we only count happenings of S after P
                if (propertyParam2)
                    if (propertyParam0) propertyParam1 = true;
            }
            else if (actionId == actionQ)
            {
                // Action is Q (15)
                // executedAction = Q
                // Assign to true to Q, to enable checking of property.
                propertyParam2 = true;
            }
            else if (actionId == actionR)
            {
                // Action is R (6)
                // The following check is only made if Q is true.    
                // Here check for the case where P happened, but not followed by S,
                // then throw error otherwise just set R to true.

                if (propertyParam2)
                {
                    if ((propertyParam0) && (!propertyParam1))
                    {
                        // Prepare the voilation message by sending execution trace and other info.
                        GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                        // Return false to say that property has been voilated.
                        return false;
                    }

                    // Finally set P, S and Q to false to disable checking until we get Q.
                    propertyParam2 = false;

                    propertyParam0 = false;

                    propertyParam1 = false;
                }
            }


            return true;
        }

        #endregion
    }
}
