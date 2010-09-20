using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Absence
{
    public class AfterUntilAbsent : PropertyMonitorBase
    {
        // Note: AfterUntil is same as BetweenAbsent, so same code!

        // between and will have many instances of scope instead of just one instance
        // like in case of before and after.

        // between (absent (P), Q, R)

        // State Tracking Variables.
        // Tracking var for P
        //private bool propertyParam0;
        // Tracking var for Q
        // Tracking var for R
        //private bool propertyParam2;


        private readonly short actionP;
        private readonly short actionQ;
        private readonly short actionR;
        private bool propertyParam1;

        #region Constructor.

        public AfterUntilAbsent(short actionP, short actionQ, short actionR)
        {
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
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal,
                                     string executionTrace)
        {
            if (actionId == actionP)
            {
                // executedAction = P
                // if  Q is true, then throw voilation
                if (propertyParam1)
                {
                    // Prepare the voilation message by sending execution trace and other info.
                    GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                    // Return false to say that property has been voilated.
                    return false;
                }
            }
            else if (actionId == actionQ)
            {
                // executedAction = Q
                // Assign to true to Q, to enable checking of property.
                propertyParam1 = true;
            }
            else if (actionId == actionR)
            {
                // executedAction = R

                // Finally set Q to false to disable checking until we get Q.
                propertyParam1 = false;
            }

            return true;
        }

        #endregion
    }
}
