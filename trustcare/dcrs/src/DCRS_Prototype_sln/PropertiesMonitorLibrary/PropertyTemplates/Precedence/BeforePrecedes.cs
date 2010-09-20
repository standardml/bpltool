using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Precedence
{
    public class BeforePrecedes : PropertyMonitorBase
    {

        // before (precedes (S, P), R)
        // before (precedes (12, 5), 15)

        // State Tracking Variables.
        // Tracking var for S
        private bool propertyParam0;
        // Tracking var for P
        private bool propertyParam1;
        // Tracking var for R
        private bool propertyParam2;


        private readonly short actionS;
        private readonly short actionP;
        private readonly short actionR;


        #region Constructor.

        public BeforePrecedes(short actionS, short actionP, short actionR)
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

            // If we have reached R, we don't need to monitor the property! 
            if (propertyParam2) return true;


            if (actionId == actionS)
            {
                // Action is S (12)
                // Here set S to true, if P is not true,
                // that means we only count S happened before P. and we dont care about 
                // S happened after P.
                if (!propertyParam1)
                    propertyParam0 = true;
            }
            else if (actionId == actionP)
            {
                // Action is P (5)
                // Simply set P to true.
                propertyParam1 = true;
            }
            else if (actionId == actionR)
            {
                // Action is R (15)
                // Check if P happened, then also S happened otherwise throw voilation.
                if ((propertyParam1) && (!propertyParam0))
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
