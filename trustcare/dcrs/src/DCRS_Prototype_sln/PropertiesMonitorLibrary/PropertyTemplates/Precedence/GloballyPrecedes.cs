using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Precedence
{
    public class GloballyPrecedes : PropertyMonitorBase
    {

        // globally (precedes (S, P))
        // globally (precedes (12, 5))

        // State Tracking Variables.
        // Tracking var for S
        private bool propertyParam0;
        // Tracking var for P
        private bool propertyParam1;

        private readonly short actionS;
        private readonly short actionP;

        
        #region Constructor.

        public GloballyPrecedes(short actionS, short actionP)
        {
            this.actionS = actionS;
            this.actionP = actionP;

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
            if (propertyParam1) return true;


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
                // Check if S has happened, otherwise throw error.
                if (!propertyParam0)
                {
                    // Prepare the voilation message by sending execution trace and other info.
                    GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                    // Return false to say that property has been voilated.
                    return false;
                }
                propertyParam1 = true;
            }


            return true;
        }

        #endregion
    }
}
