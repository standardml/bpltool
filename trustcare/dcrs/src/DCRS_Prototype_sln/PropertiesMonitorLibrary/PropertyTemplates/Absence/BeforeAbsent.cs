using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Absence
{
    public class BeforeAbsent : PropertyMonitorBase
    {
        // before (absent (P), R)

        // State Tracking Variables.
        // Tracking var for P

        private readonly short actionP;
        private readonly short actionR;
        private bool propertyParam0;
        // Tracking var for R
        private bool propertyParam1;

        #region Constructor.

        public BeforeAbsent(short actionP, short actionR)
        {
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
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal,
                                     string executionTrace)
        {
            // We dont want to monitor the property if we reach R.
            if (propertyParam1) return true;


            if (actionId == actionP)
            {
                propertyParam0 = true;
            }
            else if (actionId == actionR)
            {
                // At R check if P has already happened, then throw voilation.
                if (propertyParam0)
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