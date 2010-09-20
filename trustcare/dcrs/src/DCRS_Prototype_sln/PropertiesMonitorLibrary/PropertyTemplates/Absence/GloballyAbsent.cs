using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Absence
{
    public class GloballyAbsent : PropertyMonitorBase
    {
        // globally (absent (P))


        private readonly short actionP;

        #region Constructor.

        public GloballyAbsent(short actionP)
        {
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
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal,
                                     string executionTrace)
        {
            // Throw voilation if the current action is P.
            if (actionId == actionP)
            {
                // Prepare the voilation message by sending execution trace and other info.
                GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                // Return false to say that property has been voilated.
                return false;
            }


            return true;
        }

        #endregion
    }
}