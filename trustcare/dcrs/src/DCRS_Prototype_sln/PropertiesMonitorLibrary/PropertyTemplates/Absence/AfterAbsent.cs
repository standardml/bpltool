using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Absence
{
    public class AfterAbsent : PropertyMonitorBase
    {
        // after( absent(P), Q)

        private readonly short actionP;
        private readonly short actionQ;

        // Tracking var for P
        private bool propertyParam0;
        // Tracking var for Q
        private bool propertyParam1;

        #region Constructor.

        public AfterAbsent(short actionP, short actionQ)
        {
            this.actionP = actionP;
            this.actionQ = actionQ;
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
            // If we have not reached Q, then we dont monitor the property.
            if (!propertyParam1) return true;

            if (actionId == actionP)
            {
                // If we comehere, then we might have crossed Q, so throw voialtion.
                // Prepare the voilation message by sending execution trace and other info.
                GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                // Return false to say that property has been voilated.
                return false;
            }

            if (actionId == actionQ)
            {
                // Start monitoring the property.
                propertyParam1 = true;
            }

            return true;
        }

        #endregion
    }
}