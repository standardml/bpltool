using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Precedence
{
    public class AfterPrecedes : PropertyMonitorBase
    {
        private readonly short actionS;
        private readonly short actionP;
        private readonly short actionQ;

        // After (precedes (S, P), Q)
        // After (precedes (12, 5), 15)

        // State Tracking Variables.
        // Tracking var for S
        private bool propertyParam0;
        // Tracking var for P
        //private bool propertyParam1;
        // Tracking var for Q
        private bool propertyParam2;

        #region Constructor.

        public AfterPrecedes(short actionS, short actionP, short actionQ)
        {
            this.actionS = actionS;
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
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal, string executionTrace)
        {
            
            
            if (actionId == actionS)
            {
                // Action is S (12)    
                // If Q is true, that is check is enabled!
                if (propertyParam2)
                {
                    propertyParam0 = true;
                }
            }
            else if (actionId == actionP)
            {
                // Action is P (5)
                // If Q is true
                //  And if S is false, then throw, else do nothing
                if (propertyParam2)
                    if (!propertyParam0)
                    {
                        // Prepare the voilation message by sending execution trace and other info.
                        GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                        // Return false to say that property has been voilated.
                        return false;
                    }
            }
            else if (actionId == actionQ)
            {
                // Action is Q (15)   
                // Simply set Q is true, that is check is enabled!
                propertyParam2 = true;
            }

            return true;
        }

        #endregion
    }
}
