using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Universality
{
    public class AfterUniversal : PropertyMonitorBase
    {

        // after( universal(P), Q)

        private readonly short actionP;
        private readonly short actionQ;

        // Tracking var for P
        private bool propertyParam0;
        // Tracking var for Q
        private bool propertyParam1;



        #region Constructor.

        public AfterUniversal(short actionP, short actionQ)
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
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal, string executionTrace)
        {
            // If we have not reached Q, just dont monitor the property!
            if (!propertyParam1) return true;
            

            if(actionId == actionQ)
            {
                propertyParam1 = true;
            }
            else
            {
                // Check if Q is reached and the current action is not P, then throw voilation.
                if (actionId != actionP)
                {
                    // Prepare the voilation message by sending execution trace and other info.
                    GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                    // Return false to say that property has been voilated.
                    return false;
   
                }
            }


            
            
            return true;
        }

        #endregion
    }
}
