using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Universality
{
    public class BeforeUniversal : PropertyMonitorBase
    {

                
        // before (univeral (P), R)

        // State Tracking Variables.
        
        // Tracking var for P
        // Set this property to true intially
        private bool propertyParam0 = true;
        // Tracking var for R
        private bool propertyParam1;

        private readonly short actionP;
        private readonly short actionR;

        #region Constructor.


        public BeforeUniversal(short actionP, short actionR)
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
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal, string executionTrace)
        {
            // If we have reached R then we dont monitor the property any more.
            if (propertyParam1) return true;

            if(actionId == actionR)
            {
                // At R check whether P is absent in any of the previous action executions
                if(!propertyParam0)
                {
                    // Prepare the voilation message by sending execution trace and other info.
                    GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                    // Return false to say that property has been voilated.
                    return false;
                }

                // Set R to true
                propertyParam1 = true;
                // Since this event is R set, propertyParam0 to false.
                propertyParam0 = false;
            }
            else if(actionId != actionP)
            {
                // set propertyParam0 to false
                propertyParam0 = false;
            }

            
            return true;
        }

        #endregion
    }
}
