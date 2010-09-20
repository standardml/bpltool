using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Universality
{
    public class BetweenUniversal : PropertyMonitorBase
    {
               
        // between and will have many instances of scope instead of just one instance
        // like in case of before and after.

        // between (universal (P), Q, R)

        // State Tracking Variables.
        // Tracking var for P
        //private bool propertyParam0;
        // Tracking var for Q
        private bool propertyParam1;
        // Tracking var for R
        //private bool propertyParam2;


        private readonly short actionP;
        private readonly short actionQ;
        private readonly short actionR;

        #region Constructor.

        public BetweenUniversal(short actionP, short actionQ, short actionR)
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
        public override bool Monitor(int processId, int processInstanceId, short actionId, string principal, string executionTrace)
        {


            if(actionId == actionQ)
            {
                propertyParam1 = true;
            }
            else if(actionId == actionR)
            {
                // set Q to false, to Turn off property monitor.
                propertyParam1 = false;
            }
            else
            {
                // If Q is true and the current action is not P then throw voilation.
                if ((propertyParam1) && (actionId != actionP))
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
