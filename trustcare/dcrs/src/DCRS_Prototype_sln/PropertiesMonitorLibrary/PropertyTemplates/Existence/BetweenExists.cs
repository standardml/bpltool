using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Existence
{
    public class BetweenExists : PropertyMonitorBase
    {
        private readonly short actionP;
        private readonly short actionQ;
        private readonly short actionR;


        // between (exists (P), Q, R)
        // between (exists (sign), pm, gm)  ::= between (exists (13), 42, 56)
        // Let's say Action Ids of P, Q, R are 13, 42, 56
        // then property definition will be 
        // between (exists (13), 42, 56)

        // State Tracking Variables.
        // Tracking var for P
        private bool propertyParam0;
        // Tracking var for Q
        private bool propertyParam1;
        // Tracking var for R
        //private bool propertyParam2;



        #region Constructor

        public BetweenExists(short actionP, short actionQ, short actionR)
        {
            this.actionP = actionP;
            this.actionQ = actionQ;
            this.actionR = actionR;
        }

        #endregion




        /// <summary>
        /// This method will check if the property is voilated by the execution instance of workflow.
        /// </summary>
        /// <param name="processId"></param>
        /// <param name="processInstanceId"></param>
        /// <param name="actionId"></param>
        /// <param name="principal"></param>
        /// <param name="executionTrace"></param>
        /// <returns></returns>
        public  override bool Monitor(int processId, int processInstanceId, short actionId, string principal, string executionTrace)
        {


            // This is dynamic switch case generated based on property definition.
            if (actionId == actionP)
            {
                // executedAction = P
                // Assign to true to P only if the Q has occured before else do nothing.
                if (propertyParam1)
                {
                    propertyParam0 = true;
                }
            }
            else if (actionId == actionQ)
            {
                // executedAction = Q
                // Assign to true to Q, to enable checking of property.
                propertyParam1 = true;

                // Here we dont need to set P to false, If there is an execution 
                // QXXXQ, then we still consider that we are in the scope of first Q until we 
                // get an R to turn off the scopr of Q.
                //// In order to start tracking the property set P to false.
                //propertyParam0 = false;
            }
            else if (actionId == actionR)
            {
                // executedAction = R
                // Make a check here as follows.
                // 1. if check is enabled that is Q is true
                // 2. then if P is not true, then throw voilation.
                // Finally set Q to false to disable checking until we get Q.
                if (propertyParam1)
                {
                    if (!propertyParam0)
                    {
                        // Prepare the voilation message by sending execution trace and other info.
                        GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                        // Return false to say that property has been voilated.
                        return false;
                    }

                    // Finally set P and Q to false to disable checking until we get Q.
                    propertyParam1 = false;

                    propertyParam0 = false;
                }
            }


            return true;
        }



        

        
    }
}
