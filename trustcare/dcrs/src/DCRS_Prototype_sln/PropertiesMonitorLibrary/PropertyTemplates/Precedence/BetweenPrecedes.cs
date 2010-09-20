using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Precedence
{
    public class BetweenPrecedes : PropertyMonitorBase
    {

        // between and will have many instances of scope instead of just one instance
        // like in case of before and after.

        // between (responds (P, S), Q, R)
        // between (responds (12, 5), 15, 6)

        // State Tracking Variables.
        // Tracking var for P
        private bool propertyParam0;
        // Tracking var for S
        private bool propertyParam1;
        // Tracking var for Q
        private bool propertyParam2;
        // Tracking var for R
        //private bool propertyParam3;


        private readonly short actionS;
        private readonly short actionP;
        private readonly short actionQ;
        private readonly short actionR;

        #region Constructor.

        public BetweenPrecedes(short actionS, short actionP, short actionQ, short actionR)
        {
            this.actionS = actionS;
            this.actionP = actionP;
            this.actionQ = actionQ;
            this.actionR = actionR;
        }

        #endregion


        #region Implementation of IPropertyMonitor

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
            // This is dynamic switch case generated based on property definition.
            if (actionId == actionS)
            {
                // executedAction = S
                // Assign to true to S only 
                //  if the Q has occured before and 
                //  if P has not happed before
                // (That means we only count S in case if P is not true.)
                if ((propertyParam2) && (!propertyParam1))
                {
                    propertyParam0 = true;
                }
            }
            else if (actionId == actionP)
            {
                // executedAction = P
                // Assign P to true if only if  Q is true.
                if (propertyParam2)
                {
                    propertyParam1 = true;
                }
            }
            else if (actionId == actionQ)
            {
                // executedAction = Q
                // Assign to true to Q, to enable checking of property.
                propertyParam2 = true;

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
                // 1. if check is enabled that is Q is true then
                //  2. throw voilation only if p is true and S is not true
                // Finally set Q to false to disable checking until we get Q and also reset
                // P and S to make sure that we start on a new scope with clean values.
                if (propertyParam2)
                {
                    if ((propertyParam1) && (!propertyParam0))
                    {
                        // Prepare the voilation message by sending execution trace and other info.
                        GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                        // Return false to say that property has been voilated.
                        return false;
                    }

                    // Finally set P, S and Q to false to disable checking until we get Q.
                    propertyParam2 = false;

                    propertyParam0 = false;

                    propertyParam1 = false;
                }
            }


            return true;
        }

        #endregion
    }
}
