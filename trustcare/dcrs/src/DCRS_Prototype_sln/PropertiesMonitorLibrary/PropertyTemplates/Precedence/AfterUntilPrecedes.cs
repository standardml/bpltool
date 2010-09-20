using ITU.DK.DCRS.CommonTypes.PropertyMonitor;

namespace PropertiesRuntimeMonitor.PropertyTemplates.Precedence
{
    public class AfterUntilPrecedes : PropertyMonitorBase
    {
        private readonly short actionS;
        private readonly short actionP;
        private readonly short actionQ;
        private readonly short actionR;

        // Note: The main difference between afterUntilPrecedes and betweenPrecedes is that
        // in betweenPrecedes, you have the end state R where the check will be done, but in afterUntil
        // R may not happen, so the check will be done similar to AfterPrecedes, in addition a reset of  stats 
        // will also be done as afterUntilPrecedes can have multiple scope occurences where as in AfterPrecedes, there
        // is only one.
        // So AfterUntilPrecedes is a kind of in between 'BetweenPrecedes' and 'AfterPrecedes'


        // afterUntil (precedes (S, P), Q, R)
        // afterUntil (precedes (12, 5), 15, 6)

        // State Tracking Variables.
        // Tracking var for S
        private bool propertyParam0;
        // Tracking var for P
        private bool propertyParam1;
        // Tracking var for Q
        private bool propertyParam2;
        // Tracking var for R
        //private bool propertyParam3;



        #region Constructor.

        public AfterUntilPrecedes(short actionS, short actionP, short actionQ, short actionR)
        {
            this.actionS = actionS;
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
            if (actionId == actionS)
            {
                // Action is S (12)
                // Assign to true to S only 
                //  if the Q has occured before (that means after Q) 
                //  if P has not happed before
                // (That means we only count S in case if P is not true.)
                if ((propertyParam2) && (!propertyParam1))
                {
                    propertyParam0 = true;
                }
            }
            else if (actionId == actionP)
            {
                // Action is P (5)
                // If Q is true (that is after Q)
                //  And if S is false, then throw, else set p to true.
                if (propertyParam2)
                {
                    if (!propertyParam0)
                    {
                        // Prepare the voilation message by sending execution trace and other info.
                        GenerateVoilationMessage(processId, processInstanceId, actionId, principal, executionTrace);
                        // Return false to say that property has been voilated.
                        return false;
                    }

                    propertyParam1 = true;
                }
            }
            else if (actionId == actionQ)
            {
                // executedAction = Q
                // Assign to true to Q, to enable checking of property.
                propertyParam2 = true;
            }
            else if (actionId == actionR)
            {
                // executedAction = R

                // Make a check here as follows.
                // Finally set Q to false to disable checking until we get Q and also reset
                // P and S to make sure that we start on a new scope with clean values.
                if (propertyParam2)
                {
                    // Finally set P and Q to false to disable checking until we get Q.
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
