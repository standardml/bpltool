using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ITU.DK.DCRS.CommonTypes.PropertyMonitor
{
    public abstract class PropertyMonitorBase
    {
        /// <summary>
        /// In case if the Monitor method returns false, this field will contain detailed 
        /// message, under which conditions the propoerty voilated.
        /// </summary>
        public string VoilationMessage { get; set; }

        /// <summary>
        /// 
        /// </summary>
        public string PropertyDescription { get; set; }

        /// <summary>
        /// 
        /// </summary>
        public PropertyFunctionTypeEnum PropertyFunction { get; set; }

        /// <summary>
        /// 
        /// </summary>
        public PropertyScopeTypeEnum PropertyScope { get; set; }

        /// <summary>
        /// This method will check if the property is voilated by the execution instance of workflow.
        /// </summary>
        /// <param name="processId"></param>
        /// <param name="processInstanceId"></param>
        /// <param name="actionId"></param>
        /// <param name="principal"></param>
        /// <param name="executionTrace"></param>
        /// <returns></returns>
        public abstract bool Monitor(int processId, int processInstanceId, short actionId, string principal, string executionTrace);

        protected void GenerateVoilationMessage(int processId, int processInstanceId, short actionId, string principal, string executionTrace)
        {

            VoilationMessage = string.Empty;
        }

    }
}
