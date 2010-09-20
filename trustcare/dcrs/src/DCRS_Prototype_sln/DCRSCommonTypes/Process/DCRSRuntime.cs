using System.Runtime.Serialization;

namespace ITU.DK.DCRS.CommonTypes.Process
{
    [DataContract]
    public class DCRSRuntime  
    {
        [DataMember]
        public DCRSState CurrentState { get; private set; }
        [DataMember]
        public int ProcessInstanceId { get; private set; }
        [DataMember]
        public string ExecutionTrace { get; private set; }

        
        public DCRSRuntime(DCRSState currentState, int processInstanceId, string executionTrace)
        {
            CurrentState = currentState;

            ExecutionTrace = executionTrace;
            
            ProcessInstanceId = processInstanceId;
        }

    }
}
