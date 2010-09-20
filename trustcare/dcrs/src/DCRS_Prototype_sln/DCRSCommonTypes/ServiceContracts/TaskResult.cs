using System.Runtime.Serialization;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{
    [DataContract]
    public class TaskResult
    {
        /// <summary>
        /// The execution status will indicate whether action execution is successful or not.
        /// It will be true if successful, other wise false.
        /// </summary>
        [DataMember]
        public bool Status;
        
        /// <summary>
        /// In case of failure to execute an action, Message property will 
        /// contain the reason for failure.
        /// </summary>
        [DataMember]
        public string Message;
    }
}
