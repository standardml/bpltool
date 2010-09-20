using System.Runtime.Serialization;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{
    [DataContract]
    public struct PersistentSubscription
    {
        [DataMember]
        public string Address { get; set; }

        [DataMember]
        public string EventsContract { get; set; }

        [DataMember]
        public string EventOperation { get; set; }
        
    }
}
