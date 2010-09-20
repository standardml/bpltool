using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;


namespace ITU.DK.DCRS.WorkflowEngine.EventManagement
{
    [ServiceBehavior(InstanceContextMode = InstanceContextMode.PerCall)]
    public class RepositorySubscriptionService : SubscriptionManager<IRepositoryNotificationContract>,
                                                    IPersistentSubscriptionService
    {


    }
}
