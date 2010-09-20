using System.ServiceModel;

namespace ITU.DK.DCRS.CommonTypes.ServiceContracts
{
    [ServiceContract]
    public interface IPersistentSubscriptionService
    {
        [OperationContract]
        [TransactionFlow(TransactionFlowOption.Allowed)]
        void PersistSubscribe(string address, string eventsContract, string eventOperation);

        [OperationContract]
        [TransactionFlow(TransactionFlowOption.Allowed)]
        void PersistUnsubscribe(string address, string eventsContract, string eventOperation);

        [OperationContract]
        [TransactionFlow(TransactionFlowOption.Allowed)]
        PersistentSubscription[] GetAllSubscribers();

        [OperationContract]
        [TransactionFlow(TransactionFlowOption.Allowed)]
        PersistentSubscription[] GetSubscribersToContract(string eventsContract);

        [OperationContract]
        [TransactionFlow(TransactionFlowOption.Allowed)]
        string[] GetSubscribersToContractEventType(string eventsContract, string eventOperation);

        [OperationContract]
        [TransactionFlow(TransactionFlowOption.Allowed)]
        PersistentSubscription[] GetAllSubscribersFromAddress(string address);
    }
}
