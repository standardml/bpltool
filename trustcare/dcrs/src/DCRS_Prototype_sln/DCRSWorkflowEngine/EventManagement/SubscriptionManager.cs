using System;
using System.Collections.Generic;
using System.Configuration;
using System.Linq;
using System.Reflection;
using System.ServiceModel;
using System.ServiceModel.Channels;
using System.Xml.Linq;
using System.Diagnostics;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace ITU.DK.DCRS.WorkflowEngine.EventManagement
{
    public abstract class SubscriptionManager<T> where T : class
    {
        private static readonly string xmlFilepath;
        //= @"C:\Work\RM\non_source\OtherSource\Spikes&Examples\WCF\WCFModels\PublishSubscribe\PersistentSubscriptionManager\Subscriptions.xml";

        static SubscriptionManager()
        {
            xmlFilepath = ConfigurationManager.AppSettings.Get("SubscriptionManagerFile");

            if (string.IsNullOrEmpty(xmlFilepath))
                throw new ApplicationException(
                    "Path of Subscriptions Xml file is null or empty! Check Configuration for SubscriptionManagerFile !");
        }

        #region Implementation of IPersistentSubscriptionService

        [OperationBehavior(TransactionScopeRequired = true)]
        public PersistentSubscription[] GetAllSubscribers()
        {
            //PublishSubscribeDataSet.PersistentSubscribersDataTable subscribers = new PublishSubscribeDataSet.PersistentSubscribersDataTable();
            //PersistentSubscribersTableAdapter adapter = new PersistentSubscribersTableAdapter();
            //subscribers = adapter.GetAllSubscribers();
            //return Convert(subscribers);

            // Xml persistant store Implementation
            XElement rootElement = XElement.Load(xmlFilepath);

            PersistentSubscription[] persistentSubscriptions =
                (rootElement.Elements("subscription").Select(
                    subElement1 => (new PersistentSubscription
                    {
                        // ReSharper disable PossibleNullReferenceException
                        Address = subElement1.Element("address").Value,
                        // ReSharper restore PossibleNullReferenceException
                        // ReSharper disable PossibleNullReferenceException
                        EventOperation = subElement1.Element("operation").Value,
                        // ReSharper restore PossibleNullReferenceException
                        // ReSharper disable PossibleNullReferenceException
                        EventsContract = subElement1.Element("contract").Value
                        // ReSharper restore PossibleNullReferenceException
                    }
                                   ))).ToArray();


            return persistentSubscriptions;
        }

        [OperationBehavior(TransactionScopeRequired = true)]
        public PersistentSubscription[] GetSubscribersToContract(string eventContract)
        {
            //PublishSubscribeDataSet.PersistentSubscribersDataTable subscribers = new PublishSubscribeDataSet.PersistentSubscribersDataTable();
            //PersistentSubscribersTableAdapter adapter = new PersistentSubscribersTableAdapter();
            //subscribers = adapter.GetSubscribersToContract(eventContract);
            //return Convert(subscribers);

            // Xml persistant store Implementation
            return GetSubscribersByFilter("contract", eventContract);
        }

        [OperationBehavior(TransactionScopeRequired = true)]
        public string[] GetSubscribersToContractEventType(string eventsContract, string eventOperation)
        {
            return GetSubscribersToContractEventOperation(eventsContract, eventOperation);
        }


        [OperationBehavior(TransactionScopeRequired = true)]
        public PersistentSubscription[] GetAllSubscribersFromAddress(string address)
        {
            //VerifyAddress(address);

            //PublishSubscribeDataSet.PersistentSubscribersDataTable subscribers = new PublishSubscribeDataSet.PersistentSubscribersDataTable();

            //PersistentSubscribersTableAdapter adapter = new PersistentSubscribersTableAdapter();
            //subscribers = adapter.GetSubscribersFromAddress(address);

            //return Convert(subscribers);

            // Xml persistant store Implementation
            return GetSubscribersByFilter("address", address);
        }


        [OperationBehavior(TransactionScopeRequired = true)]
        public void PersistUnsubscribe(string address, string eventsContract, string eventOperation)
        {
            VerifyAddress(address);

            if (String.IsNullOrEmpty(eventOperation) == false)
            {
                RemovePersistent(address, eventsContract, eventOperation);
            }
            else
            {
                string[] methods = GetOperations();

                //Action<string> removePersistent = delegate(string methodName)
                //                                  {
                //                                      RemovePersistent(address, eventsContract, methodName);
                //                                  };

                Action<string> removePersistent = methodName => RemovePersistent(address, eventsContract, methodName);


                Array.ForEach(methods, removePersistent);
            }
        }


        [OperationBehavior(TransactionScopeRequired = true)]
        public void PersistSubscribe(string address, string eventsContract, string eventOperation)
        {
            VerifyAddress(address);

            if (String.IsNullOrEmpty(eventOperation) == false)
            {
                AddPersistent(address, eventsContract, eventOperation);
            }
            else
            {
                string[] methods = GetOperations();
                //Action<string> addPersistent = delegate(string methodName)
                //{
                //    AddPersistent(address, eventsContract, methodName);
                //};

                Action<string> addPersistent = methodName => AddPersistent(address, eventsContract, methodName);
                Array.ForEach(methods, addPersistent);
            }
        }

        #endregion

        #region Static Helper methods.

        private static void VerifyAddress(string address)
        {
            if (address.StartsWith("http:") || address.StartsWith("https:"))
            {
                return;
            }
            if (address.StartsWith("net.tcp:"))
            {
                return;
            }
            if (address.StartsWith("net.pipe:"))
            {
                return;
            }
            if (address.StartsWith("net.msmq:"))
            {
                return;
            }
            throw new InvalidOperationException("Unsupported protocol specified");
        }

        private static Binding GetBindingFromAddress(string address)
        {
            if (address.StartsWith("http:") || address.StartsWith("https:"))
            {
                var binding = new WSHttpBinding(SecurityMode.Message, true)
                {
                    ReliableSession = { Enabled = true },
                    TransactionFlow = true
                };
                return binding;
            }
            if (address.StartsWith("net.tcp:"))
            {
                var binding = new NetTcpBinding(SecurityMode.Message, true)
                {
                    ReliableSession = { Enabled = true },
                    TransactionFlow = true
                };
                return binding;
            }
            if (address.StartsWith("net.pipe:"))
            {
                var binding = new NetNamedPipeBinding { TransactionFlow = true };
                return binding;
            }
            if (address.StartsWith("net.msmq:"))
            {
                var binding = new NetMsmqBinding { Security = { Mode = NetMsmqSecurityMode.None } };
                return binding;
            }
            Debug.Assert(false, "Unsupported protocol specified");

// ReSharper disable HeuristicUnreachableCode
            return null;
// ReSharper restore HeuristicUnreachableCode
        }

        private static string[] GetOperations()
        {
            MethodInfo[] methods =
                typeof(T).GetMethods(BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.Instance);
            var operations = new List<string>(methods.Length);

            Action<MethodInfo> add = delegate(MethodInfo method)
            {
                Debug.Assert(!operations.Contains(method.Name));
                operations.Add(method.Name);
            };
            Array.ForEach(methods, add);
            return operations.ToArray();
        }

        private static bool ContainsPersistent(string address, string eventsContract, string eventOperation)
        {
            string[] addresses = GetSubscribersToContractEventOperation(eventsContract, eventOperation);
            Predicate<string> exists = addressToMatch => addressToMatch == address;
            return Array.Exists(addresses, exists);
        }

        private static void AddPersistent(string address, string eventsContract, string eventOperation)
        {
            bool exists = ContainsPersistent(address, eventsContract, eventOperation);
            if (exists)
            {
                return;
            }

            //PersistentSubscribersTableAdapter adapter = new PersistentSubscribersTableAdapter();
            //adapter.Insert(address, eventOperation, eventsContract);

            // Xml persistant store Implementation
            XElement rootElement = XElement.Load(xmlFilepath);

            // Add a New Persistent subscription...
            var subscriptionElement = new XElement("subscription",
                                                   new XElement("address", address),
                                                   new XElement("operation", eventOperation),
                                                   new XElement("contract", eventsContract));
            rootElement.Add(subscriptionElement);

            rootElement.Save(xmlFilepath);
        }

        private static void RemovePersistent(string address, string eventsContract, string eventOperation)
        {
            //PersistentSubscribersTableAdapter adapter = new PersistentSubscribersTableAdapter();

            //PublishSubscribeDataSet.PersistentSubscribersDataTable subscribers = adapter.GetSubscribersByAddressContractOperation(address, eventsContract, eventOperation);
            //foreach (PublishSubscribeDataSet.PersistentSubscribersRow subscriber in subscribers)
            //{
            //    adapter.Delete(subscriber.Address, subscriber.Operation, subscriber.Contract, subscriber.ID);
            //}


            // Xml persistant store Implementation
            XElement rootElement = XElement.Load(xmlFilepath);

            IEnumerable<XElement> subscriptionElements =
                rootElement.Elements("subscription").Where(
                    subElement => ((subElement.Element("address").Value == address) &&
                                   (subElement.Element("operation").Value == eventOperation) &&
                                   (subElement.Element("contract").Value == eventsContract)));

            foreach (XElement subscriptionElement in subscriptionElements)
            {
                subscriptionElement.Remove();
            }

            rootElement.Save(xmlFilepath);
        }

        //static PersistentSubscription[] Convert(PublishSubscribeDataSet.PersistentSubscribersDataTable subscribers)
        //{
        //    Converter<PublishSubscribeDataSet.PersistentSubscribersRow, PersistentSubscription> converter;
        //    converter = delegate(PublishSubscribeDataSet.PersistentSubscribersRow row)
        //    {
        //        PersistentSubscription subscription = new PersistentSubscription();
        //        subscription.Address = row.Address;
        //        subscription.EventsContract = row.Contract;
        //        subscription.EventOperation = row.Operation;
        //        return subscription;
        //    };
        //    if (subscribers.Rows.Count == 0)
        //    {
        //        return new PersistentSubscription[] { };
        //    }
        //    return Collection.UnsafeToArray(subscribers.Rows, converter);
        //}

        internal static T[] GetPersistentList(string eventOperation)
        {
            //string[] addresses = GetSubscribersToContractEventOperation(typeof(T).ToString(), eventOperation);

            string[] addresses = GetSubscribersToContractEventOperation(typeof(T).FullName, eventOperation);

            var subscribers = new List<T>(addresses.Length);

            //foreach (string address in addresses)
            //{
            //    Binding binding = GetBindingFromAddress(address);
            //    T proxy = ChannelFactory<T>.CreateChannel(binding, new EndpointAddress(address));
            //    subscribers.Add(proxy);
            //}

            subscribers.AddRange(from address in addresses
                                 let binding = GetBindingFromAddress(address)
                                 select ChannelFactory<T>.CreateChannel(binding, new EndpointAddress(address)));


            return subscribers.ToArray();
        }

        private static string[] GetSubscribersToContractEventOperation(string eventsContract, string eventOperation)
        {
            //PublishSubscribeDataSet.PersistentSubscribersDataTable subscribers = new PublishSubscribeDataSet.PersistentSubscribersDataTable();
            //PersistentSubscribersTableAdapter adapter = new PersistentSubscribersTableAdapter();
            //subscribers = adapter.GetSubscribersToContractOperation(eventsContract, eventOperation);

            //List<string> list = new List<string>();
            //foreach (PublishSubscribeDataSet.PersistentSubscribersRow row in subscribers)
            //{
            //    list.Add(row.Address);
            //}
            //return list.ToArray();

            // Xml persistant store Implementation

            XElement rootElement = XElement.Load(xmlFilepath);

            string[] adresses = (from subElement in rootElement.Elements("subscription")
                                 where ((subElement.Element("operation").Value == eventOperation) &&
                                        (subElement.Element("contract").Value == eventsContract))
                                 select subElement.Element("address").Value).ToArray();

            return adresses;
        }

        private static PersistentSubscription[] GetSubscribersByFilter(string filterName, string filterValue)
        {
            // Xml persistant store Implementation
            XElement rootElement = XElement.Load(xmlFilepath);

            PersistentSubscription[] persistentSubscriptions =
                (rootElement.Elements("subscription").Where(
                    subElement => (subElement.Element(filterName).Value == filterValue)).Select(
                        subElement1 => (new PersistentSubscription
                        {
                            Address = subElement1.Element("address").Value,
                            EventOperation = subElement1.Element("operation").Value,
                            EventsContract = subElement1.Element("contract").Value
                        }
                                       ))).ToArray();

            return persistentSubscriptions;
        }

        #endregion
    }
}
