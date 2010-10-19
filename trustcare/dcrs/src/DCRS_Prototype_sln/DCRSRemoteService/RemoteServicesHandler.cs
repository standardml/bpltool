using System;
using System.Collections.Generic;
using System.Configuration;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.Exceptions;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ITU.DK.DCRS.CommonTypes.Process;

namespace ITU.DK.DCRS.RemoteServices
{
    public class RemoteServicesHandler
    {

        private static ServiceHost notificationServiceHost;

        private static readonly string NOTIFACATION_SERVICE_BASE_URL =
            ConfigurationManager.AppSettings.Get("NotificationServiceBaseURL");

        private static readonly string REPOSITORY_SUBSCRIPTION_SERVICE_URL =
            ConfigurationManager.AppSettings.Get("RepositorySubscriptionServiceURL");

        private static readonly string REPOSITORY_PROVIDER_SERVICE_URL =
    ConfigurationManager.AppSettings.Get("RepositoryProviderServiceURL");

        private static readonly string PROCESS_EXECUTION_SERVICE_URL =
ConfigurationManager.AppSettings.Get("ProcessExecutionServiceURL");



        #region Services.
        public static void HostNotificationService()
        {

            try
            {

                notificationServiceHost = new ServiceHost(
                    typeof(NotificationContract),
                    new[]
                        {
                            //new Uri("net.tcp://localhost:8305/NotificationService/NotificationContract")
                            new Uri(NOTIFACATION_SERVICE_BASE_URL),
                        });

                var wsHttpbinding = new WSHttpBinding(SecurityMode.Message, true)
                {
                    ReliableSession = { Enabled = true },
                    TransactionFlow = true
                };


                notificationServiceHost.AddServiceEndpoint(typeof(IRepositoryNotificationContract),
                                                           wsHttpbinding, "NotificationContract");

                notificationServiceHost.Open();

                //subscriptionServiceHost = new ServiceHost(typeof(RepositorySubscriptionService),
                //    new[]
                //        {
                //            new Uri(SUBSCRIPTION_SERVICE_BASE_URL)
                //        });

                //var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                //subscriptionServiceHost.AddServiceEndpoint(typeof(IPersistentSubscriptionService),
                //                        netTcpBinding,
                //                        "RepositorySubscriptionService");

                //subscriptionServiceHost.Open();

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format("Failed to host notification service at {0}/NotificationContract! Error message: {1}",
                                  NOTIFACATION_SERVICE_BASE_URL, exception.Message));
            }

        }

        public static void CloseNotificationService()
        {
            if (notificationServiceHost == null) return;

            notificationServiceHost.Close();

            notificationServiceHost = null;
        }
        
        #endregion


        #region Client providers.

        public static void HostSubscriptionServiceClient()
        {

            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IPersistentSubscriptionService>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            REPOSITORY_SUBSCRIPTION_SERVICE_URL));

                var proxy = factory.CreateChannel();

                proxy.PersistSubscribe(string.Format("{0}NotificationContract", NOTIFACATION_SERVICE_BASE_URL),
                                       typeof(IRepositoryNotificationContract).FullName,
                                       string.Empty);

                ((IClientChannel)proxy).Close();

                factory.Close();

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to create subscription service client at {0}/NotificationContract! Error message: {1}",
                        REPOSITORY_SUBSCRIPTION_SERVICE_URL, exception.Message));
            }

        } 

        public static List<int> GetProcessInstancesList(int processId)
        {
            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IRepositoryServiceContract>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            REPOSITORY_PROVIDER_SERVICE_URL));

                var proxy = factory.CreateChannel();

                var list = proxy.GetProcessInstanceList(processId);

                ((IClientChannel)proxy).Close();

                factory.Close();

                return list;

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call GetProcessInstanceList at {0}! Error message: {1}",
                        REPOSITORY_PROVIDER_SERVICE_URL, exception.Message));
            }


        }

        public static Dictionary<int,string > GetProcessList()
        {
            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IRepositoryServiceContract>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            REPOSITORY_PROVIDER_SERVICE_URL));

                var proxy = factory.CreateChannel();

                var dictionary = proxy.GetProcessList();

                ((IClientChannel)proxy).Close();

                factory.Close();

                return dictionary;

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call GetProcessInstanceList at {0}! Error message: {1}",
                        REPOSITORY_PROVIDER_SERVICE_URL, exception.Message));
            }


        }

        public static string GetProcess(int processId)
        {
            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IRepositoryServiceContract>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            REPOSITORY_PROVIDER_SERVICE_URL));

                var proxy = factory.CreateChannel();

                var processXml = proxy.GetProcess(processId);

                ((IClientChannel)proxy).Close();

                factory.Close();

                return processXml;

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call GetProcess with process Id:{0} from {1}! Error message: {2}",
                        processId, REPOSITORY_PROVIDER_SERVICE_URL, exception.Message));
            }


        }


        public static void ImportSpecification(DCRSProcess process)
        {
            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IRepositoryServiceContract>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            REPOSITORY_PROVIDER_SERVICE_URL));

                var proxy = factory.CreateChannel();

                proxy.ImportSpecification(DCRSProcess.Serialize(process));

                ((IClientChannel)proxy).Close();

                factory.Close();

                
                
            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to import a process to {1}! Error message: {2}",
                        REPOSITORY_PROVIDER_SERVICE_URL, exception.Message));
            }
        }


        public static string GetProcessInstance(int processId, int processInstanceId)
        {
            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IRepositoryServiceContract>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            REPOSITORY_PROVIDER_SERVICE_URL));

                var proxy = factory.CreateChannel();

                var processInstanceXml = proxy.GetProcessInstance(processId,processInstanceId);

                ((IClientChannel)proxy).Close();

                factory.Close();

                return processInstanceXml;

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call Get Process Instance with process Id:{0}, InstanceId:{1} from {2}! Error message: {3}",
                        processId, processInstanceId, REPOSITORY_PROVIDER_SERVICE_URL, exception.Message));
            }


        }



        #region Process Execution Service Functions.

        public static int StartNewInstance(int processId)
        {

            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IProcessExecutionServiceContract>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            PROCESS_EXECUTION_SERVICE_URL));

                var proxy = factory.CreateChannel();

                var processInstanceId = proxy.StartNewInstance(processId);

                ((IClientChannel)proxy).Close();

                factory.Close();

                return processInstanceId;

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call StartNewInstance() methods with process Id:{0} from {1}! Error message: {2}",
                        processId, PROCESS_EXECUTION_SERVICE_URL, exception.Message));
            }


        }


        public static TaskResult ExecuteAction(int processId, int processInstanceId, short action, string principal)
        {

            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory =
                    new ChannelFactory<IProcessExecutionServiceContract>(netTcpBinding,
                                                                        new EndpointAddress(
                                                                            PROCESS_EXECUTION_SERVICE_URL));

                var proxy = factory.CreateChannel();

                var actionResult = proxy.ExecuteAction(processId, processInstanceId, action, principal);

                ((IClientChannel)proxy).Close();

                factory.Close();

                return actionResult;

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call ExecuteAction() method for action: {0}, principal: {1} with process Id:{2}, processInstanceId: {3} from {4}! Error message: {5}",
                        action, principal, processId, processInstanceId, PROCESS_EXECUTION_SERVICE_URL,
                        exception.Message));
            }

        }

        #endregion





        #endregion
        
        

    }

}
