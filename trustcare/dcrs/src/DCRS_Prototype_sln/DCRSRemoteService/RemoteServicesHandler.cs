using System;
using System.Collections.Generic;
using System.Configuration;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.Exceptions;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ITU.DK.DCRS.CommonTypes.Process;
using System.Runtime.Serialization;

namespace ITU.DK.DCRS.RemoteServices
{
	public class RemoteServicesHandler
	{
        private static readonly TimeSpan _defaultTimeout = new TimeSpan(0,0,10);
		private static ServiceHost _notificationServiceHost;
        
		private static readonly string _notificationServiceBaseUrl =
			ConfigurationManager.AppSettings.Get("NotificationServiceBaseURL");

		private static readonly string _RepositorySubscriptionServiceUrl =
			ConfigurationManager.AppSettings.Get("RepositorySubscriptionServiceURL");

		private static readonly string _RepositoryProviderServiceUrl =
        	ConfigurationManager.AppSettings.Get("RepositoryProviderServiceURL");

		private static readonly string _ProcessExecutionServiceUrl =
            ConfigurationManager.AppSettings.Get("ProcessExecutionServiceURL");

        // Create a ChannelFactory of the specified Contract to the address given in paramater
        // The SendTimout is setted to the _defaultTimeout value
        private static ChannelFactory<Contract> CreateChannelFactory<Contract>(string address)
        {
            var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };
            netTcpBinding.SendTimeout = _defaultTimeout;
            netTcpBinding.ReaderQuotas.MaxStringContentLength = int.MaxValue;
            return new ChannelFactory<Contract>(netTcpBinding,
                                                new EndpointAddress(address));
        }
        
        #region Services.
        public static void HostNotificationService()
        {
            try
            {
                _notificationServiceHost = new ServiceHost(
                       typeof(NotificationContract),
                       new[] {new Uri(_notificationServiceBaseUrl)} );

                var wsHttpbinding = new WSHttpBinding(SecurityMode.Message, true)
                {
                    ReliableSession = { Enabled = true },
                    TransactionFlow = true
                };
                
                _notificationServiceHost.AddServiceEndpoint(typeof(IRepositoryNotificationContract),
                                                            wsHttpbinding, "NotificationContract");
                _notificationServiceHost.Open();
            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format("Failed to host notification service at {0}/NotificationContract! Error message: {1}",
                                  _notificationServiceBaseUrl, exception.Message));
            }

        }

        public static void CloseNotificationService()
        {
            if (_notificationServiceHost == null) return;
            _notificationServiceHost.Close();
            _notificationServiceHost = null;
        }
        
        #endregion


        #region Client providers.


        public static void HostSubscriptionServiceClient()
        {
            try
            {
                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                var factory = CreateChannelFactory<IPersistentSubscriptionService>(_RepositorySubscriptionServiceUrl);
                var proxy = factory.CreateChannel();

                proxy.PersistSubscribe(string.Format("{0}NotificationContract", _notificationServiceBaseUrl),
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
                        _RepositorySubscriptionServiceUrl, exception.Message));
            }
        } 


        public static List<int> GetProcessInstancesList(int processId)
        {
            try
            {
                var factory = CreateChannelFactory<IRepositoryServiceContract>(_RepositoryProviderServiceUrl);
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
                        _RepositoryProviderServiceUrl, exception.Message));
            }
        }


        public static Dictionary<int,string > GetProcessList()
        {
            try
            {
                var factory = CreateChannelFactory<IRepositoryServiceContract>(_RepositoryProviderServiceUrl);
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
                        "Failed to call GetProcessList at {0}! Error message: {1}",
                        _RepositoryProviderServiceUrl, exception.Message));
            }
        }


        public static string GetProcess(int processId)
        {
            try
            {
                //var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                //netTcpBinding.ReaderQuotas.MaxStringContentLength = int.MaxValue;

                //var factory =
                //    new ChannelFactory<IRepositoryServiceContract>(netTcpBinding,
                //                                                        new EndpointAddress(
                  //                                                          REPOSITORY_PROVIDER_SERVICE_URL));

                var factory = CreateChannelFactory<IRepositoryServiceContract>(_RepositoryProviderServiceUrl);
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
                        processId, _RepositoryProviderServiceUrl, exception.Message));
            }
        }



        public static string NewProcess()
        {
            try
            {
                //var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                //netTcpBinding.ReaderQuotas.MaxStringContentLength = int.MaxValue;

                //var factory =
                //    new ChannelFactory<IRepositoryServiceContract>(netTcpBinding,
                //                                                        new EndpointAddress(
                //                                                          REPOSITORY_PROVIDER_SERVICE_URL));

                var factory = CreateChannelFactory<IRepositoryServiceContract>(_RepositoryProviderServiceUrl);
                var proxy = factory.CreateChannel();

                var processXml = proxy.NewProcess();

                ((IClientChannel)proxy).Close();
                factory.Close();

                return processXml;

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call NewProcess from {0}! Error message: {1}",
                        _RepositoryProviderServiceUrl, exception.Message));
            }
        }



        public static void ImportSpecification(DCRSProcess process)
        {
            try
            {
                var factory = CreateChannelFactory<IRepositoryServiceContract>(_RepositoryProviderServiceUrl);
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
                        _RepositoryProviderServiceUrl, exception.Message));
            }
        }
        

        public static string GetProcessInstance(int processId, int processInstanceId)
        {
            try
            {
                var factory = CreateChannelFactory<IRepositoryServiceContract>(_RepositoryProviderServiceUrl);
                var proxy = factory.CreateChannel();

                var processInstanceXml = proxy.GetProcessInstance(processId,processInstanceId);

                ((IClientChannel)proxy).Close();

                return processInstanceXml;
            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format(
                        "Failed to call Get Process Instance with process Id:{0}, InstanceId:{1} from {2}! Error message: {3}",
                        processId, processInstanceId, _RepositoryProviderServiceUrl, exception.Message));
            }
        }
        

        #region Process Execution Service Functions.

        public static int StartNewInstance(int processId)
        {
            try
            {
                var factory = CreateChannelFactory<IProcessExecutionServiceContract>(_ProcessExecutionServiceUrl);
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
                        processId, _ProcessExecutionServiceUrl, exception.Message));
            }
        }


        public static TaskResult ExecuteAction(int processId, int processInstanceId, short action, string principal)
        {
            try
            {
                var factory = CreateChannelFactory<IProcessExecutionServiceContract>(_ProcessExecutionServiceUrl);
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
                        action, principal, processId, processInstanceId, _ProcessExecutionServiceUrl,
                        exception.Message));
            }
        }

        #endregion

        #endregion
        
        

    }

}
