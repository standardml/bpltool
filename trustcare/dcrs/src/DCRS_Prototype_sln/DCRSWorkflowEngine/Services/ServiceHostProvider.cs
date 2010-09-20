using System;
using System.Configuration;
using System.ServiceModel;
using ITU.DK.DCRS.CommonTypes.Exceptions;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;
using ITU.DK.DCRS.WorkflowEngine.EventManagement;

namespace ITU.DK.DCRS.WorkflowEngine.Services
{
    public class ServiceHostProvider
    {

        private static ServiceHost subscriptionServiceHost;

        private static ServiceHost executionServiceHost;

        private static ServiceHost repositoryServiceHost;

        private static readonly string SUBSCRIPTION_SERVICE_BASE_URL = ConfigurationManager.AppSettings.Get("SubscriptionServiceBaseURL");

        private static readonly string REPOSITORY_SERVICE_BASE_URL = ConfigurationManager.AppSettings.Get("RepositoryServiceBaseURL");

        private static readonly string PROCESS_EXECUTION_SERVICE_BASE_URL = ConfigurationManager.AppSettings.Get("ProcessExecutionServiceBaseURL");

        public static void HostSubscriptionService()
        {


            try
            {
                subscriptionServiceHost = new ServiceHost(typeof(RepositorySubscriptionService),
                    new[]
                        {
                            new Uri(SUBSCRIPTION_SERVICE_BASE_URL)
                        });

                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                subscriptionServiceHost.AddServiceEndpoint(typeof(IPersistentSubscriptionService),
                                        netTcpBinding,
                                        "RepositorySubscriptionService");

                subscriptionServiceHost.Open();

                Console.WriteLine(
                    "Successfully hosted Repository Subscription service at: {0}/RepositorySubscriptionService",
                    SUBSCRIPTION_SERVICE_BASE_URL);

            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format("Failed to host subscription service at {0}/RepositorySubscriptionService! Error message: {1}",
                                  SUBSCRIPTION_SERVICE_BASE_URL, exception.Message));
            }

        }

        public static void HostRepositoryService()
        {


            try
            {
                repositoryServiceHost = new ServiceHost(typeof(ProcessRepositoryService),
                    new[]
                        {
                            new Uri(REPOSITORY_SERVICE_BASE_URL)
                        });

                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                repositoryServiceHost.AddServiceEndpoint(typeof(IRepositoryServiceContract),
                                        netTcpBinding,
                                        "ProcessRepositoryService");

                repositoryServiceHost.Open();

                Console.WriteLine(
                    "Successfully hosted Process Repository Service at: {0}/ProcessRepositoryService",
                    REPOSITORY_SERVICE_BASE_URL);


            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format("Failed to host Repository Service service at {0}/ProcessRepositoryService! Error message: {1}",
                                  REPOSITORY_SERVICE_BASE_URL, exception.Message));
            }

        }

        public static void HostProcessExecutionService()
        {


            try
            {
                executionServiceHost = new ServiceHost(typeof(ProcessExecutionService),
                    new[]
                        {
                            new Uri(PROCESS_EXECUTION_SERVICE_BASE_URL)
                        });

                var netTcpBinding = new NetTcpBinding(SecurityMode.Transport, true) { TransactionFlow = true };

                executionServiceHost.AddServiceEndpoint(typeof(IProcessExecutionServiceContract),
                                        netTcpBinding,
                                        "ProcessExecutionService");

                executionServiceHost.Open();

                Console.WriteLine(
                    "Successfully hosted Process Execution Service at: {0}/ProcessExecutionService",
                    PROCESS_EXECUTION_SERVICE_BASE_URL);


            }
            catch (Exception exception)
            {
                throw new DCRSWorkflowException(
                    string.Format("Failed to host Process Execution Service  at {0}/ProcessExecutionService! Error message: {1}",
                                  PROCESS_EXECUTION_SERVICE_BASE_URL, exception.Message));
            }

        }

        public static void CloseRepositoryService()
        {
            if (repositoryServiceHost == null) return;

            repositoryServiceHost.Close();

            repositoryServiceHost = null;
        }

        public static void CloseProcessExecutionService()
        {
            if (executionServiceHost == null) return;

            executionServiceHost.Close();

            executionServiceHost = null;
        }

        public static void CloseSubscriptionService()
        {
            if (subscriptionServiceHost == null) return;

            subscriptionServiceHost.Close();

            subscriptionServiceHost = null;
        }

    }

}
