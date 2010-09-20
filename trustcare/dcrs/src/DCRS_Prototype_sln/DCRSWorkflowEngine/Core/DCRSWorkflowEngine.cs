using System;
using ITU.DK.DCRS.WorkflowEngine.DataAccess;
using ITU.DK.DCRS.WorkflowEngine.Services;

namespace ITU.DK.DCRS.WorkflowEngine.Core
{
    public class DCRSWorkflowEngine : IDisposable
    {

        #region Static Data.
        
        private static readonly DCRSWorkflowEngine WORKFLOW_ENGINE_INSTANCE = new DCRSWorkflowEngine();

        #endregion

        #region Instance Data.

        private static  XmlRepositoryProvider repositoryProvider;

        private readonly object lockerObject = new object();

        #endregion


        #region Private Constructor.

        private DCRSWorkflowEngine()
        {
            repositoryProvider = XmlRepositoryProvider.RepositoryProvider;

            // Host services.
            // Subscription Service.
            ServiceHostProvider.HostSubscriptionService();
            
            // Repository Service
            ServiceHostProvider.HostRepositoryService();

            // Process Execution Service
            ServiceHostProvider.HostProcessExecutionService();
        }


        #endregion

        #region Static Properties.

        public static DCRSWorkflowEngine WorkflowEngineInstance
        {
            get { return WORKFLOW_ENGINE_INSTANCE; }
        }


        public static IDataPresitanceProvider ProcessRepositoryProvider
        {
            get { return repositoryProvider; }
        }


        #endregion


        #region IDisposable Members and implementation of Disposable patteren.
        /// <summary>
        /// Performs application-defined tasks associated with freeing, releasing, or 
        /// resetting unmanaged resources
		/// </summary>
		public void Dispose ()
		{
            // This object is explicitly cleaned up, 
            // Requests that the system not call the finalizer for the specified object
            GC.SuppressFinalize(this);

			// Call Dispose method to clean.
            Dispose(true);
		}

		/// <summary>
		/// The method performs cleanup. This will be called from Finalize and Dispose.
        /// Dispose(bool disposing) executes in two distinct scenarios.
        /// If disposing equals true, the method has been called directly
        /// or indirectly by a user's code. Managed and unmanaged resources
        /// can be disposed.
        /// If disposing equals false, the method has been called by the 
        /// runtime from inside the finalizer and you should not reference 
        /// other objects. Only unmanaged resources can be disposed.
		/// </summary>
		/// <param name="disposing">true if object is being disposed explicitly, false if from Finalize 
		/// method called by garbage collector</param>
		private void Dispose ( bool disposing )
		{
            lock (lockerObject)
            {
                if (!disposing) return;
                // Close the hosted services.
                ServiceHostProvider.CloseSubscriptionService();

                ServiceHostProvider.CloseRepositoryService();

                ServiceHostProvider.CloseProcessExecutionService();
            }
		}

		/// <summary>
		/// Destructor
		/// </summary>
		~DCRSWorkflowEngine ()
		{
			Dispose ( false );
		}
		#endregion
    }
}
