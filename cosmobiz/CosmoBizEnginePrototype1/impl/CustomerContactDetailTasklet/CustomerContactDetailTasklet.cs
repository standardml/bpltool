/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dynamics.Mobile.Framework;
using System.Windows.Forms;
using Microsoft.Dynamics.Mobile.Framework.DataAccess;
using System.Data.Common;
using System.Data;
using Services.StyleService;
using Microsoft.Dynamics.Mobile.Framework.Services;
using Microsoft.Dynamics.Mobile.Framework.Entities;
using Microsoft.Dynamics.Mobile.Framework.Controls;
using System.Xml;
using CustomerContactDetailTasklet.Properties;

namespace CustomerContactDetailTasklet
{
    public class CustomerContactDetailTasklet : Tasklet, IRequestContributor
    {
        private const string XmlNamespace = "http://schemas.microsoft.com/mag/2007/03/RefCreateCustomer";
        private DatabaseCatalog databaseCatalog;
        private ICustomerContactDetailTaskletView view;
        private int? customerId;
        private string customerName;
        private StyleService styleService;
        private TaskletMode taskletMode;
		private ILoggingService loggingService;

        public CustomerContactDetailTasklet()
        {
            view = new CustomerContactDetailTaskletView();
            view.Presenter = this;
        }

		[RolePadService]
        public DatabaseCatalog DatabaseCatalog
        {
            get { return databaseCatalog; }
            set { databaseCatalog = value; }
        }

		[RolePadService]
        public StyleService StyleService
        {
            get { return styleService; }
            set { styleService = value; }
        }

		[RolePadService]
		public ILoggingService LoggingService
		{
			get { return loggingService; }
			set { loggingService = value; }
		}

        [InputParameter(InputParameterType.Optional)]
        public int? CustomerId
        {
            get { return customerId; }
            set { customerId = value; }
        }

        [InputParameter(InputParameterType.Optional)]
        public string CustomerName
        {
            get { return customerName; }
            set { customerName = value; }
        }

        protected override void OnStarted()
        {
            base.OnStarted();

			this.PopulateView((ITaskletView)view);

            SetTaskletMode();

            if (taskletMode != TaskletMode.Create)
            {
                LoadCustomer();
            }

            view.ApplyStyles(StyleService);

            this.Container.Show((Control)view);
        }

        private void LoadCustomer()
        {
            try
            {
                SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");
                if (database != null)
                {
                    DbParameter[] parameters = new DbParameter[] { 
                    database.CreateParameter("Id", DbType.Int32, 4, customerId) 
                };

                    DbDataReader dbReader = database.ExecuteReader("SELECT ContactName, ContactPhone FROM CUSTOMERS WHERE Id = @Id", parameters);

                    if (dbReader.Read() == true)
                    {
                        if (dbReader.IsDBNull(0) == false)
                            view.ContactName = dbReader.GetString(0);
                        if (dbReader.IsDBNull(1) == false)
                            view.ContactPhone = dbReader.GetString(1);
                    }
                }
            }
            catch (Exception e)
            {
                LoggingService.Submit(new ErrorLogEntry("Database error", e));
                throw e;
            }
        }

        protected override void OnClosing(ExitResult exitResult)
        {
			base.OnClosing(exitResult);

            if (taskletMode == TaskletMode.Edit && exitResult == ExitResult.Ok)
            {
                try
                {
                    SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");
                    if (database != null)
                    {
                        string sql = "UPDATE CUSTOMERS set ContactName = @contactName, ContactPhone = @contactPhone WHERE Id = @id";

                        List<DbParameter> parameters = new List<DbParameter>();
                        parameters.Add(database.CreateParameter("id", customerId));
                        parameters.Add(database.CreateParameter("contactName", view.ContactName));
                        parameters.Add(database.CreateParameter("contactPhone", view.ContactPhone));

                        database.ExecuteNonQuery(sql, parameters.ToArray());
                    }
                }
                catch (Exception e)
                {
                    LoggingService.Submit(new ErrorLogEntry("Database error", e));
                    throw e;
                }
            }
        }
        
        public void CreateRequest(XmlWriter writer)
        {
            writer.WriteStartElement("contact", XmlNamespace);

            if (!String.IsNullOrEmpty(view.ContactName))
                writer.WriteElementString("name", XmlNamespace, view.ContactName);

            if (!String.IsNullOrEmpty(view.ContactPhone))
                writer.WriteElementString("tel", XmlNamespace, view.ContactPhone);

            writer.WriteEndElement();
        }
        
        private void SetTaskletMode()
        {
            string mode = Configuration.AppSettings.GetValue<string>("TaskletMode", "View");
            switch (mode)
            {
                case "Create":
                    if (customerName == null)
                        throw new Exception(Resources.MISSING_CUSTOMER_NAME);

                    taskletMode = TaskletMode.Create;
                    break;
                case "Edit":
                    if (CustomerId == null)
                        throw new Exception(Resources.MISSING_CUSTOMER_ID);

                    taskletMode = TaskletMode.Edit;
                    break;
                case "View":
                    if (CustomerId == null)
                        throw new Exception(Resources.MISSING_CUSTOMER_ID);

                    taskletMode = TaskletMode.View;
                    break;
            }
            view.TaskletMode = taskletMode;
        }
    }
}
