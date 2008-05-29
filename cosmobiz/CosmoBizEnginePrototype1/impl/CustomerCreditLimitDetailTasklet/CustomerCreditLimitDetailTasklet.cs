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
using CustomerCreditLimitDetailTasklet.Properties;

namespace CustomerCreditLimitDetailTasklet
{
    public class CustomerCreditLimitDetailTasklet : Tasklet, IRequestContributor
    {
        private const string XmlNamespace = "http://schemas.microsoft.com/mag/2007/03/RefCreateCustomer";
        private DatabaseCatalog databaseCatalog;
        private ICustomerCreditLimitDetailTaskletView view;
        private int? customerId;
        private string customerName;
        private string customerType;
        private StyleService styleService;
        private TaskletMode taskletMode;
        private string creditLimit;
		private ILoggingService loggingService;

        public CustomerCreditLimitDetailTasklet()
        {
            view = new CustomerCreditLimitDetailTaskletView();
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

        [InputParameter(InputParameterType.Optional)]
        public string CustomerType
        {
            get { return customerType; }
            set { customerType = value; }
        }

        [OutputParameter]
        public string CreditLimit
        {
            get { return creditLimit; }
            set
            {

                creditLimit = value;
                OnOutputChanged();
            }
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
            else
            {
                LoadCreditLimits(customerType, "");
            }

            view.ApplyStyles(StyleService);

            this.Container.Show((Control)view);
        }

        private void LoadCreditLimits(string customerType, string selectCreditLimit)
        {
            SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");

            if (database != null)
            {
                try
                {
                    List<string> listCreditLimits = new List<string>();
                    DbParameter[] parameters = new DbParameter[] { 
                    database.CreateParameter("Type", DbType.String, 100, customerType) 
                };

                    DbDataReader dbReader = database.ExecuteReader("SELECT Description FROM CREDITLIMITS WHERE Type = @Type", parameters);

                    view.ClearCreditLimit();
                    while (dbReader.Read() == true)
                    {
                        listCreditLimits.Add(dbReader.GetString(0));
                    }
                    view.SetCreditLimits(listCreditLimits, selectCreditLimit);
                }
                catch (Exception e)
                {
                    LoggingService.Submit(new ErrorLogEntry("Database error", e));
                    throw e;
                }
            }
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

                    DbDataReader dbReader = database.ExecuteReader("SELECT Type, CreditLimit FROM CUSTOMERS WHERE Id = @Id", parameters);

                    if (dbReader.Read() == true)
                    {
                        string type = dbReader.GetString(0);
                        string creditLimitDesc = dbReader.GetString(1);

                        LoadCreditLimits(type, creditLimitDesc);
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
                    string sql = "UPDATE CUSTOMERS SET CreditLimit = @creditLimit WHERE Id = @id";

                    SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");
                    List<DbParameter> listParameter = new List<DbParameter>();
                    listParameter.Add(database.CreateParameter("id", DbType.Int32, 4, customerId));
                    listParameter.Add(database.CreateParameter("creditLimit", DbType.String, 100, creditLimit));

                    database.ExecuteNonQuery(sql, listParameter.ToArray());
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
            writer.WriteStartElement("creditlimit", XmlNamespace);

            if (!String.IsNullOrEmpty(creditLimit))
                writer.WriteElementString("limit", XmlNamespace, creditLimit);

            writer.WriteEndElement();
        }

        private void SetTaskletMode()
        {
            string mode = Configuration.AppSettings.GetValue<string>("TaskletMode", "View");
            switch (mode)
            {
                case "Create":
                    if (CustomerType == null)
                        throw new Exception(Resources.MISSING_CUSTOMER_TYPE);
                    if (CustomerName == null)
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
