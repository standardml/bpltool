/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Windows.Forms;
using System.Data.Common;
using System.Data;

using Microsoft.Dynamics.Mobile.Framework.DataAccess;
using Services.StyleService;

using Microsoft.Dynamics.Mobile.Framework;
using Microsoft.Dynamics.Mobile.Framework.Services;
using Microsoft.Dynamics.Mobile.Framework.Entities;
using Microsoft.Dynamics.Mobile.Framework.Controls;
using CustomerDetailTasklet;
using CustomerDetailTasklet.Properties;



namespace CustomerDetailTasklet
{
    public class CustomerDetailTasklet : Tasklet, IRequestContributor
    {
        private const string XmlNamespace = "http://schemas.microsoft.com/mag/2007/03/RefCreateCustomer";
        private int? customerId;
        private string customerName;
        private string customerType;
        private ICustomerDetailTaskletView view;
        private DatabaseCatalog databaseCatalog;
        private StyleService styleService;
		private TaskletMode taskletMode;
		private ILoggingService loggingService;

        public CustomerDetailTasklet()
        {
            view = new CustomerDetailTaskletView();
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

        //The customer id is not used when creating a new customer in the wizard flow
        //therefore its optional here
        [InputParameter(InputParameterType.Optional)]
        public int? CustomerId
        {
            get { return customerId; }
            set { customerId = value;}
        }

        [OutputParameter]
        public string CustomerName
        {
            get { return customerName; }
            set
            {
                customerName = value;
                OnOutputChanged();
            }
        }

        [OutputParameter]
        public string CustomerType
        {
            get { return customerType; }
            set
            {
                customerType = value;
                OnOutputChanged();
            }
        }       

        protected override void OnStarted()
        {
            base.OnStarted();
            
            PopulateView((TaskletView)view);

            SetTaskletMode();
           
            LoadCustomerTypes();

			if (taskletMode != TaskletMode.Create)
			{
				LoadCustomer();
			}

            view.ApplyStyles(StyleService);

            this.Container.Show((Control)view);
        }       

        private void LoadCustomerTypes()
        {           
            try
            {
                SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");
                if (database != null)
                {
                    List<string> listCustomerType = new List<string>();
                    DbDataReader dbReader = database.ExecuteReader("SELECT Name FROM CUSTOMERTYPE");

                    while (dbReader.Read() == true)
                    {
                        listCustomerType.Add(dbReader.GetString(0));
                    }
                    view.SetCustomerTypes(listCustomerType);
                }

            }
            catch (Exception e)
            {
                LoggingService.Submit(new ErrorLogEntry("Database error", e));
                throw e;
            }
        }

        private void LoadCustomer()
        {
            try
            {
                SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");
                if (database != null)
                {
                    DbParameter[] parameters = new DbParameter[] { database.CreateParameter("Id", DbType.Int32, 4, customerId) };

                    DbDataReader dbReader = database.ExecuteReader("SELECT cus.Name, cus.Email, custype.Name FROM CUSTOMERS cus LEFT JOIN CUSTOMERTYPE custype ON custype.Id = cus.Type WHERE cus.Id = @Id", parameters);

                    if (dbReader.Read() == true)
                    {
                        view.CustomerName = dbReader.GetString(0);
                        if (dbReader.IsDBNull(1) == false)
                            view.CustomerEmail = dbReader.GetString(1);
                        if (dbReader.IsDBNull(2) == false)
                            view.CustomerTypeName = dbReader.GetString(2);
                    }
                }
            }
            catch (Exception e)
            {
                LoggingService.Submit(new ErrorLogEntry("Database error", e));
                throw e;
            }
        }

        public string GetCustomerTypeIdFromName(string name)
        {
            string result = "";

            try
            {
                SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");
                if (database != null)
                {
                    DbParameter[] parameter = new DbParameter[] { database.CreateParameter("name", DbType.String, 100, name) };

                    DbDataReader reader = database.ExecuteReader("SELECT Id FROM CUSTOMERTYPE WHERE Name = @name", parameter);

                    if (reader.Read() == true)
                        result = reader.GetString(0);
                }
            }
            catch (Exception e)
            {
                LoggingService.Submit(new ErrorLogEntry("Database error", e));
                throw e;
            }

            return result;
        }
        

        public override ValidationResult Validate()
        {
			if (taskletMode == TaskletMode.Create)
			{
				ValidationResult vr = new ValidationResult(false, Resources.CUSTOMER_ALLREADY_EXIST);

				try
				{
					SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");
					if (database != null)
					{
						DbParameter[] parameters = new DbParameter[] { 
                    database.CreateParameter("name", DbType.String, 100, view.CustomerName) 
                };

						int nCount = (int)database.ExecuteScalar("SELECT count(Id) FROM CUSTOMERS WHERE Name = @name", parameters);

						if (nCount == 0)
							vr = ValidationResult.ValidationPassed;
					}
				}
				catch (Exception e)
				{
					LoggingService.Submit(new ErrorLogEntry("Database error", e));
					throw e;
				}

				return vr;
			}
			else
				return ValidationResult.ValidationPassed;
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
                        string sql = "UPDATE CUSTOMERS set Email = @email WHERE Id = @id";

                        List<DbParameter> parameters = new List<DbParameter>();
                        parameters.Add(database.CreateParameter("id", customerId));
                        parameters.Add(database.CreateParameter("email", view.CustomerEmail));

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
			writer.WriteStartElement("customer", XmlNamespace);

			if (!String.IsNullOrEmpty(view.CustomerName))
				writer.WriteElementString("name", XmlNamespace, view.CustomerName);

			if (!String.IsNullOrEmpty(view.CustomerEmail))
				writer.WriteElementString("email", XmlNamespace, view.CustomerEmail);

			string customerTypeId = GetCustomerTypeIdFromName(view.CustomerTypeName);
			if (!String.IsNullOrEmpty(customerTypeId))
				writer.WriteElementString("type", XmlNamespace, customerTypeId);

			writer.WriteEndElement();
		}

		private void SetTaskletMode()
		{
			string mode = Configuration.AppSettings.GetValue<string>("TaskletMode", "View");
			switch (mode)
			{
				case "Create":
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
