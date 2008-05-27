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
using CustomerListTasklet.Properties;
using Microsoft.Dynamics.Mobile.Framework.Entities;
using Services.StyleService;
using Microsoft.Dynamics.Mobile.Framework.Services;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace CustomerListTasklet
{
    public class CustomerListTasklet : Tasklet
    {
        private ICustomerListView view;
        private int? customerId;
        private DatabaseCatalog databaseCatalog;
        private int nCustomerCount = 0;
        private StyleService styleService;
		private ILoggingService loggingService;

        public CustomerListTasklet()
        {
            view = new CustomerListTaskletView();
            view.Presenter = this;
        }

		[RolePadService]
		public ILoggingService LoggingService
		{
			get { return loggingService; }
			set { loggingService = value; }
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

        [OutputParameter]
        public int? SelectedCustomer
        {
            get { return customerId; }
            set 
            {
                customerId = value; 
                OnOutputChanged();
            }
        }

        protected override void OnStarted()
        {
            base.OnStarted();

			this.PopulateView((ITaskletView)this.view);

      //try
      //{
        ContextMenuManager.LoadMenu(view.CustomerListContextMenu, "SelectedCustomer");
      //}
      //catch (Exception e)
      //{        
      //}


            view.ApplyStyles(styleService);

            this.Container.Show((Control)view);
        }

        protected override void OnPopulatingMenu()
        {
            base.OnPopulatingMenu();

            MenuItem mnuCustomerCount = new MenuItem();
            mnuCustomerCount.Text = Resources.MENU_ITEM_TOTAL_CUSTOMERS_TEXT;
            mnuCustomerCount.Click += new EventHandler(CustomerCount_Click);
            MainMenuManager.Add(mnuCustomerCount, 50, new Group(null, 15, GroupType.Flat));
        }

        protected override void OnActivated()
        {
            base.OnActivated();

            LoadCustomer();
        }

        public void CustomerCount_Click(object sender, EventArgs e)
        {
            Container.Alert(String.Format(Resources.MSG_BOX_TOTAL_CUSTOMERS, nCustomerCount));
        }

        private void LoadCustomer()
        {
            List<List<string>> listCustomers = new List<List<string>>();

            try
            {
                SqlDatabase database = (SqlDatabase)databaseCatalog.GetDatabase("AppDatabase");

                DbDataReader reader = database.ExecuteReader("SELECT cus.Id, cus.Name, custype.Name FROM CUSTOMERS cus LEFT JOIN CUSTOMERTYPE custype ON custype.Id = cus.Type");

                nCustomerCount = 0;
                view.ClearCustomerList();
                while (reader.Read() == true)
                {
                    nCustomerCount++;
                    List<string> customer = new List<string>();

                    customer.Add(Convert.ToString(reader.GetInt32(0)));
                    customer.Add(reader.GetString(1));
                    customer.Add(reader.GetString(2));

                    listCustomers.Add(customer);
                }
                view.SetCustomerList(listCustomers);
            }
            catch (Exception e)
            {
                LoggingService.Submit(new ErrorLogEntry("Database error", e));
                throw e;
            }
        }
    }
}
