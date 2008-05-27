/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using CustomerListTasklet.Properties;
using Microsoft.Dynamics.Mobile.Framework.Entities;
using Services.StyleService;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace CustomerListTasklet
{
    public partial class CustomerListTaskletView : TaskletView, ICustomerListView
    {
        private CustomerListTasklet presenter;

        public CustomerListTaskletView()
        {
            InitializeComponent();

            lvCustomers.Columns[0].Text = Resources.LISTTEXT_CUSTOMERID;
            lvCustomers.Columns[1].Text = Resources.LISTTEXT_CUSTOMERNAME;
            lvCustomers.Columns[2].Text = Resources.LISTTEXT_CUSTOMERTYPE;
        }

        public CustomerListTasklet Presenter
        {
            set { presenter = value; }
        }

        public ContextMenu CustomerListContextMenu
        {
            get { return contextMenu; }
        }

        private void lvCustomers_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (lvCustomers.SelectedIndices.Count == 1)
                presenter.SelectedCustomer = Convert.ToInt32(lvCustomers.Items[lvCustomers.SelectedIndices[0]].Tag);
            else
                presenter.SelectedCustomer = null;
        }

        public void ClearCustomerList()
        {
            lvCustomers.Items.Clear();
            lvCustomers.Update();
        }

        public void SetCustomerList(List<List<string>> listCustomers)
        {
            foreach (List<string> customer in listCustomers)
            {
                if (customer.Count == 3)
                {
                    ListViewItem lvItem = new ListViewItem(new string[] { customer[0], customer[1], customer[2] });
                    lvItem.Tag = customer[0];

                    lvCustomers.Items.Add(lvItem);
                }
            }
        }

        public void ApplyStyles(StyleService styleService)
        {
            styleService.ApplyMainControlStyle(this);
            styleService.ApplyHeaderStyle(this.Header);
            styleService.ApplyControlStyle(lvCustomers);
        }
    }
}
