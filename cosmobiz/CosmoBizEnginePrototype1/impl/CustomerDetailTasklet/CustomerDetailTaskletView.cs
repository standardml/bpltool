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
using CustomerDetailTasklet.Properties;
using Services.StyleService;
using Microsoft.Dynamics.Mobile.Framework.Controls;
using Microsoft.Dynamics.Mobile.Framework;

namespace CustomerDetailTasklet
{
    public partial class CustomerDetailTaskletView : TaskletView, ICustomerDetailTaskletView
    {
        private CustomerDetailTasklet presenter;
        private TaskletMode activeTaskletMode;

        public CustomerDetailTasklet Presenter
        {
            set { presenter = value; }
        }

        public CustomerDetailTaskletView()
        {
            InitializeComponent();

            lblEmail.Text = Resources.CUSTOMER_EMAIL_TEXT;
            lblName.Text = Resources.CUSTOMER_NAME_TEXT;
            lblType.Text = Resources.CUSTOMER_TYPE_TEXT;
        }

        public TaskletMode TaskletMode 
        {
			get { return activeTaskletMode; }
			set 
            { 
                activeTaskletMode = value;

				if (value == TaskletMode.Create)
                {
                    txtName.Enabled = true;
                    txtEmail.Enabled = true;
                    comboType.Enabled = true;                    
                }
                else if (value == TaskletMode.Edit)
                {
                    txtName.Enabled = false;
                    txtEmail.Enabled = true;
                    comboType.Enabled = false;
                }
                else 
                {
                    txtName.Enabled = false;
                    txtEmail.Enabled = false;
                    comboType.Enabled = false;
                }
            }
        }

        public string CustomerName
        {
            set { txtName.Text = value; }
            get { return txtName.Text; }
        }

        public string CustomerEmail
        {
            get { return txtEmail.Text; }
            set { txtEmail.Text = value; }
        }

        public string CustomerTypeName
        {
            get { return comboType.SelectedItem.ToString(); }
			set
			{
				if (comboType.Items.Contains(value) == true)
				{
					comboType.SelectedIndex = comboType.Items.IndexOf(value);
				}
			}
		}

        public void SetCustomerTypes(List<string> listCustomerType)
        {
            comboType.BeginUpdate();

            foreach (string customerType in listCustomerType)
            {
                comboType.Items.Add(customerType);
            }

            comboType.EndUpdate();
        }

        public void ApplyStyles(StyleService styleService)
        {
            styleService.ApplyMainControlStyle(this);
            styleService.ApplyHeaderStyle(this.Header);
            styleService.ApplyControlStyle(lblEmail);
            styleService.ApplyControlStyle(lblName);
            styleService.ApplyControlStyle(lblType);
            styleService.ApplyControlStyle(txtEmail);
            styleService.ApplyControlStyle(txtName);
            styleService.ApplyControlStyle(comboType);
        }

        private void comboType_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboType.SelectedIndex != -1)
            {
                presenter.CustomerType = presenter.GetCustomerTypeIdFromName(comboType.SelectedItem.ToString());
            }
            else
            {
                presenter.CustomerType = null;
            }

        }

        private void txtName_TextChanged(object sender, EventArgs e)
        {
            presenter.CustomerName = (txtName.Text.Trim().Length == 0 ? null : txtName.Text);
        }
    }
}
