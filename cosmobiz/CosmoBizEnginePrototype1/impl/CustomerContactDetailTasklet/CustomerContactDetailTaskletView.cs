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
using CustomerContactDetailTasklet.Properties;
using Services.StyleService;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace CustomerContactDetailTasklet
{
    public partial class CustomerContactDetailTaskletView : TaskletView, ICustomerContactDetailTaskletView
    {
        private CustomerContactDetailTasklet presenter;
        private TaskletMode activeTaskletMode;

        public CustomerContactDetailTaskletView()
        {
            InitializeComponent();

            lblContact.Text = Resources.CONTACT_TEXT;
            lblPhone.Text = Resources.PHONE_TEXT;
        }

        public TaskletMode TaskletMode
        {
            get { return activeTaskletMode; }
            set
            {
                activeTaskletMode = value;
                txtPhone.Enabled = (value != TaskletMode.View);
                txtContact.Enabled = (value != TaskletMode.View);
            }
        }

        public string ContactName
        {
            get { return txtContact.Text; }
            set { txtContact.Text = value; }
        }

        public string ContactPhone
        {
            get { return txtPhone.Text; }
            set { txtPhone.Text = value; }
        }

        public CustomerContactDetailTasklet Presenter
        {
            set { presenter = value; }
        }

        public void ApplyStyles(StyleService styleService)
        {
            styleService.ApplyMainControlStyle(this);
            styleService.ApplyHeaderStyle(this.Header);
            styleService.ApplyControlStyle(txtContact);
            styleService.ApplyControlStyle(txtPhone);
            styleService.ApplyControlStyle(lblContact);
            styleService.ApplyControlStyle(lblPhone);
        }

        private void txtContact_TextChanged(object sender, EventArgs e)
        {

        }

        private void gradientPanel1_Click(object sender, EventArgs e)
        {

        }

        private void txtPhone_Click(object sender, EventArgs e)
        {

        }

        private void lblPhone_ParentChanged(object sender, EventArgs e)
        {

        }
    }
}
