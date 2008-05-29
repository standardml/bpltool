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
using CustomerCreditLimitDetailTasklet.Properties;
using Services.StyleService;
using Microsoft.Dynamics.Mobile.Framework.Controls;

namespace CustomerCreditLimitDetailTasklet
{
    public partial class CustomerCreditLimitDetailTaskletView : TaskletView, ICustomerCreditLimitDetailTaskletView
    {
        private CustomerCreditLimitDetailTasklet presenter;
        private TaskletMode activeTaskletMode;

        public CustomerCreditLimitDetailTasklet Presenter
        {
            set { presenter = value; }
        }

        public CustomerCreditLimitDetailTaskletView()
        {
            InitializeComponent();

            lblCreditLimit.Text = Resources.CREDIT_LIMIT_TEXT;
        }

        public TaskletMode TaskletMode
        {
            get { return activeTaskletMode; }
            set
            {
                activeTaskletMode = value;
                comboCreditLimit.Enabled = (value != TaskletMode.View);
            }
        }

        public void ClearCreditLimit()
        {
            comboCreditLimit.Items.Clear();
            comboCreditLimit.Update();
        }

        public void SetCreditLimits(List<string> listCreditlimit, string selectCreditLimit)
        {
            comboCreditLimit.BeginUpdate();

            foreach (string creditLimit in listCreditlimit)
            {
                comboCreditLimit.Items.Add(creditLimit);
            }

            if (comboCreditLimit.Items.Contains(selectCreditLimit) == true)
            {
                comboCreditLimit.SelectedIndex = comboCreditLimit.Items.IndexOf(selectCreditLimit);
            }

            comboCreditLimit.EndUpdate();
        }

        public void ApplyStyles(StyleService styleService)
        {
            styleService.ApplyMainControlStyle(this);
            styleService.ApplyHeaderStyle(this.Header);
            styleService.ApplyControlStyle(lblCreditLimit);
            styleService.ApplyControlStyle(comboCreditLimit);
        }

        private void comboCreditLimit_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (comboCreditLimit.SelectedIndex != -1)
                presenter.CreditLimit = comboCreditLimit.SelectedItem.ToString();
            else
                presenter.CreditLimit = null;

        }
    }
}
