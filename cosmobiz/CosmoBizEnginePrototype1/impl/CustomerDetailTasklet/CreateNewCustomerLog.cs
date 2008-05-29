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

namespace CustomerDetailTasklet
{
    class CreateNewCustomerLog : LogEntry
    {
        private string customerName;
        private string customerEmail;
        private string customerTypeName;

        public string CustomerTypeName
        {
            get { return customerTypeName; }
            set { customerTypeName = value; }
        }

        public string CustomerEmail
        {
            get { return customerEmail; }
            set { customerEmail = value; }
        }

        public string CustomerName
        {
            get { return customerName; }
            set { customerName = value; }
        }

        public CreateNewCustomerLog()
        {
            this.Message = Properties.Resources.CUSTOMER_CREATED;
        }
    }
}
