/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
using System;
using System.Collections.Generic;
using System.Text;
using Services.StyleService;

namespace CustomerDetailTasklet
{
    interface ICustomerDetailTaskletView
    {
        CustomerDetailTasklet Presenter { set; }
        TaskletMode TaskletMode { set; get; }
        void SetCustomerTypes(List<string> listCustomerType);
		string CustomerName { get; set;}
		string CustomerEmail { get; set; }
		string CustomerTypeName { get; set; }
        void ApplyStyles(StyleService styleService);    
    }
}
