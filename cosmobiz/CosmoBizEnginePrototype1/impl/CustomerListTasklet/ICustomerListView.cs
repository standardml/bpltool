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
using System.Windows.Forms;

namespace CustomerListTasklet
{
    interface ICustomerListView
    {
        CustomerListTasklet Presenter { set; }
        ContextMenu CustomerListContextMenu { get; }
        void ClearCustomerList();
        void SetCustomerList(List<List<string>> listCustomers);
        void ApplyStyles(StyleService styleService);
    }
}
