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

namespace CustomerContactDetailTasklet
{
    interface ICustomerContactDetailTaskletView
    {
        CustomerContactDetailTasklet Presenter { set; }
        TaskletMode TaskletMode { set; get; }
        string ContactName { set; get;}
        string ContactPhone { set; get;}
        void ApplyStyles(StyleService styleService);
    }
}
