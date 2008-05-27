/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Dynamics.Mobile.Framework.Configuration;
using System.Drawing;
using System.Reflection;
using Microsoft.Dynamics.Mobile.Framework.Runtime;

namespace Services.StyleService.Configuration
{
    public class StyleServiceFactory : CustomServiceFactory
    {
        public string HeaderBackColor
        {
            get { return this.Configuration.AppSettings["headerBackColor"]; }
        }

        public string HeaderTextColor
        {
			get { return this.Configuration.AppSettings["headerTextColor"]; }
        }

        public string MainControlBackColor
        {
			get { return this.Configuration.AppSettings["mainControlBackColor"]; }
        }

        public string MainControlTextColor
        {
			get { return this.Configuration.AppSettings["mainControlTextColor"]; }
        }

        public string ControlBackColor
        {
			get { return this.Configuration.AppSettings["controlBackColor"]; }
        }

        public string ControlTextColor
        {
			get { return this.Configuration.AppSettings["controlTextColor"]; }
        }

        public override object CreateService(Type serviceType)
        {
            StyleService styleService = new StyleService();
            if (HeaderBackColor != null)
                styleService.HeaderBackColor = GetColor(HeaderBackColor);
            if (HeaderTextColor != null)
                styleService.HeaderTextColor = GetColor(HeaderTextColor);
            if (MainControlBackColor != null)
                styleService.MainControlBackColor = GetColor(MainControlBackColor);
            if (MainControlTextColor != null)
                styleService.MainControlTextColor = GetColor(MainControlTextColor);
            if (ControlBackColor != null)
                styleService.ControlBackColor = GetColor(ControlBackColor);
            if (ControlTextColor != null)
                styleService.ControlTextColor = GetColor(ControlTextColor);

            return styleService;
        }

        private Color GetColor(string color)
        {
            try
            {
                if (color.Length == 0)
                    return Color.Empty;

                if (color.StartsWith("#") && color.Length == 7)
                {
                    int red = int.Parse(color.Substring(1, 2), System.Globalization.NumberStyles.HexNumber);
                    int green = int.Parse(color.Substring(3, 2), System.Globalization.NumberStyles.HexNumber);
                    int blue = int.Parse(color.Substring(5, 2), System.Globalization.NumberStyles.HexNumber);
                    
                    return Color.FromArgb(red, green, blue);
                }
                else
                {
                    PropertyInfo propertyInfo = typeof(Color).GetProperty(color);
                    if (propertyInfo == null)
                        propertyInfo = typeof(SystemColors).GetProperty(color);


                    return (Color)propertyInfo.GetValue(null, null);
                }
            }
            catch
            {
                throw new Exception(String.Format("Style Service Configuration Exception: color malformatted - {0}", color));
            }
        }
    }
}
