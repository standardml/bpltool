/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using System.Drawing;

namespace Services.StyleService
{
    public class StyleService
    {
        private Color headerBackColor = Color.AntiqueWhite;
        private Color headerTextColor = Color.Aquamarine;
        private Color controlsBackColor = Color.Beige;
        private Color controlTextColor = Color.Black;
        private Color mainControlBackColor = Color.Blue; 
        private Color mainControlTextColor = Color.Brown;

        public Color HeaderBackColor
        {
            get { return headerBackColor; }
            set { headerBackColor = value; }
        }

        public Color HeaderTextColor
        {
            get { return headerTextColor; }
            set { headerTextColor = value; }
        }

        public Color MainControlBackColor
        {
            get { return mainControlBackColor; }
            set { mainControlBackColor = value; }
        }

        public Color MainControlTextColor
        {
            get { return mainControlTextColor; }
            set { mainControlTextColor = value; }
        }

        public Color ControlBackColor
        {
            get { return controlsBackColor; }
            set { controlsBackColor = value; }
        }

        public Color ControlTextColor
        {
            get { return controlTextColor; }
            set { controlTextColor = value; }
        }

        public void ApplyHeaderStyle(Control control)
        {
            if (headerBackColor != Color.Empty)
                control.BackColor = headerBackColor;
            if (headerTextColor != Color.Empty)
                control.ForeColor = headerTextColor;
        }

        public void ApplyMainControlStyle(Control control)
        {
            if (mainControlBackColor != Color.Empty)
                control.BackColor = mainControlBackColor;
            if (MainControlTextColor != Color.Empty)
                control.ForeColor = MainControlTextColor;
        }

        public void ApplyControlStyle(Control control)
        {
            if(controlsBackColor != Color.Empty)
                control.BackColor = controlsBackColor;
            if (controlTextColor != Color.Empty)
                control.ForeColor = ControlTextColor;
        }
    }
}
