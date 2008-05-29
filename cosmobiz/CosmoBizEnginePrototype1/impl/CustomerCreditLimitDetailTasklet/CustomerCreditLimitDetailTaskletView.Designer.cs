/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
namespace CustomerCreditLimitDetailTasklet
{
    partial class CustomerCreditLimitDetailTaskletView
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CustomerCreditLimitDetailTaskletView));
			this.gradientPanel1 = new Microsoft.Dynamics.Mobile.Framework.Controls.ScrollableControlEx();
			this.comboCreditLimit = new Microsoft.Dynamics.Mobile.Framework.Controls.ComboBox();
			this.lblCreditLimit = new Microsoft.Dynamics.Mobile.Framework.Controls.Label();
			this.gradientPanel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// gradientPanel1
			// 
			this.gradientPanel1.AutoScroll = true;
			this.gradientPanel1.Controls.Add(this.comboCreditLimit);
			this.gradientPanel1.Controls.Add(this.lblCreditLimit);
			this.gradientPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.gradientPanel1.GradientEndColor = System.Drawing.Color.FromArgb(((int)(((byte)(198)))), ((int)(((byte)(229)))), ((int)(((byte)(249)))));
			this.gradientPanel1.GradientStartColor = System.Drawing.Color.FromArgb(((int)(((byte)(241)))), ((int)(((byte)(245)))), ((int)(((byte)(251)))));
			this.gradientPanel1.HScrollHeight = 15;
			this.gradientPanel1.Id = 0;
			this.gradientPanel1.Location = new System.Drawing.Point(0, 0);
			this.gradientPanel1.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.BackgroundMode.System;
			this.gradientPanel1.Name = "gradientPanel1";
			this.gradientPanel1.Size = new System.Drawing.Size(240, 268);
			this.gradientPanel1.TabIndex = 17;
			this.gradientPanel1.Transparent = false;
			this.gradientPanel1.VScrollWidth = 15;
			// 
			// comboCreditLimit
			// 
			this.comboCreditLimit.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.comboCreditLimit.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.comboCreditLimit.Location = new System.Drawing.Point(5, 21);
			this.comboCreditLimit.Name = "comboCreditLimit";
			this.comboCreditLimit.Size = new System.Drawing.Size(202, 20);
			this.comboCreditLimit.TabIndex = 0;
			this.comboCreditLimit.SelectedIndexChanged += new System.EventHandler(this.comboCreditLimit_SelectedIndexChanged);
			// 
			// lblCreditLimit
			// 
			this.lblCreditLimit.AutoSize = false;
			this.lblCreditLimit.AutoSizeMode = Microsoft.Dynamics.Mobile.Framework.Controls.AutoSizeMode.None;
			this.lblCreditLimit.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.lblCreditLimit.Id = 0;
			this.lblCreditLimit.Location = new System.Drawing.Point(5, 3);
			this.lblCreditLimit.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.LabelMode.Plain;
			this.lblCreditLimit.Name = "lblCreditLimit";
			this.lblCreditLimit.Size = new System.Drawing.Size(188, 15);
			this.lblCreditLimit.Text = "lblCreditLimit";
			this.lblCreditLimit.TextMargin = 0;
			this.lblCreditLimit.Title = "";
			this.lblCreditLimit.Tooltip = "";
			this.lblCreditLimit.ToolTipActivation = Microsoft.Dynamics.Mobile.Framework.Controls.ToolTipActivationMode.None;
			this.lblCreditLimit.Transparent = true;
			// 
			// CustomerCreditLimitDetailTaskletView
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.Controls.Add(this.gradientPanel1);
			this.Image = ((System.Drawing.Image)(resources.GetObject("$this.Image")));
			this.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.Name = "CustomerCreditLimitDetailTaskletView";
			this.Size = new System.Drawing.Size(240, 268);
			this.gradientPanel1.ResumeLayout(false);
			this.ResumeLayout(false);

        }

        #endregion

		private Microsoft.Dynamics.Mobile.Framework.Controls.ScrollableControlEx gradientPanel1;
        private Microsoft.Dynamics.Mobile.Framework.Controls.ComboBox comboCreditLimit;
		private Microsoft.Dynamics.Mobile.Framework.Controls.Label lblCreditLimit;



	}
}
