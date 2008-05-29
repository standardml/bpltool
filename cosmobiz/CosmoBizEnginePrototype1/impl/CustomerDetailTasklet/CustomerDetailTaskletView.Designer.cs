/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
using CustomerDetailTasklet.Properties;
namespace CustomerDetailTasklet
{
    partial class CustomerDetailTaskletView
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
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CustomerDetailTaskletView));
			this.gradientPanel1 = new Microsoft.Dynamics.Mobile.Framework.Controls.ScrollableControlEx();
			this.comboType = new Microsoft.Dynamics.Mobile.Framework.Controls.ComboBox();
			this.lblType = new Microsoft.Dynamics.Mobile.Framework.Controls.Label();
			this.txtEmail = new Microsoft.Dynamics.Mobile.Framework.Controls.TextBox();
			this.lblEmail = new Microsoft.Dynamics.Mobile.Framework.Controls.Label();
			this.txtName = new Microsoft.Dynamics.Mobile.Framework.Controls.TextBox();
			this.lblName = new Microsoft.Dynamics.Mobile.Framework.Controls.Label();
			this.gradientPanel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// gradientPanel1
			// 
			this.gradientPanel1.AutoScroll = true;
			this.gradientPanel1.Controls.Add(this.comboType);
			this.gradientPanel1.Controls.Add(this.lblType);
			this.gradientPanel1.Controls.Add(this.txtEmail);
			this.gradientPanel1.Controls.Add(this.lblEmail);
			this.gradientPanel1.Controls.Add(this.txtName);
			this.gradientPanel1.Controls.Add(this.lblName);
			this.gradientPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.gradientPanel1.GradientEndColor = System.Drawing.Color.FromArgb(((int)(((byte)(198)))), ((int)(((byte)(229)))), ((int)(((byte)(249)))));
			this.gradientPanel1.GradientStartColor = System.Drawing.Color.FromArgb(((int)(((byte)(241)))), ((int)(((byte)(245)))), ((int)(((byte)(251)))));
			this.gradientPanel1.HScrollHeight = 15;
			this.gradientPanel1.Id = 0;
			this.gradientPanel1.Location = new System.Drawing.Point(0, 0);
			this.gradientPanel1.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.BackgroundMode.System;
			this.gradientPanel1.Name = "gradientPanel1";
			this.gradientPanel1.Size = new System.Drawing.Size(240, 268);
			this.gradientPanel1.TabIndex = 27;
			this.gradientPanel1.Transparent = false;
			this.gradientPanel1.VScrollWidth = 15;
			// 
			// comboType
			// 
			this.comboType.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.comboType.Enabled = false;
			this.comboType.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.comboType.Location = new System.Drawing.Point(5, 103);
			this.comboType.Name = "comboType";
			this.comboType.Size = new System.Drawing.Size(202, 20);
			this.comboType.TabIndex = 2;
			this.comboType.SelectedIndexChanged += new System.EventHandler(this.comboType_SelectedIndexChanged);
			// 
			// lblType
			// 
			this.lblType.AutoSize = false;
			this.lblType.AutoSizeMode = Microsoft.Dynamics.Mobile.Framework.Controls.AutoSizeMode.None;
			this.lblType.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.lblType.Id = 0;
			this.lblType.Location = new System.Drawing.Point(5, 85);
			this.lblType.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.LabelMode.Plain;
			this.lblType.Name = "lblType";
			this.lblType.Size = new System.Drawing.Size(188, 15);
			this.lblType.Text = "lblType";
			this.lblType.TextMargin = 0;
			this.lblType.Title = "";
			this.lblType.Tooltip = "";
			this.lblType.ToolTipActivation = Microsoft.Dynamics.Mobile.Framework.Controls.ToolTipActivationMode.None;
			this.lblType.Transparent = true;
			// 
			// txtEmail
			// 
			this.txtEmail.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.txtEmail.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.txtEmail.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.txtEmail.Location = new System.Drawing.Point(5, 62);
			this.txtEmail.Name = "txtEmail";
			this.txtEmail.Size = new System.Drawing.Size(202, 20);
			this.txtEmail.TabIndex = 1;
			// 
			// lblEmail
			// 
			this.lblEmail.AutoSize = false;
			this.lblEmail.AutoSizeMode = Microsoft.Dynamics.Mobile.Framework.Controls.AutoSizeMode.None;
			this.lblEmail.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.lblEmail.Id = 0;
			this.lblEmail.Location = new System.Drawing.Point(5, 44);
			this.lblEmail.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.LabelMode.Plain;
			this.lblEmail.Name = "lblEmail";
			this.lblEmail.Size = new System.Drawing.Size(188, 15);
			this.lblEmail.Text = "lblEmail";
			this.lblEmail.TextMargin = 0;
			this.lblEmail.Title = "";
			this.lblEmail.Tooltip = "";
			this.lblEmail.ToolTipActivation = Microsoft.Dynamics.Mobile.Framework.Controls.ToolTipActivationMode.None;
			this.lblEmail.Transparent = true;
			// 
			// txtName
			// 
			this.txtName.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.txtName.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.txtName.Enabled = false;
			this.txtName.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.txtName.Location = new System.Drawing.Point(5, 21);
			this.txtName.Name = "txtName";
			this.txtName.Size = new System.Drawing.Size(202, 20);
			this.txtName.TabIndex = 0;
			this.txtName.TextChanged += new System.EventHandler(this.txtName_TextChanged);
			// 
			// lblName
			// 
			this.lblName.AutoSize = false;
			this.lblName.AutoSizeMode = Microsoft.Dynamics.Mobile.Framework.Controls.AutoSizeMode.None;
			this.lblName.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.lblName.Id = 0;
			this.lblName.Location = new System.Drawing.Point(5, 3);
			this.lblName.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.LabelMode.Plain;
			this.lblName.Name = "lblName";
			this.lblName.Size = new System.Drawing.Size(188, 15);
			this.lblName.Text = "lblName";
			this.lblName.TextMargin = 0;
			this.lblName.Title = "";
			this.lblName.Tooltip = "";
			this.lblName.ToolTipActivation = Microsoft.Dynamics.Mobile.Framework.Controls.ToolTipActivationMode.None;
			this.lblName.Transparent = true;
			// 
			// CustomerDetailTaskletView
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.Controls.Add(this.gradientPanel1);
			this.Image = ((System.Drawing.Image)(resources.GetObject("$this.Image")));
			this.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.Name = "CustomerDetailTaskletView";
			this.Size = new System.Drawing.Size(240, 268);
			this.gradientPanel1.ResumeLayout(false);
			this.ResumeLayout(false);

        }

        #endregion

		private Microsoft.Dynamics.Mobile.Framework.Controls.ScrollableControlEx gradientPanel1;
        private Microsoft.Dynamics.Mobile.Framework.Controls.ComboBox comboType;
		private Microsoft.Dynamics.Mobile.Framework.Controls.Label lblType;
        private Microsoft.Dynamics.Mobile.Framework.Controls.TextBox txtEmail;
		private Microsoft.Dynamics.Mobile.Framework.Controls.Label lblEmail;
        private Microsoft.Dynamics.Mobile.Framework.Controls.TextBox txtName;
		private Microsoft.Dynamics.Mobile.Framework.Controls.Label lblName;



	}
}
