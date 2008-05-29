/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
namespace CustomerContactDetailTasklet
{
    partial class CustomerContactDetailTaskletView
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
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CustomerContactDetailTaskletView));
			this.gradientPanel1 = new Microsoft.Dynamics.Mobile.Framework.Controls.ScrollableControlEx();
			this.txtPhone = new Microsoft.Dynamics.Mobile.Framework.Controls.TextBox();
			this.lblPhone = new Microsoft.Dynamics.Mobile.Framework.Controls.Label();
			this.txtContact = new Microsoft.Dynamics.Mobile.Framework.Controls.TextBox();
			this.lblContact = new Microsoft.Dynamics.Mobile.Framework.Controls.Label();
			this.gradientPanel1.SuspendLayout();
			this.SuspendLayout();
			// 
			// gradientPanel1
			// 
			this.gradientPanel1.AutoScroll = true;
			this.gradientPanel1.Controls.Add(this.txtPhone);
			this.gradientPanel1.Controls.Add(this.lblPhone);
			this.gradientPanel1.Controls.Add(this.txtContact);
			this.gradientPanel1.Controls.Add(this.lblContact);
			this.gradientPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.gradientPanel1.GradientEndColor = System.Drawing.Color.FromArgb(((int)(((byte)(198)))), ((int)(((byte)(229)))), ((int)(((byte)(249)))));
			this.gradientPanel1.GradientStartColor = System.Drawing.Color.FromArgb(((int)(((byte)(241)))), ((int)(((byte)(245)))), ((int)(((byte)(251)))));
			this.gradientPanel1.HScrollHeight = 15;
			this.gradientPanel1.Id = 0;
			this.gradientPanel1.Location = new System.Drawing.Point(0, 0);
			this.gradientPanel1.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.BackgroundMode.System;
			this.gradientPanel1.Name = "gradientPanel1";
			this.gradientPanel1.Size = new System.Drawing.Size(240, 268);
			this.gradientPanel1.TabIndex = 23;
			this.gradientPanel1.Transparent = false;
			this.gradientPanel1.VScrollWidth = 15;
			this.gradientPanel1.Click += new System.EventHandler(this.gradientPanel1_Click);
			// 
			// txtPhone
			// 
			this.txtPhone.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.txtPhone.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.txtPhone.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.txtPhone.Location = new System.Drawing.Point(5, 62);
			this.txtPhone.Name = "txtPhone";
			this.txtPhone.Size = new System.Drawing.Size(202, 21);
			this.txtPhone.TabIndex = 1;
			this.txtPhone.Click += new System.EventHandler(this.txtPhone_Click);
			// 
			// lblPhone
			// 
			this.lblPhone.AutoSize = false;
			this.lblPhone.AutoSizeMode = Microsoft.Dynamics.Mobile.Framework.Controls.AutoSizeMode.None;
			this.lblPhone.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.lblPhone.Id = 0;
			this.lblPhone.Location = new System.Drawing.Point(5, 44);
			this.lblPhone.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.LabelMode.Plain;
			this.lblPhone.Name = "lblPhone";
			this.lblPhone.Size = new System.Drawing.Size(188, 15);
			this.lblPhone.Text = "lblPhone";
			this.lblPhone.TextMargin = 0;
			this.lblPhone.Title = "";
			this.lblPhone.Tooltip = "";
			this.lblPhone.ToolTipActivation = Microsoft.Dynamics.Mobile.Framework.Controls.ToolTipActivationMode.None;
			this.lblPhone.Transparent = true;
			this.lblPhone.ParentChanged += new System.EventHandler(this.lblPhone_ParentChanged);
			// 
			// txtContact
			// 
			this.txtContact.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.txtContact.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
			this.txtContact.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.txtContact.Location = new System.Drawing.Point(5, 21);
			this.txtContact.Name = "txtContact";
			this.txtContact.Size = new System.Drawing.Size(202, 20);
			this.txtContact.TabIndex = 0;
			this.txtContact.TextChanged += new System.EventHandler(this.txtContact_TextChanged);
			// 
			// lblContact
			// 
			this.lblContact.AutoSize = false;
			this.lblContact.AutoSizeMode = Microsoft.Dynamics.Mobile.Framework.Controls.AutoSizeMode.None;
			this.lblContact.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.lblContact.Id = 0;
			this.lblContact.Location = new System.Drawing.Point(5, 3);
			this.lblContact.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.LabelMode.Plain;
			this.lblContact.Name = "lblContact";
			this.lblContact.Size = new System.Drawing.Size(188, 15);
			this.lblContact.Text = "lblContact";
			this.lblContact.TextMargin = 0;
			this.lblContact.Title = "";
			this.lblContact.Tooltip = "";
			this.lblContact.ToolTipActivation = Microsoft.Dynamics.Mobile.Framework.Controls.ToolTipActivationMode.None;
			this.lblContact.Transparent = true;
			// 
			// CustomerContactDetailTaskletView
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.Controls.Add(this.gradientPanel1);
			this.Image = ((System.Drawing.Image)(resources.GetObject("$this.Image")));
			this.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.Name = "CustomerContactDetailTaskletView";
			this.Size = new System.Drawing.Size(240, 268);
			this.gradientPanel1.ResumeLayout(false);
			this.ResumeLayout(false);

        }

        #endregion

        private Microsoft.Dynamics.Mobile.Framework.Controls.ScrollableControlEx gradientPanel1;
        private Microsoft.Dynamics.Mobile.Framework.Controls.Label lblPhone;
        private Microsoft.Dynamics.Mobile.Framework.Controls.Label lblContact;
        private Microsoft.Dynamics.Mobile.Framework.Controls.TextBox txtPhone;
        private Microsoft.Dynamics.Mobile.Framework.Controls.TextBox txtContact;



	}
}
