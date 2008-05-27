/***************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.  
THIS CODE IS MADE AVAILABLE AS IS, WITHOUT WARRANTY OF ANY KIND. 
THE ENTIRE RISK OF THE USE OR THE RESULTS FROM THE USE OF THIS CODE 
REMAINS WITH THE USER
***************************************************************************/
namespace CustomerListTasklet
{
    partial class CustomerListTaskletView
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
			System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(CustomerListTaskletView));
			this.contextMenu = new System.Windows.Forms.ContextMenu();
			this.lvCustomers = new Microsoft.Dynamics.Mobile.Framework.Controls.ListView();
			this.colCustId = new System.Windows.Forms.ColumnHeader();
			this.colCustName = new System.Windows.Forms.ColumnHeader();
			this.colCustType = new System.Windows.Forms.ColumnHeader();
			this.SuspendLayout();
			// 
			// lvCustomers
			// 
			this.lvCustomers.Activation = System.Windows.Forms.ItemActivation.Standard;
			this.lvCustomers.AutoScroll = true;
			this.lvCustomers.AutoSort = false;
			this.lvCustomers.CheckBoxes = false;
			this.lvCustomers.Columns.Add(this.colCustId);
			this.lvCustomers.Columns.Add(this.colCustName);
			this.lvCustomers.Columns.Add(this.colCustType);
			this.lvCustomers.ContextMenu = this.contextMenu;
			this.lvCustomers.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lvCustomers.Font = new System.Drawing.Font("Tahoma", 8F, System.Drawing.FontStyle.Regular);
			this.lvCustomers.FullRowSelect = true;
			this.lvCustomers.GradientBackground = false;
			this.lvCustomers.GradientEndColor = System.Drawing.Color.FromArgb(((int)(((byte)(241)))), ((int)(((byte)(245)))), ((int)(((byte)(251)))));
			this.lvCustomers.GradientStartColor = System.Drawing.Color.FromArgb(((int)(((byte)(198)))), ((int)(((byte)(229)))), ((int)(((byte)(249)))));
			this.lvCustomers.GridLines = false;
			this.lvCustomers.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Clickable;
			this.lvCustomers.HScrollHeight = 15;
			this.lvCustomers.Id = 0;
			this.lvCustomers.Location = new System.Drawing.Point(0, 0);
			this.lvCustomers.Mode = Microsoft.Dynamics.Mobile.Framework.Controls.BackgroundMode.Gradient;
			this.lvCustomers.Name = "lvCustomers";
			this.lvCustomers.SelectedIndex = -1;
			this.lvCustomers.SelectionColor = System.Drawing.SystemColors.Highlight;
			this.lvCustomers.Size = new System.Drawing.Size(190, 218);
			this.lvCustomers.TabIndex = 3;
			this.lvCustomers.Transparent = false;
			this.lvCustomers.TransparentColor = System.Drawing.Color.Magenta;
			this.lvCustomers.VScrollWidth = 15;
			this.lvCustomers.ZebraColor = System.Drawing.Color.FromArgb(((int)(((byte)(234)))), ((int)(((byte)(234)))), ((int)(((byte)(234)))));
			this.lvCustomers.ZebraMode = true;
			this.lvCustomers.SelectedIndexChanged += new System.EventHandler(this.lvCustomers_SelectedIndexChanged);
			// 
			// colCustId
			// 
			this.colCustId.Text = "colCustId";
			this.colCustId.Width = 40;
			// 
			// colCustName
			// 
			this.colCustName.Text = "colCustName";
			this.colCustName.Width = 125;
			// 
			// colCustType
			// 
			this.colCustType.Text = "colCustType";
			this.colCustType.Width = 75;
			// 
			// CustomerListTaskletView
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
			this.BackColor = System.Drawing.Color.Gainsboro;
			this.Controls.Add(this.lvCustomers);
			this.Image = ((System.Drawing.Image)(resources.GetObject("$this.Image")));
			this.ImageTransparentColor = System.Drawing.Color.Magenta;
			this.Name = "CustomerListTaskletView";
			this.Size = new System.Drawing.Size(190, 218);
			this.ResumeLayout(false);

        }

        #endregion

		private System.Windows.Forms.ContextMenu contextMenu;
		private Microsoft.Dynamics.Mobile.Framework.Controls.ListView lvCustomers;
		private System.Windows.Forms.ColumnHeader colCustId;
		private System.Windows.Forms.ColumnHeader colCustName;
		private System.Windows.Forms.ColumnHeader colCustType;

    }
}
