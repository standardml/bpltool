namespace DCRSGraphicalEditor
{
    partial class MainForm
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.saveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.processToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.storePlacementToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.storeProcessToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.cmProcessPanel = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.addConditionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addResponseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addIncludeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addExcludeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addNodeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.removeNodeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.processPanel = new DCRSGraphicalEditor.DoubleBufferPanel();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.clbRoles = new System.Windows.Forms.CheckedListBox();
            this.cbIncluded = new System.Windows.Forms.CheckBox();
            this.cbEnabled = new System.Windows.Forms.CheckBox();
            this.tbName = new System.Windows.Forms.TextBox();
            this.btnStoreActionDetails = new System.Windows.Forms.Button();
            this.removePrimitiveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.menuStrip1.SuspendLayout();
            this.cmProcessPanel.SuspendLayout();
            this.processPanel.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.processToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(899, 24);
            this.menuStrip1.TabIndex = 0;
            this.menuStrip1.Text = "menuStrip1";
            this.menuStrip1.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.menuStrip1_ItemClicked);
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openToolStripMenuItem,
            this.openToolStripMenuItem1,
            this.saveToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(35, 20);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // openToolStripMenuItem
            // 
            this.openToolStripMenuItem.Name = "openToolStripMenuItem";
            this.openToolStripMenuItem.Size = new System.Drawing.Size(111, 22);
            this.openToolStripMenuItem.Text = "New";
            this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
            // 
            // openToolStripMenuItem1
            // 
            this.openToolStripMenuItem1.Name = "openToolStripMenuItem1";
            this.openToolStripMenuItem1.Size = new System.Drawing.Size(111, 22);
            this.openToolStripMenuItem1.Text = "Open";
            this.openToolStripMenuItem1.Click += new System.EventHandler(this.openToolStripMenuItem1_Click);
            // 
            // saveToolStripMenuItem
            // 
            this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
            this.saveToolStripMenuItem.Size = new System.Drawing.Size(111, 22);
            this.saveToolStripMenuItem.Text = "Save";
            this.saveToolStripMenuItem.Click += new System.EventHandler(this.saveToolStripMenuItem_Click);
            // 
            // processToolStripMenuItem
            // 
            this.processToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.storePlacementToolStripMenuItem,
            this.storeProcessToolStripMenuItem});
            this.processToolStripMenuItem.Name = "processToolStripMenuItem";
            this.processToolStripMenuItem.Size = new System.Drawing.Size(56, 20);
            this.processToolStripMenuItem.Text = "Process";
            // 
            // storePlacementToolStripMenuItem
            // 
            this.storePlacementToolStripMenuItem.Name = "storePlacementToolStripMenuItem";
            this.storePlacementToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.storePlacementToolStripMenuItem.Text = "Store Placement";
            this.storePlacementToolStripMenuItem.Click += new System.EventHandler(this.storePlacementToolStripMenuItem_Click);
            // 
            // storeProcessToolStripMenuItem
            // 
            this.storeProcessToolStripMenuItem.Name = "storeProcessToolStripMenuItem";
            this.storeProcessToolStripMenuItem.Size = new System.Drawing.Size(163, 22);
            this.storeProcessToolStripMenuItem.Text = "Store Process";
            // 
            // cmProcessPanel
            // 
            this.cmProcessPanel.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.addConditionToolStripMenuItem,
            this.addResponseToolStripMenuItem,
            this.addIncludeToolStripMenuItem,
            this.addExcludeToolStripMenuItem,
            this.addNodeToolStripMenuItem,
            this.removeNodeToolStripMenuItem,
            this.removePrimitiveToolStripMenuItem});
            this.cmProcessPanel.Name = "cmProcessPanel";
            this.cmProcessPanel.Size = new System.Drawing.Size(168, 180);
            this.cmProcessPanel.Opening += new System.ComponentModel.CancelEventHandler(this.cmProcessPanel_Opening);
            // 
            // addConditionToolStripMenuItem
            // 
            this.addConditionToolStripMenuItem.Name = "addConditionToolStripMenuItem";
            this.addConditionToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.addConditionToolStripMenuItem.Text = "Add &Condition";
            this.addConditionToolStripMenuItem.Click += new System.EventHandler(this.testToolStripMenuItem_Click);
            // 
            // addResponseToolStripMenuItem
            // 
            this.addResponseToolStripMenuItem.Name = "addResponseToolStripMenuItem";
            this.addResponseToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.addResponseToolStripMenuItem.Text = "Add &Response";
            this.addResponseToolStripMenuItem.Click += new System.EventHandler(this.testToolStripMenuItem1_Click);
            // 
            // addIncludeToolStripMenuItem
            // 
            this.addIncludeToolStripMenuItem.Name = "addIncludeToolStripMenuItem";
            this.addIncludeToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.addIncludeToolStripMenuItem.Text = "Add &Include";
            this.addIncludeToolStripMenuItem.Click += new System.EventHandler(this.addIncludeToolStripMenuItem_Click);
            // 
            // addExcludeToolStripMenuItem
            // 
            this.addExcludeToolStripMenuItem.Name = "addExcludeToolStripMenuItem";
            this.addExcludeToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.addExcludeToolStripMenuItem.Text = "Add &Exclude";
            this.addExcludeToolStripMenuItem.Click += new System.EventHandler(this.addExcludeToolStripMenuItem_Click);
            // 
            // addNodeToolStripMenuItem
            // 
            this.addNodeToolStripMenuItem.Name = "addNodeToolStripMenuItem";
            this.addNodeToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.addNodeToolStripMenuItem.Text = "Add Node";
            this.addNodeToolStripMenuItem.Click += new System.EventHandler(this.addNodeToolStripMenuItem_Click);
            // 
            // removeNodeToolStripMenuItem
            // 
            this.removeNodeToolStripMenuItem.Name = "removeNodeToolStripMenuItem";
            this.removeNodeToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.removeNodeToolStripMenuItem.Text = "Remove Node";
            this.removeNodeToolStripMenuItem.Click += new System.EventHandler(this.removeNodeToolStripMenuItem_Click);
            // 
            // processPanel
            // 
            this.processPanel.ContextMenuStrip = this.cmProcessPanel;
            this.processPanel.Controls.Add(this.groupBox1);
            this.processPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.processPanel.Location = new System.Drawing.Point(0, 24);
            this.processPanel.Name = "processPanel";
            this.processPanel.Size = new System.Drawing.Size(899, 502);
            this.processPanel.TabIndex = 1;
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.clbRoles);
            this.groupBox1.Controls.Add(this.cbIncluded);
            this.groupBox1.Controls.Add(this.cbEnabled);
            this.groupBox1.Controls.Add(this.tbName);
            this.groupBox1.Controls.Add(this.btnStoreActionDetails);
            this.groupBox1.Location = new System.Drawing.Point(651, 0);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(248, 290);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Action Details";
            // 
            // clbRoles
            // 
            this.clbRoles.FormattingEnabled = true;
            this.clbRoles.Location = new System.Drawing.Point(7, 95);
            this.clbRoles.Name = "clbRoles";
            this.clbRoles.Size = new System.Drawing.Size(187, 139);
            this.clbRoles.TabIndex = 4;
            // 
            // cbIncluded
            // 
            this.cbIncluded.AutoSize = true;
            this.cbIncluded.Location = new System.Drawing.Point(7, 71);
            this.cbIncluded.Name = "cbIncluded";
            this.cbIncluded.Size = new System.Drawing.Size(67, 17);
            this.cbIncluded.TabIndex = 3;
            this.cbIncluded.Text = "Included";
            this.cbIncluded.UseVisualStyleBackColor = true;
            // 
            // cbEnabled
            // 
            this.cbEnabled.AutoSize = true;
            this.cbEnabled.Location = new System.Drawing.Point(7, 47);
            this.cbEnabled.Name = "cbEnabled";
            this.cbEnabled.Size = new System.Drawing.Size(65, 17);
            this.cbEnabled.TabIndex = 2;
            this.cbEnabled.Text = "Enabled";
            this.cbEnabled.UseVisualStyleBackColor = true;
            // 
            // tbName
            // 
            this.tbName.Location = new System.Drawing.Point(7, 20);
            this.tbName.Name = "tbName";
            this.tbName.Size = new System.Drawing.Size(187, 20);
            this.tbName.TabIndex = 1;
            // 
            // btnStoreActionDetails
            // 
            this.btnStoreActionDetails.Location = new System.Drawing.Point(7, 254);
            this.btnStoreActionDetails.Name = "btnStoreActionDetails";
            this.btnStoreActionDetails.Size = new System.Drawing.Size(75, 23);
            this.btnStoreActionDetails.TabIndex = 0;
            this.btnStoreActionDetails.Text = "Update";
            this.btnStoreActionDetails.UseVisualStyleBackColor = true;
            this.btnStoreActionDetails.Click += new System.EventHandler(this.btnStoreActionDetails_Click);
            // 
            // removePrimitiveToolStripMenuItem
            // 
            this.removePrimitiveToolStripMenuItem.Name = "removePrimitiveToolStripMenuItem";
            this.removePrimitiveToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.removePrimitiveToolStripMenuItem.Text = "Remove Primitive";
            this.removePrimitiveToolStripMenuItem.Click += new System.EventHandler(this.removePrimitiveToolStripMenuItem_Click);
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(899, 526);
            this.Controls.Add(this.processPanel);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "MainForm";
            this.Text = "Form1";
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.cmProcessPanel.ResumeLayout(false);
            this.processPanel.ResumeLayout(false);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem saveToolStripMenuItem;
        private DoubleBufferPanel processPanel;
        private System.Windows.Forms.ToolStripMenuItem processToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem storePlacementToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem storeProcessToolStripMenuItem;
        private System.Windows.Forms.ContextMenuStrip cmProcessPanel;
        private System.Windows.Forms.ToolStripMenuItem addConditionToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addResponseToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addIncludeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addExcludeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addNodeToolStripMenuItem;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.TextBox tbName;
        private System.Windows.Forms.Button btnStoreActionDetails;
        private System.Windows.Forms.CheckBox cbIncluded;
        private System.Windows.Forms.CheckBox cbEnabled;
        private System.Windows.Forms.CheckedListBox clbRoles;
        private System.Windows.Forms.ToolStripMenuItem removeNodeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem removePrimitiveToolStripMenuItem;
    }
}

