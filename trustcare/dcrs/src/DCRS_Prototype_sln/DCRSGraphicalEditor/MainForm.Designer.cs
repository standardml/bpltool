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
            this.simulationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.enableNodeOnlyViewToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.enableExecutionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.cmProcessPanel = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.addConditionToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addResponseToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addIncludeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addExcludeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addNodeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.removeNodeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.removePrimitiveToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tpProcessData = new System.Windows.Forms.TabPage();
            this.dgvPrincipals = new System.Windows.Forms.DataGridView();
            this.dgvRoles = new System.Windows.Forms.DataGridView();
            this.tbName_def = new System.Windows.Forms.TextBox();
            this.lblID = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label1 = new System.Windows.Forms.Label();
            this.tpProcessModel = new System.Windows.Forms.TabPage();
            this.tpExecution = new System.Windows.Forms.TabPage();
            this.executionPanel = new System.Windows.Forms.Panel();
            this.optionsToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.backgroundColorToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.whiteToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.whiteToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.processPanel = new DCRSGraphicalEditor.DoubleBufferPanel();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.clbRoles = new System.Windows.Forms.CheckedListBox();
            this.cbIncluded = new System.Windows.Forms.CheckBox();
            this.tbName = new System.Windows.Forms.TextBox();
            this.menuStrip1.SuspendLayout();
            this.cmProcessPanel.SuspendLayout();
            this.tabControl1.SuspendLayout();
            this.tpProcessData.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dgvPrincipals)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.dgvRoles)).BeginInit();
            this.tpProcessModel.SuspendLayout();
            this.tpExecution.SuspendLayout();
            this.processPanel.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.processToolStripMenuItem,
            this.simulationToolStripMenuItem,
            this.optionsToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(1016, 24);
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
            // simulationToolStripMenuItem
            // 
            this.simulationToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.enableNodeOnlyViewToolStripMenuItem,
            this.enableExecutionToolStripMenuItem});
            this.simulationToolStripMenuItem.Name = "simulationToolStripMenuItem";
            this.simulationToolStripMenuItem.Size = new System.Drawing.Size(67, 20);
            this.simulationToolStripMenuItem.Text = "Simulation";
            // 
            // enableNodeOnlyViewToolStripMenuItem
            // 
            this.enableNodeOnlyViewToolStripMenuItem.Name = "enableNodeOnlyViewToolStripMenuItem";
            this.enableNodeOnlyViewToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.enableNodeOnlyViewToolStripMenuItem.Text = "Enable Node Only View";
            this.enableNodeOnlyViewToolStripMenuItem.Click += new System.EventHandler(this.enableNodeOnlyViewToolStripMenuItem_Click);
            // 
            // enableExecutionToolStripMenuItem
            // 
            this.enableExecutionToolStripMenuItem.Name = "enableExecutionToolStripMenuItem";
            this.enableExecutionToolStripMenuItem.Size = new System.Drawing.Size(195, 22);
            this.enableExecutionToolStripMenuItem.Text = "Start Simulation";
            this.enableExecutionToolStripMenuItem.Click += new System.EventHandler(this.enableExecutionToolStripMenuItem_Click);
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
            this.cmProcessPanel.Size = new System.Drawing.Size(168, 158);
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
            this.addNodeToolStripMenuItem.Text = "&Add Event";
            this.addNodeToolStripMenuItem.Click += new System.EventHandler(this.addNodeToolStripMenuItem_Click);
            // 
            // removeNodeToolStripMenuItem
            // 
            this.removeNodeToolStripMenuItem.Name = "removeNodeToolStripMenuItem";
            this.removeNodeToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.removeNodeToolStripMenuItem.Text = "&Remove Event";
            this.removeNodeToolStripMenuItem.Click += new System.EventHandler(this.removeNodeToolStripMenuItem_Click);
            // 
            // removePrimitiveToolStripMenuItem
            // 
            this.removePrimitiveToolStripMenuItem.Name = "removePrimitiveToolStripMenuItem";
            this.removePrimitiveToolStripMenuItem.Size = new System.Drawing.Size(167, 22);
            this.removePrimitiveToolStripMenuItem.Text = "Remove &Primitive";
            this.removePrimitiveToolStripMenuItem.Click += new System.EventHandler(this.removePrimitiveToolStripMenuItem_Click);
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tpProcessData);
            this.tabControl1.Controls.Add(this.tpProcessModel);
            this.tabControl1.Controls.Add(this.tpExecution);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 24);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(1016, 717);
            this.tabControl1.TabIndex = 2;
            // 
            // tpProcessData
            // 
            this.tpProcessData.Controls.Add(this.dgvPrincipals);
            this.tpProcessData.Controls.Add(this.dgvRoles);
            this.tpProcessData.Controls.Add(this.tbName_def);
            this.tpProcessData.Controls.Add(this.lblID);
            this.tpProcessData.Controls.Add(this.label2);
            this.tpProcessData.Controls.Add(this.label1);
            this.tpProcessData.Location = new System.Drawing.Point(4, 22);
            this.tpProcessData.Name = "tpProcessData";
            this.tpProcessData.Size = new System.Drawing.Size(1008, 691);
            this.tpProcessData.TabIndex = 2;
            this.tpProcessData.Text = "Process Data";
            this.tpProcessData.UseVisualStyleBackColor = true;
            // 
            // dgvPrincipals
            // 
            this.dgvPrincipals.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.dgvPrincipals.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgvPrincipals.Location = new System.Drawing.Point(236, 73);
            this.dgvPrincipals.Name = "dgvPrincipals";
            this.dgvPrincipals.Size = new System.Drawing.Size(764, 587);
            this.dgvPrincipals.TabIndex = 7;
            this.dgvPrincipals.ColumnAdded += new System.Windows.Forms.DataGridViewColumnEventHandler(this.dgvPrincipals_ColumnAdded);
            this.dgvPrincipals.DataError += new System.Windows.Forms.DataGridViewDataErrorEventHandler(this.dgvPrincipals_DataError);
            // 
            // dgvRoles
            // 
            this.dgvRoles.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.dgvRoles.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dgvRoles.Location = new System.Drawing.Point(11, 73);
            this.dgvRoles.Name = "dgvRoles";
            this.dgvRoles.Size = new System.Drawing.Size(203, 587);
            this.dgvRoles.TabIndex = 6;
            this.dgvRoles.DataError += new System.Windows.Forms.DataGridViewDataErrorEventHandler(this.dgvRoles_DataError);
            // 
            // tbName_def
            // 
            this.tbName_def.Location = new System.Drawing.Point(63, 34);
            this.tbName_def.Name = "tbName_def";
            this.tbName_def.Size = new System.Drawing.Size(247, 20);
            this.tbName_def.TabIndex = 5;
            this.tbName_def.TextChanged += new System.EventHandler(this.tbName_def_TextChanged);
            // 
            // lblID
            // 
            this.lblID.AutoSize = true;
            this.lblID.Location = new System.Drawing.Point(60, 11);
            this.lblID.Name = "lblID";
            this.lblID.Size = new System.Drawing.Size(0, 13);
            this.lblID.TabIndex = 4;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(8, 34);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(38, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Name:";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(8, 11);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(21, 13);
            this.label1.TabIndex = 2;
            this.label1.Text = "ID:";
            // 
            // tpProcessModel
            // 
            this.tpProcessModel.Controls.Add(this.processPanel);
            this.tpProcessModel.Location = new System.Drawing.Point(4, 22);
            this.tpProcessModel.Name = "tpProcessModel";
            this.tpProcessModel.Padding = new System.Windows.Forms.Padding(3);
            this.tpProcessModel.Size = new System.Drawing.Size(1008, 691);
            this.tpProcessModel.TabIndex = 0;
            this.tpProcessModel.Text = "Process Model";
            this.tpProcessModel.UseVisualStyleBackColor = true;
            this.tpProcessModel.Enter += new System.EventHandler(this.tpProcessModel_Enter);
            // 
            // tpExecution
            // 
            this.tpExecution.Controls.Add(this.executionPanel);
            this.tpExecution.Location = new System.Drawing.Point(4, 22);
            this.tpExecution.Name = "tpExecution";
            this.tpExecution.Padding = new System.Windows.Forms.Padding(3);
            this.tpExecution.Size = new System.Drawing.Size(1008, 691);
            this.tpExecution.TabIndex = 1;
            this.tpExecution.Text = "Process Simulation";
            this.tpExecution.UseVisualStyleBackColor = true;
            // 
            // executionPanel
            // 
            this.executionPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.executionPanel.Location = new System.Drawing.Point(3, 3);
            this.executionPanel.Name = "executionPanel";
            this.executionPanel.Size = new System.Drawing.Size(1002, 685);
            this.executionPanel.TabIndex = 0;
            this.executionPanel.Paint += new System.Windows.Forms.PaintEventHandler(this.executionPanel_Paint);
            this.executionPanel.MouseUp += new System.Windows.Forms.MouseEventHandler(this.executionPanel_MouseUp);
            // 
            // optionsToolStripMenuItem
            // 
            this.optionsToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.backgroundColorToolStripMenuItem});
            this.optionsToolStripMenuItem.Name = "optionsToolStripMenuItem";
            this.optionsToolStripMenuItem.Size = new System.Drawing.Size(56, 20);
            this.optionsToolStripMenuItem.Text = "Options";
            // 
            // backgroundColorToolStripMenuItem
            // 
            this.backgroundColorToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.whiteToolStripMenuItem,
            this.whiteToolStripMenuItem1});
            this.backgroundColorToolStripMenuItem.Name = "backgroundColorToolStripMenuItem";
            this.backgroundColorToolStripMenuItem.Size = new System.Drawing.Size(169, 22);
            this.backgroundColorToolStripMenuItem.Text = "Background Color";
            // 
            // whiteToolStripMenuItem
            // 
            this.whiteToolStripMenuItem.Name = "whiteToolStripMenuItem";
            this.whiteToolStripMenuItem.Size = new System.Drawing.Size(120, 22);
            this.whiteToolStripMenuItem.Text = "Default";
            this.whiteToolStripMenuItem.Click += new System.EventHandler(this.whiteToolStripMenuItem_Click);
            // 
            // whiteToolStripMenuItem1
            // 
            this.whiteToolStripMenuItem1.Name = "whiteToolStripMenuItem1";
            this.whiteToolStripMenuItem1.Size = new System.Drawing.Size(120, 22);
            this.whiteToolStripMenuItem1.Text = "White";
            this.whiteToolStripMenuItem1.Click += new System.EventHandler(this.whiteToolStripMenuItem1_Click);
            // 
            // processPanel
            // 
            this.processPanel.ContextMenuStrip = this.cmProcessPanel;
            this.processPanel.Controls.Add(this.groupBox1);
            this.processPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.processPanel.Location = new System.Drawing.Point(3, 3);
            this.processPanel.Name = "processPanel";
            this.processPanel.Size = new System.Drawing.Size(1002, 685);
            this.processPanel.TabIndex = 2;
            // 
            // groupBox1
            // 
            this.groupBox1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.groupBox1.Controls.Add(this.clbRoles);
            this.groupBox1.Controls.Add(this.cbIncluded);
            this.groupBox1.Controls.Add(this.tbName);
            this.groupBox1.Location = new System.Drawing.Point(754, 0);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(248, 290);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Event Details";
            // 
            // clbRoles
            // 
            this.clbRoles.FormattingEnabled = true;
            this.clbRoles.Location = new System.Drawing.Point(7, 65);
            this.clbRoles.Name = "clbRoles";
            this.clbRoles.Size = new System.Drawing.Size(187, 214);
            this.clbRoles.TabIndex = 4;
            this.clbRoles.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.clbRoles_ItemCheck);
            // 
            // cbIncluded
            // 
            this.cbIncluded.AutoSize = true;
            this.cbIncluded.Location = new System.Drawing.Point(6, 46);
            this.cbIncluded.Name = "cbIncluded";
            this.cbIncluded.Size = new System.Drawing.Size(67, 17);
            this.cbIncluded.TabIndex = 3;
            this.cbIncluded.Text = "Included";
            this.cbIncluded.UseVisualStyleBackColor = true;
            this.cbIncluded.CheckedChanged += new System.EventHandler(this.cbIncluded_CheckedChanged);
            // 
            // tbName
            // 
            this.tbName.Location = new System.Drawing.Point(7, 20);
            this.tbName.Name = "tbName";
            this.tbName.Size = new System.Drawing.Size(187, 20);
            this.tbName.TabIndex = 1;
            this.tbName.TextChanged += new System.EventHandler(this.tbName_TextChanged);
            // 
            // MainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1016, 741);
            this.Controls.Add(this.tabControl1);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "MainForm";
            this.Text = "DCR Graphs Graphical Editor";
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.MainForm_FormClosed);
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.cmProcessPanel.ResumeLayout(false);
            this.tabControl1.ResumeLayout(false);
            this.tpProcessData.ResumeLayout(false);
            this.tpProcessData.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dgvPrincipals)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.dgvRoles)).EndInit();
            this.tpProcessModel.ResumeLayout(false);
            this.tpExecution.ResumeLayout(false);
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
        private System.Windows.Forms.ToolStripMenuItem processToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem storePlacementToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem storeProcessToolStripMenuItem;
        private System.Windows.Forms.ContextMenuStrip cmProcessPanel;
        private System.Windows.Forms.ToolStripMenuItem addConditionToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addResponseToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addIncludeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addExcludeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addNodeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem removeNodeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem removePrimitiveToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem simulationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem enableNodeOnlyViewToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem enableExecutionToolStripMenuItem;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tpProcessModel;
        private DoubleBufferPanel processPanel;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.CheckedListBox clbRoles;
        private System.Windows.Forms.CheckBox cbIncluded;
        private System.Windows.Forms.TextBox tbName;
        private System.Windows.Forms.TabPage tpExecution;
        private System.Windows.Forms.Panel executionPanel;
        private System.Windows.Forms.TabPage tpProcessData;
        private System.Windows.Forms.TextBox tbName_def;
        private System.Windows.Forms.Label lblID;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.DataGridViewTextBoxColumn ColumnRoles;
        private System.Windows.Forms.DataGridView dgvRoles;
        private System.Windows.Forms.DataGridView dgvPrincipals;
        private System.Windows.Forms.ToolStripMenuItem optionsToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem backgroundColorToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem whiteToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem whiteToolStripMenuItem1;
    }
}

