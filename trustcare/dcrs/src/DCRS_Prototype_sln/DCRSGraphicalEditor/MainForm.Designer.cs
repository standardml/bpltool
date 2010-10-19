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
            this.testToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.testToolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.addIncludeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.addExcludeToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.processPanel = new DCRSGraphicalEditor.DoubleBufferPanel();
            this.menuStrip1.SuspendLayout();
            this.cmProcessPanel.SuspendLayout();
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
            this.openToolStripMenuItem.Size = new System.Drawing.Size(152, 22);
            this.openToolStripMenuItem.Text = "New";
            this.openToolStripMenuItem.Click += new System.EventHandler(this.openToolStripMenuItem_Click);
            // 
            // openToolStripMenuItem1
            // 
            this.openToolStripMenuItem1.Name = "openToolStripMenuItem1";
            this.openToolStripMenuItem1.Size = new System.Drawing.Size(152, 22);
            this.openToolStripMenuItem1.Text = "Open";
            this.openToolStripMenuItem1.Click += new System.EventHandler(this.openToolStripMenuItem1_Click);
            // 
            // saveToolStripMenuItem
            // 
            this.saveToolStripMenuItem.Name = "saveToolStripMenuItem";
            this.saveToolStripMenuItem.Size = new System.Drawing.Size(152, 22);
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
            this.testToolStripMenuItem,
            this.testToolStripMenuItem1,
            this.addIncludeToolStripMenuItem,
            this.addExcludeToolStripMenuItem});
            this.cmProcessPanel.Name = "cmProcessPanel";
            this.cmProcessPanel.Size = new System.Drawing.Size(155, 92);
            this.cmProcessPanel.Opening += new System.ComponentModel.CancelEventHandler(this.cmProcessPanel_Opening);
            // 
            // testToolStripMenuItem
            // 
            this.testToolStripMenuItem.Name = "testToolStripMenuItem";
            this.testToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.testToolStripMenuItem.Text = "Add &Condition";
            this.testToolStripMenuItem.Click += new System.EventHandler(this.testToolStripMenuItem_Click);
            // 
            // testToolStripMenuItem1
            // 
            this.testToolStripMenuItem1.Name = "testToolStripMenuItem1";
            this.testToolStripMenuItem1.Size = new System.Drawing.Size(154, 22);
            this.testToolStripMenuItem1.Text = "Add &Response";
            this.testToolStripMenuItem1.Click += new System.EventHandler(this.testToolStripMenuItem1_Click);
            // 
            // addIncludeToolStripMenuItem
            // 
            this.addIncludeToolStripMenuItem.Name = "addIncludeToolStripMenuItem";
            this.addIncludeToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.addIncludeToolStripMenuItem.Text = "Add &Include";
            this.addIncludeToolStripMenuItem.Click += new System.EventHandler(this.addIncludeToolStripMenuItem_Click);
            // 
            // addExcludeToolStripMenuItem
            // 
            this.addExcludeToolStripMenuItem.Name = "addExcludeToolStripMenuItem";
            this.addExcludeToolStripMenuItem.Size = new System.Drawing.Size(154, 22);
            this.addExcludeToolStripMenuItem.Text = "Add &Exclude";
            this.addExcludeToolStripMenuItem.Click += new System.EventHandler(this.addExcludeToolStripMenuItem_Click);
            // 
            // processPanel
            // 
            this.processPanel.ContextMenuStrip = this.cmProcessPanel;
            this.processPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.processPanel.Location = new System.Drawing.Point(0, 24);
            this.processPanel.Name = "processPanel";
            this.processPanel.Size = new System.Drawing.Size(899, 502);
            this.processPanel.TabIndex = 1;
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
        private System.Windows.Forms.ToolStripMenuItem testToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem testToolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem addIncludeToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem addExcludeToolStripMenuItem;
    }
}

