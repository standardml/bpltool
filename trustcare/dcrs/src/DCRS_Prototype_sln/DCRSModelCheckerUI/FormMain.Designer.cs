namespace DCRSModelCheckerUI
{
    partial class FormMain
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
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openDCRSSpecToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.openSpecificationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.importSpecificationToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.hostServicesToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.processRepositoryToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.loadProcessToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.loadInstanceToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.windowToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.menuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.processRepositoryToolStripMenuItem,
            this.windowToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.MdiWindowListItem = this.fileToolStripMenuItem;
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(827, 24);
            this.menuStrip1.TabIndex = 1;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.openDCRSSpecToolStripMenuItem,
            this.openSpecificationToolStripMenuItem,
            this.importSpecificationToolStripMenuItem,
            this.hostServicesToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // openDCRSSpecToolStripMenuItem
            // 
            this.openDCRSSpecToolStripMenuItem.Name = "openDCRSSpecToolStripMenuItem";
            this.openDCRSSpecToolStripMenuItem.Size = new System.Drawing.Size(181, 22);
            this.openDCRSSpecToolStripMenuItem.Text = "&New Specification";
            this.openDCRSSpecToolStripMenuItem.Click += new System.EventHandler(this.openDCRSSpecToolStripMenuItem_Click);
            // 
            // openSpecificationToolStripMenuItem
            // 
            this.openSpecificationToolStripMenuItem.Name = "openSpecificationToolStripMenuItem";
            this.openSpecificationToolStripMenuItem.Size = new System.Drawing.Size(181, 22);
            this.openSpecificationToolStripMenuItem.Text = "Open Specification";
            // 
            // importSpecificationToolStripMenuItem
            // 
            this.importSpecificationToolStripMenuItem.Name = "importSpecificationToolStripMenuItem";
            this.importSpecificationToolStripMenuItem.Size = new System.Drawing.Size(181, 22);
            this.importSpecificationToolStripMenuItem.Text = "Import Specification";
            // 
            // hostServicesToolStripMenuItem
            // 
            this.hostServicesToolStripMenuItem.Name = "hostServicesToolStripMenuItem";
            this.hostServicesToolStripMenuItem.Size = new System.Drawing.Size(181, 22);
            this.hostServicesToolStripMenuItem.Text = "Host Services";
            this.hostServicesToolStripMenuItem.Click += new System.EventHandler(this.hostServicesToolStripMenuItem_Click);
            // 
            // processRepositoryToolStripMenuItem
            // 
            this.processRepositoryToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.loadProcessToolStripMenuItem,
            this.loadInstanceToolStripMenuItem});
            this.processRepositoryToolStripMenuItem.Name = "processRepositoryToolStripMenuItem";
            this.processRepositoryToolStripMenuItem.Size = new System.Drawing.Size(75, 20);
            this.processRepositoryToolStripMenuItem.Text = "Repository";
            // 
            // loadProcessToolStripMenuItem
            // 
            this.loadProcessToolStripMenuItem.Name = "loadProcessToolStripMenuItem";
            this.loadProcessToolStripMenuItem.Size = new System.Drawing.Size(147, 22);
            this.loadProcessToolStripMenuItem.Text = "Process List";
            this.loadProcessToolStripMenuItem.Click += new System.EventHandler(this.ProcessListStripMenuItem_Click);
            // 
            // loadInstanceToolStripMenuItem
            // 
            this.loadInstanceToolStripMenuItem.Name = "loadInstanceToolStripMenuItem";
            this.loadInstanceToolStripMenuItem.Size = new System.Drawing.Size(147, 22);
            this.loadInstanceToolStripMenuItem.Text = "Load Instance";
            // 
            // windowToolStripMenuItem
            // 
            this.windowToolStripMenuItem.Name = "windowToolStripMenuItem";
            this.windowToolStripMenuItem.Size = new System.Drawing.Size(63, 20);
            this.windowToolStripMenuItem.Text = "&Window";
            // 
            // FormMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(827, 472);
            this.Controls.Add(this.menuStrip1);
            this.IsMdiContainer = true;
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "FormMain";
            this.Text = "DCRS Model Checker";
            this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openDCRSSpecToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem openSpecificationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem importSpecificationToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem hostServicesToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem processRepositoryToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem loadProcessToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem loadInstanceToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem windowToolStripMenuItem;
    }
}

