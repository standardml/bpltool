namespace CFDemo
{
    partial class DetailedView
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        private System.Windows.Forms.MainMenu mainMenu1;

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
            this.mainMenu1 = new System.Windows.Forms.MainMenu();
            this.OK = new System.Windows.Forms.Button();
            this.NameLA = new System.Windows.Forms.Label();
            this.OwnerLA = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // OK
            // 
            this.OK.Location = new System.Drawing.Point(80, 196);
            this.OK.Name = "OK";
            this.OK.Size = new System.Drawing.Size(72, 20);
            this.OK.TabIndex = 0;
            this.OK.Text = "OK";
            this.OK.Click += new System.EventHandler(this.OK_Click);
            // 
            // NameLA
            // 
            this.NameLA.Location = new System.Drawing.Point(28, 14);
            this.NameLA.Name = "NameLA";
            this.NameLA.Size = new System.Drawing.Size(182, 20);
            this.NameLA.Text = "Name?";
            // 
            // OwnerLA
            // 
            this.OwnerLA.Location = new System.Drawing.Point(28, 49);
            this.OwnerLA.Name = "OwnerLA";
            this.OwnerLA.Size = new System.Drawing.Size(182, 20);
            this.OwnerLA.Text = "Owner?";
            // 
            // DetailedView
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.AutoScroll = true;
            this.ClientSize = new System.Drawing.Size(240, 268);
            this.Controls.Add(this.OwnerLA);
            this.Controls.Add(this.NameLA);
            this.Controls.Add(this.OK);
            this.Menu = this.mainMenu1;
            this.Name = "DetailedView";
            this.Text = "DetailedView";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button OK;
        private System.Windows.Forms.Label NameLA;
        private System.Windows.Forms.Label OwnerLA;
    }
}