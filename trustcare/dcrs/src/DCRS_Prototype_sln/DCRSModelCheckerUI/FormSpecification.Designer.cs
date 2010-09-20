namespace DCRSModelCheckerUI
{
    partial class FormSpecification
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
            this.button1 = new System.Windows.Forms.Button();
            this.textBoxTrace = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(115, 340);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(152, 64);
            this.button1.TabIndex = 0;
            this.button1.Text = "button1";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // textBoxTrace
            // 
            this.textBoxTrace.Location = new System.Drawing.Point(12, 25);
            this.textBoxTrace.Multiline = true;
            this.textBoxTrace.Name = "textBoxTrace";
            this.textBoxTrace.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.textBoxTrace.Size = new System.Drawing.Size(490, 287);
            this.textBoxTrace.TabIndex = 1;
            // 
            // FormSpecification
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(564, 431);
            this.Controls.Add(this.textBoxTrace);
            this.Controls.Add(this.button1);
            this.Name = "FormSpecification";
            this.Text = "FormSpecification";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button button1;
        internal System.Windows.Forms.TextBox textBoxTrace;
    }
}