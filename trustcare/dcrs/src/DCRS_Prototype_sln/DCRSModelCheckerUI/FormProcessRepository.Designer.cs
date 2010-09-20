namespace DCRSModelCheckerUI
{
    partial class FormProcessRepository
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
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.buttonOpenInstance = new System.Windows.Forms.Button();
            this.label2 = new System.Windows.Forms.Label();
            this.listBoxInstances = new System.Windows.Forms.ListBox();
            this.label1 = new System.Windows.Forms.Label();
            this.listBoxProcesses = new System.Windows.Forms.ListBox();
            this.buttonStartNewInstance = new System.Windows.Forms.Button();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.buttonStartNewInstance);
            this.groupBox1.Controls.Add(this.buttonOpenInstance);
            this.groupBox1.Controls.Add(this.label2);
            this.groupBox1.Controls.Add(this.listBoxInstances);
            this.groupBox1.Controls.Add(this.label1);
            this.groupBox1.Controls.Add(this.listBoxProcesses);
            this.groupBox1.Location = new System.Drawing.Point(12, 22);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(731, 396);
            this.groupBox1.TabIndex = 0;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "DCRS Process Repository";
            // 
            // buttonOpenInstance
            // 
            this.buttonOpenInstance.Location = new System.Drawing.Point(629, 60);
            this.buttonOpenInstance.Name = "buttonOpenInstance";
            this.buttonOpenInstance.Size = new System.Drawing.Size(96, 46);
            this.buttonOpenInstance.TabIndex = 4;
            this.buttonOpenInstance.Text = "Open Process Instance";
            this.buttonOpenInstance.UseVisualStyleBackColor = true;
            this.buttonOpenInstance.Click += new System.EventHandler(this.ButtonOpenInstanceClick);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Calibri", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(474, 31);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(147, 17);
            this.label2.TabIndex = 3;
            this.label2.Text = "DCRS Process Instances";
            // 
            // listBoxInstances
            // 
            this.listBoxInstances.FormattingEnabled = true;
            this.listBoxInstances.Location = new System.Drawing.Point(477, 60);
            this.listBoxInstances.Name = "listBoxInstances";
            this.listBoxInstances.Size = new System.Drawing.Size(129, 238);
            this.listBoxInstances.TabIndex = 2;
            this.listBoxInstances.DoubleClick += new System.EventHandler(this.ButtonOpenInstanceClick);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Calibri", 10F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(12, 31);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(172, 17);
            this.label1.TabIndex = 1;
            this.label1.Text = "DCRS Process Specifications";
            // 
            // listBoxProcesses
            // 
            this.listBoxProcesses.FormattingEnabled = true;
            this.listBoxProcesses.Location = new System.Drawing.Point(12, 60);
            this.listBoxProcesses.Name = "listBoxProcesses";
            this.listBoxProcesses.Size = new System.Drawing.Size(286, 238);
            this.listBoxProcesses.TabIndex = 0;
            this.listBoxProcesses.SelectedIndexChanged += new System.EventHandler(this.ListBoxProcessesSelectedIndexChanged);
            // 
            // buttonStartNewInstance
            // 
            this.buttonStartNewInstance.Location = new System.Drawing.Point(54, 317);
            this.buttonStartNewInstance.Name = "buttonStartNewInstance";
            this.buttonStartNewInstance.Size = new System.Drawing.Size(96, 46);
            this.buttonStartNewInstance.TabIndex = 5;
            this.buttonStartNewInstance.Text = "Start a New Instance";
            this.buttonStartNewInstance.UseVisualStyleBackColor = true;
            this.buttonStartNewInstance.Click += new System.EventHandler(this.buttonStartNewInstance_Click);
            // 
            // FormProcessRepository
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(846, 470);
            this.Controls.Add(this.groupBox1);
            this.Name = "FormProcessRepository";
            this.Text = "Process Repository";
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label1;
        internal System.Windows.Forms.ListBox listBoxProcesses;
        private System.Windows.Forms.Label label2;
        internal System.Windows.Forms.ListBox listBoxInstances;
        private System.Windows.Forms.Button buttonOpenInstance;
        private System.Windows.Forms.Button buttonStartNewInstance;
    }
}