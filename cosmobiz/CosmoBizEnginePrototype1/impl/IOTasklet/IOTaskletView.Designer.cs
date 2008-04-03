namespace IOTasklet
{
  partial class IOTaskletView
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
      this.label1 = new System.Windows.Forms.Label();
      this.tbOutput = new System.Windows.Forms.TextBox();
      this.tbInput = new System.Windows.Forms.TextBox();
      this.label2 = new System.Windows.Forms.Label();
      this.SuspendLayout();
      // 
      // label1
      // 
      this.label1.Location = new System.Drawing.Point(69, 162);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(114, 16);
      this.label1.Text = "This is my output:";
      // 
      // tbOutput
      // 
      this.tbOutput.Location = new System.Drawing.Point(68, 181);
      this.tbOutput.Name = "tbOutput";
      this.tbOutput.Size = new System.Drawing.Size(100, 21);
      this.tbOutput.TabIndex = 1;
      this.tbOutput.TextChanged += new System.EventHandler(this.tbOutput_TextChanged);
      // 
      // tbInput
      // 
      this.tbInput.Location = new System.Drawing.Point(67, 71);
      this.tbInput.Name = "tbInput";
      this.tbInput.Size = new System.Drawing.Size(100, 21);
      this.tbInput.TabIndex = 3;
      // 
      // label2
      // 
      this.label2.Location = new System.Drawing.Point(68, 52);
      this.label2.Name = "label2";
      this.label2.Size = new System.Drawing.Size(114, 16);
      this.label2.Text = "This is my input:";
      // 
      // Tasklet1View
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
      this.Controls.Add(this.tbInput);
      this.Controls.Add(this.label2);
      this.Controls.Add(this.tbOutput);
      this.Controls.Add(this.label1);
      this.Name = "Tasklet1View";
      this.Size = new System.Drawing.Size(240, 268);
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.TextBox tbOutput;
    private System.Windows.Forms.TextBox tbInput;
    private System.Windows.Forms.Label label2;

  }
}
