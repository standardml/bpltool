namespace HWTasklet
{
  partial class HWTaskletView
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
      this.SuspendLayout();
      // 
      // label1
      // 
      this.label1.ForeColor = System.Drawing.Color.Olive;
      this.label1.Location = new System.Drawing.Point(68, 120);
      this.label1.Name = "label1";
      this.label1.Size = new System.Drawing.Size(99, 16);
      this.label1.Text = "Hello World!";
      // 
      // HWTaskletView
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
      this.Controls.Add(this.label1);
      this.Name = "HWTaskletView";
      this.Size = new System.Drawing.Size(240, 268);
      this.ResumeLayout(false);

    }

    #endregion

    private System.Windows.Forms.Label label1;

  }
}
