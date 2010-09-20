namespace WindowsApplication
{
  partial class Form1
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
        System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Form1));
        this.button1 = new System.Windows.Forms.Button();
        this.label1 = new System.Windows.Forms.Label();
        this.panel1 = new System.Windows.Forms.Panel();
        this.gViewer = new Microsoft.Glee.GraphViewerGdi.GViewer();
        this.splitter1 = new System.Windows.Forms.Splitter();
        this.propertyGrid1 = new System.Windows.Forms.PropertyGrid();
        this.recalculateLayoutButton = new System.Windows.Forms.Button();
        this.button2 = new System.Windows.Forms.Button();
        this.panel1.SuspendLayout();
        this.SuspendLayout();
        // 
        // button1
        // 
        this.button1.Location = new System.Drawing.Point(1, 1);
        this.button1.Name = "button1";
        this.button1.Size = new System.Drawing.Size(267, 23);
        this.button1.TabIndex = 2;
        this.button1.Text = "Create a graph, layout it, and display in the viewer";
        this.button1.UseVisualStyleBackColor = true;
        this.button1.Click += new System.EventHandler(this.button1_Click);
        // 
        // label1
        // 
        this.label1.AutoSize = true;
        this.label1.ForeColor = System.Drawing.SystemColors.Desktop;
        this.label1.Location = new System.Drawing.Point(287, 7);
        this.label1.Name = "label1";
        this.label1.Size = new System.Drawing.Size(106, 13);
        this.label1.TabIndex = 3;
        this.label1.Text = "No object is selected";
        // 
        // panel1
        // 
        this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.panel1.Controls.Add(this.gViewer);
        this.panel1.Controls.Add(this.splitter1);
        this.panel1.Controls.Add(this.propertyGrid1);
        this.panel1.Location = new System.Drawing.Point(1, 29);
        this.panel1.Margin = new System.Windows.Forms.Padding(2);
        this.panel1.Name = "panel1";
        this.panel1.Size = new System.Drawing.Size(996, 547);
        this.panel1.TabIndex = 4;
        // 
        // gViewer
        // 
        this.gViewer.AsyncLayout = false;
        this.gViewer.AutoScroll = true;
        this.gViewer.BackwardEnabled = false;
        this.gViewer.Dock = System.Windows.Forms.DockStyle.Fill;
        this.gViewer.ForwardEnabled = false;
        this.gViewer.Graph = null;
        this.gViewer.Location = new System.Drawing.Point(0, 0);
        this.gViewer.MouseHitDistance = 0.05;
        this.gViewer.Name = "gViewer";
        this.gViewer.NavigationVisible = true;
        this.gViewer.PanButtonPressed = false;
        this.gViewer.SaveButtonVisible = true;
        this.gViewer.Size = new System.Drawing.Size(698, 547);
        this.gViewer.TabIndex = 3;
        this.gViewer.ZoomF = 1;
        this.gViewer.ZoomFraction = 0.5;
        this.gViewer.ZoomWindowThreshold = 0.05;
        // 
        // splitter1
        // 
        this.splitter1.Dock = System.Windows.Forms.DockStyle.Right;
        this.splitter1.Location = new System.Drawing.Point(698, 0);
        this.splitter1.Name = "splitter1";
        this.splitter1.Size = new System.Drawing.Size(3, 547);
        this.splitter1.TabIndex = 2;
        this.splitter1.TabStop = false;
        // 
        // propertyGrid1
        // 
        this.propertyGrid1.Dock = System.Windows.Forms.DockStyle.Right;
        this.propertyGrid1.Location = new System.Drawing.Point(701, 0);
        this.propertyGrid1.Name = "propertyGrid1";
        this.propertyGrid1.Size = new System.Drawing.Size(295, 547);
        this.propertyGrid1.TabIndex = 1;
        // 
        // recalculateLayoutButton
        // 
        this.recalculateLayoutButton.Location = new System.Drawing.Point(740, 1);
        this.recalculateLayoutButton.Name = "recalculateLayoutButton";
        this.recalculateLayoutButton.Size = new System.Drawing.Size(193, 23);
        this.recalculateLayoutButton.TabIndex = 5;
        this.recalculateLayoutButton.Text = "Recalclulate Layout";
        this.recalculateLayoutButton.UseVisualStyleBackColor = true;
        this.recalculateLayoutButton.Click += new System.EventHandler(this.recalculateLayoutButton_Click);
        // 
        // button2
        // 
        this.button2.BackColor = System.Drawing.Color.PaleGreen;
        this.button2.Location = new System.Drawing.Point(429, 1);
        this.button2.Name = "button2";
        this.button2.Size = new System.Drawing.Size(133, 22);
        this.button2.TabIndex = 6;
        this.button2.Text = "button2";
        this.button2.UseVisualStyleBackColor = false;
        this.button2.Click += new System.EventHandler(this.button2_Click);
        // 
        // Form1
        // 
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.ClientSize = new System.Drawing.Size(994, 574);
        this.Controls.Add(this.button2);
        this.Controls.Add(this.recalculateLayoutButton);
        this.Controls.Add(this.panel1);
        this.Controls.Add(this.label1);
        this.Controls.Add(this.button1);
        this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
        this.Name = "Form1";
        this.Text = "Form1";
        this.panel1.ResumeLayout(false);
        this.ResumeLayout(false);
        this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.Button button1;
    private System.Windows.Forms.Label label1;
    private System.Windows.Forms.Panel panel1;
    private System.Windows.Forms.PropertyGrid propertyGrid1;
    private System.Windows.Forms.Button recalculateLayoutButton;
    private System.Windows.Forms.Splitter splitter1;
    private Microsoft.Glee.GraphViewerGdi.GViewer gViewer;
    private System.Windows.Forms.Button button2;

  }
}

