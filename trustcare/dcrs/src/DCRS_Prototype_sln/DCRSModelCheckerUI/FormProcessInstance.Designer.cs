using System;
using ITU.DK.DCRS.CommonTypes.ServiceContracts;

namespace DCRSModelCheckerUI
{
    partial class FormProcessInstance
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
            this.groupBoxSpecification = new System.Windows.Forms.GroupBox();
            this.listViewConditionResponses = new System.Windows.Forms.ListView();
            this.columnHeader3 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnHeader4 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.label7 = new System.Windows.Forms.Label();
            this.listViewIncludesExcludes = new System.Windows.Forms.ListView();
            this.columnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnHeader2 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.label6 = new System.Windows.Forms.Label();
            this.listViewActionToRoles = new System.Windows.Forms.ListView();
            this.Action = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ActionRole = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.listViewRoleToPrincipals = new System.Windows.Forms.ListView();
            this.Role = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.Principal = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.label5 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.listBoxPrincipals = new System.Windows.Forms.ListBox();
            this.label2 = new System.Windows.Forms.Label();
            this.listBoxRoles = new System.Windows.Forms.ListBox();
            this.label1 = new System.Windows.Forms.Label();
            this.listBoxActionsSet = new System.Windows.Forms.ListBox();
            this.groupBoxRuntime = new System.Windows.Forms.GroupBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.label15 = new System.Windows.Forms.Label();
            this.textBoxExeResult = new System.Windows.Forms.TextBox();
            this.buttonExecuteAction = new System.Windows.Forms.Button();
            this.listBoxPrincipalExe = new System.Windows.Forms.ListBox();
            this.label14 = new System.Windows.Forms.Label();
            this.listBoxEnabledActions = new System.Windows.Forms.ListBox();
            this.label11 = new System.Windows.Forms.Label();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.textBoxExecutionTrace = new System.Windows.Forms.TextBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.labelStateRank = new System.Windows.Forms.Label();
            this.label13 = new System.Windows.Forms.Label();
            this.labelAceeptingState = new System.Windows.Forms.Label();
            this.label12 = new System.Windows.Forms.Label();
            this.listBoxExecutedActions = new System.Windows.Forms.ListBox();
            this.listBoxIncludedActions = new System.Windows.Forms.ListBox();
            this.label8 = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.listBoxPendingResponses = new System.Windows.Forms.ListBox();
            this.groupBoxSpecification.SuspendLayout();
            this.groupBoxRuntime.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox1.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupBoxSpecification
            // 
            this.groupBoxSpecification.Controls.Add(this.listViewConditionResponses);
            this.groupBoxSpecification.Controls.Add(this.label7);
            this.groupBoxSpecification.Controls.Add(this.listViewIncludesExcludes);
            this.groupBoxSpecification.Controls.Add(this.label6);
            this.groupBoxSpecification.Controls.Add(this.listViewActionToRoles);
            this.groupBoxSpecification.Controls.Add(this.listViewRoleToPrincipals);
            this.groupBoxSpecification.Controls.Add(this.label5);
            this.groupBoxSpecification.Controls.Add(this.label4);
            this.groupBoxSpecification.Controls.Add(this.label3);
            this.groupBoxSpecification.Controls.Add(this.listBoxPrincipals);
            this.groupBoxSpecification.Controls.Add(this.label2);
            this.groupBoxSpecification.Controls.Add(this.listBoxRoles);
            this.groupBoxSpecification.Controls.Add(this.label1);
            this.groupBoxSpecification.Controls.Add(this.listBoxActionsSet);
            this.groupBoxSpecification.Font = new System.Drawing.Font("Calibri", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBoxSpecification.Location = new System.Drawing.Point(12, 17);
            this.groupBoxSpecification.Name = "groupBoxSpecification";
            this.groupBoxSpecification.Size = new System.Drawing.Size(1011, 185);
            this.groupBoxSpecification.TabIndex = 0;
            this.groupBoxSpecification.TabStop = false;
            this.groupBoxSpecification.Text = "DCRS Process Specification";
            // 
            // listViewConditionResponses
            // 
            this.listViewConditionResponses.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader3,
            this.columnHeader4});
            this.listViewConditionResponses.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listViewConditionResponses.Location = new System.Drawing.Point(789, 44);
            this.listViewConditionResponses.Name = "listViewConditionResponses";
            this.listViewConditionResponses.Size = new System.Drawing.Size(204, 124);
            this.listViewConditionResponses.TabIndex = 15;
            this.listViewConditionResponses.UseCompatibleStateImageBehavior = false;
            this.listViewConditionResponses.View = System.Windows.Forms.View.Details;
            // 
            // columnHeader3
            // 
            this.columnHeader3.Text = "Parent";
            this.columnHeader3.Width = 50;
            // 
            // columnHeader4
            // 
            this.columnHeader4.Text = "Child";
            this.columnHeader4.Width = 72;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label7.Location = new System.Drawing.Point(798, 20);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(128, 15);
            this.label7.TabIndex = 14;
            this.label7.Text = "Conditions/Responses";
            // 
            // listViewIncludesExcludes
            // 
            this.listViewIncludesExcludes.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1,
            this.columnHeader2});
            this.listViewIncludesExcludes.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listViewIncludesExcludes.Location = new System.Drawing.Point(590, 44);
            this.listViewIncludesExcludes.Name = "listViewIncludesExcludes";
            this.listViewIncludesExcludes.Size = new System.Drawing.Size(193, 124);
            this.listViewIncludesExcludes.TabIndex = 13;
            this.listViewIncludesExcludes.UseCompatibleStateImageBehavior = false;
            this.listViewIncludesExcludes.View = System.Windows.Forms.View.Details;
            // 
            // columnHeader1
            // 
            this.columnHeader1.Text = "Parent";
            this.columnHeader1.Width = 50;
            // 
            // columnHeader2
            // 
            this.columnHeader2.Text = "Child";
            this.columnHeader2.Width = 72;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(590, 20);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(103, 15);
            this.label6.TabIndex = 12;
            this.label6.Text = "Includes/Excludes";
            // 
            // listViewActionToRoles
            // 
            this.listViewActionToRoles.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.Action,
            this.ActionRole});
            this.listViewActionToRoles.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listViewActionToRoles.Location = new System.Drawing.Point(453, 44);
            this.listViewActionToRoles.Name = "listViewActionToRoles";
            this.listViewActionToRoles.Size = new System.Drawing.Size(122, 124);
            this.listViewActionToRoles.TabIndex = 11;
            this.listViewActionToRoles.UseCompatibleStateImageBehavior = false;
            this.listViewActionToRoles.View = System.Windows.Forms.View.Details;
            // 
            // Action
            // 
            this.Action.Text = "Action";
            this.Action.Width = 50;
            // 
            // ActionRole
            // 
            this.ActionRole.Text = "Role";
            this.ActionRole.Width = 49;
            // 
            // listViewRoleToPrincipals
            // 
            this.listViewRoleToPrincipals.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.Role,
            this.Principal});
            this.listViewRoleToPrincipals.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listViewRoleToPrincipals.Location = new System.Drawing.Point(306, 44);
            this.listViewRoleToPrincipals.Name = "listViewRoleToPrincipals";
            this.listViewRoleToPrincipals.Size = new System.Drawing.Size(134, 124);
            this.listViewRoleToPrincipals.TabIndex = 10;
            this.listViewRoleToPrincipals.UseCompatibleStateImageBehavior = false;
            this.listViewRoleToPrincipals.View = System.Windows.Forms.View.Details;
            // 
            // Role
            // 
            this.Role.Text = "Role";
            this.Role.Width = 50;
            // 
            // Principal
            // 
            this.Principal.Text = "Principal";
            this.Principal.Width = 72;
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.Location = new System.Drawing.Point(453, 20);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(75, 15);
            this.label5.TabIndex = 9;
            this.label5.Text = "Action-Roles";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(306, 20);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(86, 15);
            this.label4.TabIndex = 7;
            this.label4.Text = "Role-Principals";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(208, 20);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(58, 15);
            this.label3.TabIndex = 5;
            this.label3.Text = "Principals";
            // 
            // listBoxPrincipals
            // 
            this.listBoxPrincipals.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxPrincipals.FormattingEnabled = true;
            this.listBoxPrincipals.ItemHeight = 15;
            this.listBoxPrincipals.Location = new System.Drawing.Point(208, 44);
            this.listBoxPrincipals.Name = "listBoxPrincipals";
            this.listBoxPrincipals.Size = new System.Drawing.Size(89, 124);
            this.listBoxPrincipals.TabIndex = 4;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(109, 20);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(36, 15);
            this.label2.TabIndex = 3;
            this.label2.Text = "Roles";
            // 
            // listBoxRoles
            // 
            this.listBoxRoles.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxRoles.FormattingEnabled = true;
            this.listBoxRoles.ItemHeight = 15;
            this.listBoxRoles.Location = new System.Drawing.Point(109, 44);
            this.listBoxRoles.Name = "listBoxRoles";
            this.listBoxRoles.Size = new System.Drawing.Size(89, 124);
            this.listBoxRoles.TabIndex = 2;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(10, 20);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(47, 15);
            this.label1.TabIndex = 1;
            this.label1.Text = "Actions";
            // 
            // listBoxActionsSet
            // 
            this.listBoxActionsSet.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxActionsSet.FormattingEnabled = true;
            this.listBoxActionsSet.ItemHeight = 15;
            this.listBoxActionsSet.Location = new System.Drawing.Point(11, 44);
            this.listBoxActionsSet.Name = "listBoxActionsSet";
            this.listBoxActionsSet.Size = new System.Drawing.Size(89, 124);
            this.listBoxActionsSet.TabIndex = 0;
            // 
            // groupBoxRuntime
            // 
            this.groupBoxRuntime.Controls.Add(this.groupBox3);
            this.groupBoxRuntime.Controls.Add(this.groupBox2);
            this.groupBoxRuntime.Controls.Add(this.groupBox1);
            this.groupBoxRuntime.Font = new System.Drawing.Font("Calibri", 11.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupBoxRuntime.Location = new System.Drawing.Point(9, 208);
            this.groupBoxRuntime.Name = "groupBoxRuntime";
            this.groupBoxRuntime.Size = new System.Drawing.Size(1014, 322);
            this.groupBoxRuntime.TabIndex = 1;
            this.groupBoxRuntime.TabStop = false;
            this.groupBoxRuntime.Text = "DCRS Runtime";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.label15);
            this.groupBox3.Controls.Add(this.textBoxExeResult);
            this.groupBox3.Controls.Add(this.buttonExecuteAction);
            this.groupBox3.Controls.Add(this.listBoxPrincipalExe);
            this.groupBox3.Controls.Add(this.label14);
            this.groupBox3.Controls.Add(this.listBoxEnabledActions);
            this.groupBox3.Controls.Add(this.label11);
            this.groupBox3.Location = new System.Drawing.Point(390, 19);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(326, 293);
            this.groupBox3.TabIndex = 12;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Execute Actions";
            // 
            // label15
            // 
            this.label15.AutoSize = true;
            this.label15.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label15.Location = new System.Drawing.Point(11, 158);
            this.label15.Name = "label15";
            this.label15.Size = new System.Drawing.Size(97, 15);
            this.label15.TabIndex = 11;
            this.label15.Text = "Execution Result";
            // 
            // textBoxExeResult
            // 
            this.textBoxExeResult.Location = new System.Drawing.Point(10, 176);
            this.textBoxExeResult.Multiline = true;
            this.textBoxExeResult.Name = "textBoxExeResult";
            this.textBoxExeResult.ReadOnly = true;
            this.textBoxExeResult.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.textBoxExeResult.Size = new System.Drawing.Size(274, 104);
            this.textBoxExeResult.TabIndex = 10;
            // 
            // buttonExecuteAction
            // 
            this.buttonExecuteAction.Location = new System.Drawing.Point(230, 42);
            this.buttonExecuteAction.Name = "buttonExecuteAction";
            this.buttonExecuteAction.Size = new System.Drawing.Size(76, 49);
            this.buttonExecuteAction.TabIndex = 8;
            this.buttonExecuteAction.Text = "Execute Action";
            this.buttonExecuteAction.UseVisualStyleBackColor = true;
            this.buttonExecuteAction.Click += new System.EventHandler(this.buttonExecuteAction_Click);
            // 
            // listBoxPrincipalExe
            // 
            this.listBoxPrincipalExe.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxPrincipalExe.FormattingEnabled = true;
            this.listBoxPrincipalExe.ItemHeight = 15;
            this.listBoxPrincipalExe.Location = new System.Drawing.Point(118, 42);
            this.listBoxPrincipalExe.Name = "listBoxPrincipalExe";
            this.listBoxPrincipalExe.Size = new System.Drawing.Size(89, 109);
            this.listBoxPrincipalExe.TabIndex = 6;
            // 
            // label14
            // 
            this.label14.AutoSize = true;
            this.label14.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label14.Location = new System.Drawing.Point(117, 24);
            this.label14.Name = "label14";
            this.label14.Size = new System.Drawing.Size(58, 15);
            this.label14.TabIndex = 7;
            this.label14.Text = "Principals";
            // 
            // listBoxEnabledActions
            // 
            this.listBoxEnabledActions.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxEnabledActions.FormattingEnabled = true;
            this.listBoxEnabledActions.ItemHeight = 15;
            this.listBoxEnabledActions.Location = new System.Drawing.Point(10, 42);
            this.listBoxEnabledActions.Name = "listBoxEnabledActions";
            this.listBoxEnabledActions.Size = new System.Drawing.Size(89, 109);
            this.listBoxEnabledActions.TabIndex = 4;
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label11.Location = new System.Drawing.Point(9, 24);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(93, 15);
            this.label11.TabIndex = 5;
            this.label11.Text = "Enabled Actions";
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.textBoxExecutionTrace);
            this.groupBox2.Location = new System.Drawing.Point(17, 19);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.RightToLeft = System.Windows.Forms.RightToLeft.No;
            this.groupBox2.Size = new System.Drawing.Size(360, 82);
            this.groupBox2.TabIndex = 11;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Execution Trace";
            // 
            // textBoxExecutionTrace
            // 
            this.textBoxExecutionTrace.Location = new System.Drawing.Point(6, 20);
            this.textBoxExecutionTrace.Multiline = true;
            this.textBoxExecutionTrace.Name = "textBoxExecutionTrace";
            this.textBoxExecutionTrace.ReadOnly = true;
            this.textBoxExecutionTrace.ScrollBars = System.Windows.Forms.ScrollBars.Both;
            this.textBoxExecutionTrace.Size = new System.Drawing.Size(341, 50);
            this.textBoxExecutionTrace.TabIndex = 9;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.labelStateRank);
            this.groupBox1.Controls.Add(this.label13);
            this.groupBox1.Controls.Add(this.labelAceeptingState);
            this.groupBox1.Controls.Add(this.label12);
            this.groupBox1.Controls.Add(this.listBoxExecutedActions);
            this.groupBox1.Controls.Add(this.listBoxIncludedActions);
            this.groupBox1.Controls.Add(this.label8);
            this.groupBox1.Controls.Add(this.label10);
            this.groupBox1.Controls.Add(this.label9);
            this.groupBox1.Controls.Add(this.listBoxPendingResponses);
            this.groupBox1.Location = new System.Drawing.Point(11, 99);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(366, 213);
            this.groupBox1.TabIndex = 10;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Current State";
            // 
            // labelStateRank
            // 
            this.labelStateRank.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelStateRank.Location = new System.Drawing.Point(262, 23);
            this.labelStateRank.Name = "labelStateRank";
            this.labelStateRank.Size = new System.Drawing.Size(48, 15);
            this.labelStateRank.TabIndex = 13;
            this.labelStateRank.Text = "0";
            // 
            // label13
            // 
            this.label13.AutoSize = true;
            this.label13.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label13.Location = new System.Drawing.Point(185, 23);
            this.label13.Name = "label13";
            this.label13.Size = new System.Drawing.Size(72, 15);
            this.label13.TabIndex = 12;
            this.label13.Text = "State Rank: ";
            // 
            // labelAceeptingState
            // 
            this.labelAceeptingState.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelAceeptingState.Location = new System.Drawing.Point(116, 23);
            this.labelAceeptingState.Name = "labelAceeptingState";
            this.labelAceeptingState.Size = new System.Drawing.Size(63, 15);
            this.labelAceeptingState.TabIndex = 11;
            this.labelAceeptingState.Text = "true/false";
            // 
            // label12
            // 
            this.label12.AutoSize = true;
            this.label12.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label12.Location = new System.Drawing.Point(12, 23);
            this.label12.Name = "label12";
            this.label12.Size = new System.Drawing.Size(98, 15);
            this.label12.TabIndex = 10;
            this.label12.Text = "State Accepting?";
            // 
            // listBoxExecutedActions
            // 
            this.listBoxExecutedActions.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxExecutedActions.FormattingEnabled = true;
            this.listBoxExecutedActions.ItemHeight = 15;
            this.listBoxExecutedActions.Location = new System.Drawing.Point(128, 68);
            this.listBoxExecutedActions.Name = "listBoxExecutedActions";
            this.listBoxExecutedActions.SelectionMode = System.Windows.Forms.SelectionMode.None;
            this.listBoxExecutedActions.Size = new System.Drawing.Size(89, 124);
            this.listBoxExecutedActions.TabIndex = 4;
            // 
            // listBoxIncludedActions
            // 
            this.listBoxIncludedActions.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxIncludedActions.FormattingEnabled = true;
            this.listBoxIncludedActions.ItemHeight = 15;
            this.listBoxIncludedActions.Location = new System.Drawing.Point(13, 68);
            this.listBoxIncludedActions.Name = "listBoxIncludedActions";
            this.listBoxIncludedActions.SelectionMode = System.Windows.Forms.SelectionMode.None;
            this.listBoxIncludedActions.Size = new System.Drawing.Size(89, 124);
            this.listBoxIncludedActions.TabIndex = 2;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label8.Location = new System.Drawing.Point(12, 44);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(96, 15);
            this.label8.TabIndex = 3;
            this.label8.Text = "Included Actions";
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label10.Location = new System.Drawing.Point(242, 44);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(111, 15);
            this.label10.TabIndex = 7;
            this.label10.Text = "Pending Responses";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label9.Location = new System.Drawing.Point(127, 44);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(100, 15);
            this.label9.TabIndex = 5;
            this.label9.Text = "Executed Actions";
            // 
            // listBoxPendingResponses
            // 
            this.listBoxPendingResponses.Font = new System.Drawing.Font("Calibri", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.listBoxPendingResponses.FormattingEnabled = true;
            this.listBoxPendingResponses.ItemHeight = 15;
            this.listBoxPendingResponses.Location = new System.Drawing.Point(248, 68);
            this.listBoxPendingResponses.Name = "listBoxPendingResponses";
            this.listBoxPendingResponses.SelectionMode = System.Windows.Forms.SelectionMode.None;
            this.listBoxPendingResponses.Size = new System.Drawing.Size(89, 124);
            this.listBoxPendingResponses.TabIndex = 6;
            // 
            // FormProcessInstance
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1070, 565);
            this.Controls.Add(this.groupBoxRuntime);
            this.Controls.Add(this.groupBoxSpecification);
            this.Name = "FormProcessInstance";
            this.Text = "Process Instance";
            this.groupBoxSpecification.ResumeLayout(false);
            this.groupBoxSpecification.PerformLayout();
            this.groupBoxRuntime.ResumeLayout(false);
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.GroupBox groupBoxSpecification;
        private System.Windows.Forms.GroupBox groupBoxRuntime;
        private System.Windows.Forms.Label label3;
        internal System.Windows.Forms.ListBox listBoxPrincipals;
        private System.Windows.Forms.Label label2;
        internal System.Windows.Forms.ListBox listBoxRoles;
        private System.Windows.Forms.Label label1;
        internal System.Windows.Forms.ListBox listBoxActionsSet;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.ListView listViewRoleToPrincipals;
        private System.Windows.Forms.ColumnHeader Role;
        private System.Windows.Forms.ColumnHeader Principal;
        private System.Windows.Forms.ListView listViewActionToRoles;
        private System.Windows.Forms.ColumnHeader Action;
        private System.Windows.Forms.ColumnHeader ActionRole;
        private System.Windows.Forms.ListView listViewConditionResponses;
        private System.Windows.Forms.ColumnHeader columnHeader3;
        private System.Windows.Forms.ColumnHeader columnHeader4;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.ListView listViewIncludesExcludes;
        private System.Windows.Forms.ColumnHeader columnHeader1;
        private System.Windows.Forms.ColumnHeader columnHeader2;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label10;
        internal System.Windows.Forms.ListBox listBoxPendingResponses;
        private System.Windows.Forms.Label label9;
        internal System.Windows.Forms.ListBox listBoxExecutedActions;
        private System.Windows.Forms.Label label8;
        internal System.Windows.Forms.ListBox listBoxIncludedActions;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.Label label12;
        private System.Windows.Forms.TextBox textBoxExecutionTrace;
        private System.Windows.Forms.Label label13;
        private System.Windows.Forms.Label labelAceeptingState;
        private System.Windows.Forms.Label labelStateRank;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.GroupBox groupBox3;
        internal System.Windows.Forms.ListBox listBoxEnabledActions;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.Button buttonExecuteAction;
        internal System.Windows.Forms.ListBox listBoxPrincipalExe;
        private System.Windows.Forms.Label label14;
        private System.Windows.Forms.Label label15;
        private System.Windows.Forms.TextBox textBoxExeResult;
    }
}