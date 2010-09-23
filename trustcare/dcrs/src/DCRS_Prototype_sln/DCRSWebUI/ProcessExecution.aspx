<%@ Page Title="" Language="C#" MasterPageFile="~/Site.Master" AutoEventWireup="true" CodeBehind="ProcessExecution.aspx.cs" Inherits="DCRSWebUI.ProcessExecution" %>
<asp:Content ID="Content1" ContentPlaceHolderID="HeadContent" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">
<h2>
    Process execution<br />
    <asp:Label ID="lProcessName" runat="server" Text="Label"></asp:Label>
</h2>
<div>
    <div>        
        <asp:ListBox ID="lbPrincipals" runat="server" AutoPostBack="True" 
            onselectedindexchanged="lbPrincipals_SelectedIndexChanged"></asp:ListBox>
        <asp:ListBox ID="lbActions" runat="server"></asp:ListBox>
        <asp:Button ID="btnExecute" runat="server" Text="Execute" 
            onclick="btnExecute_Click" />
    </div>
    <div>
    </div>    
    <div><img src="modelvis.aspx"></div>    
</div>
</asp:Content>
