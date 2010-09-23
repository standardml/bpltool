<%@ Page Title="" Language="C#" MasterPageFile="~/Site.Master" AutoEventWireup="true" CodeBehind="ProcessSelection.aspx.cs" Inherits="DCRSWebUI.ProcessSelection" %>
<asp:Content ID="Content1" ContentPlaceHolderID="HeadContent" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">

Select a process...<br />
<asp:DropDownList ID="ddlProcess" runat="server" 
        onselectedindexchanged="ddlProcess_SelectedIndexChanged" AutoPostBack=true></asp:DropDownList><br />
<asp:DropDownList ID="ddlProcessInstance" runat="server" 
        onselectedindexchanged="ddlProcessInstance_SelectedIndexChanged" AutoPostBack=true></asp:DropDownList><br />
&nbsp;
</asp:Content>
