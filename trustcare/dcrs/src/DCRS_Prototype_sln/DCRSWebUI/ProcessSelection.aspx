<%@ Register
Assembly="AjaxControlToolkit" 
Namespace="AjaxControlToolkit"
TagPrefix="ajaxToolkit" %>
<%@ Page Title="" Language="C#" MasterPageFile="~/Site.Master" AutoEventWireup="true" CodeBehind="ProcessSelection.aspx.cs" Inherits="DCRSWebUI.ProcessSelection" EnableEventValidation="false" %>
<asp:Content ID="Content1" ContentPlaceHolderID="HeadContent" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">

<asp:ScriptManager ID="ScriptManager1" runat="server">
    </asp:ScriptManager>
<ajaxToolkit:CascadingDropDown
ID="CascadingDropDown2"    
runat="server"
TargetControlID="ddlProcessInstance"
ParentControlID="ddlProcess"
PromptText="Please select a process instance"
ServiceMethod="GetProcessInstanceIdsByProcessId"
ServicePath="~/services/RepositoryService.asmx"
Category="ProcessInstance" />

Select a process...<br />
<asp:DropDownList ID="ddlProcess" runat="server"></asp:DropDownList><br />
<asp:DropDownList ID="ddlProcessInstance" runat="server"></asp:DropDownList><br/>
<asp:Button ID="btnSelect" runat="server" Text="Select" onclick="btnSelect_Click" />
&nbsp;
</asp:Content>
