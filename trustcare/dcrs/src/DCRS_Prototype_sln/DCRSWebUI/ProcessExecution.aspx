<%@ Register
Assembly="AjaxControlToolkit" 
Namespace="AjaxControlToolkit"
TagPrefix="ajaxToolkit" %>
<%@ Page Title="" Language="C#" MasterPageFile="~/Site.Master" AutoEventWireup="true" CodeBehind="ProcessExecution.aspx.cs" Inherits="DCRSWebUI.ProcessExecution" EnableEventValidation="false" %>
<asp:Content ID="Content1" ContentPlaceHolderID="HeadContent" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">

<asp:ScriptManager ID="ScriptManager1" runat="server">
    </asp:ScriptManager>
<ajaxToolkit:CascadingDropDown
ID="CascadingDropDown2"    
runat="server"
TargetControlID="lbActions"
ParentControlID="lbPrincipals"
PromptText="Please select an Action"
ServiceMethod="GetActions"
ServicePath="services/ExecutionAssistanceService.asmx"
Category="Action" />
<h2>
    Process execution<br />
    <asp:Label ID="lProcessName" runat="server" Text="Label"></asp:Label>    
</h2>
<div>
    <div>        
        <table>
            <tr>
                <td valign=top>
                    <table>
                        <tr valign=top>
                            <td valign=top>
                             <!-- AutoPostBack="True" OnSelectedIndexChanged="lbPrincipals_SelectedIndexChanged" --> 
                                <asp:DropDownList ID="lbPrincipals" runat="server" Width="150px">
                                </asp:DropDownList>
                            </td>
                        </tr>
                        <tr valign=top>                            
                            <td valign=top>
                                <asp:DropDownList ID="lbActions" runat="server" Width="152px">
                                </asp:DropDownList>
                                <br/>
                                <asp:Button ID="btnExecute" runat="server" Text="Execute" 
                                    onclick="btnExecute_Click" />
                                <asp:Button ID="btnChangeProcess" runat="server" Text="Change Process" 
                                    onclick="btnChangeProcess_Click" />
                            </td>
                        </tr>
                    </table>
                </td>
                <td>
                    <div><img src="modelvis.aspx"></div>    
                </td>
            </tr>
        </table>
    </div>
    <div>
    </div>        
</div>
</asp:Content>
