<%@ Register
Assembly="AjaxControlToolkit" 
Namespace="AjaxControlToolkit"
TagPrefix="ajaxToolkit" %>
<%@ Page Title="" Language="C#" MasterPageFile="~/Site.Master" AutoEventWireup="true" CodeBehind="PrincipalExecution.aspx.cs" Inherits="DCRSWebUI.PrincipalExecution" EnableEventValidation="false" %>
<asp:Content ID="Content1" ContentPlaceHolderID="HeadContent" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">

<asp:ScriptManager ID="ScriptManager1" runat="server">
    </asp:ScriptManager>
<h2>
    Principal execution<br />
    <asp:Label ID="lProcessName" runat="server" Text="Label"></asp:Label>    
</h2>
<div>
    <div>        
        <table>
            <tr>
                <td valign=top>
                    <asp:Button ID="btnChangeProcess" runat="server" Text="Change Process" 
                        onclick="btnChangeProcess_Click" />
                        <br/>
                    <asp:DropDownList ID="lbPrincipals" runat="server" Width="150px">
                    </asp:DropDownList>
                    <asp:Button ID="btnSelect" runat="server" Text="Select" onclick="btnSelect_Click" />
                    <br/>
                </td>
            </tr>
             <tr valign=top>                            
                <td valign=top>
                    <div>
                        <asp:Label ID="errorLabel" runat="server" Text="" ForeColor="#CC0000"></asp:Label><br/>
                        <asp:ImageButton ID="ImageButton1" src="principalvis.aspx" runat="server" 
                            onclick="ImageButton1_Click" />
                    </div>    
                </td>
            </tr>
        </table>
    </div>
    <div>
    </div>        
</div>
</asp:Content>
