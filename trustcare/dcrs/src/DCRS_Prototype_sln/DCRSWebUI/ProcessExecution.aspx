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
        <table>
            <tr>
                <td valign=top>
                    <table>
                        <tr valign=top>
                            <td valign=top>
                                <asp:ListBox ID="lbPrincipals" runat="server" AutoPostBack="True" 
                                    onselectedindexchanged="lbPrincipals_SelectedIndexChanged" Height="200px" 
                                    Width="150px"></asp:ListBox>
                            </td>
                        </tr>
                        <tr valign=top>                            
                            <td valign=top>
                                <asp:ListBox ID="lbActions" runat="server" Height="200px" Width="150px"></asp:ListBox>
                                <br/>
                                <asp:Button ID="btnExecute" runat="server" Text="Execute" 
                                    onclick="btnExecute_Click" />
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
