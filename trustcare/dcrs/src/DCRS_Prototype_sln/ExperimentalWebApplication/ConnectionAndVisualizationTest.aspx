<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="ConnectionAndVisualizationTest.aspx.cs" Inherits="ExperimentalWebApplication.ConnectionAndVisualizationTest" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
</head>
<body>
    <form id="form1" runat="server">
    <div>
        <table width=100%>
        <tr>
        <td width=50%>
        <asp:ListBox ID="ListBox1" runat="server" Height="110px" 
            onselectedindexchanged="ListBox1_SelectedIndexChanged"
            Width="383px" onload="ListBox1_Load">
        </asp:ListBox>
        <asp:Button ID="Button1" runat="server" onclick="Button1_Click" Text="Button" />
            <asp:Label ID="Label1" runat="server" Text="Label"></asp:Label>      
            </td>
        <td width=50%>
        <asp:ListBox ID="ListBox2" runat="server" Height="107px" 
            style="margin-top: 0px" Width="470px"></asp:ListBox>
            <asp:Button ID="Button2" runat="server" Text="Button" onclick="Button2_Click" />
        </td>
        </tr>
        </table>
    </div>
    <div><img src="modelvis.aspx"></div>
    </form>
</body>
</html>
