<%@ Page Title="" Language="C#" MasterPageFile="~/Site.Master" AutoEventWireup="true" CodeBehind="RepositoryView.aspx.cs" Inherits="DCRSWebUI.RepositoryView" %>
<asp:Content ID="Content1" ContentPlaceHolderID="HeadContent" runat="server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="MainContent" runat="server">

<h2>Existing process list</h2>


	<form id="select_process_list" runat="server">
	<p>
		<asp:ListBox ID="ProcessList" runat="server" Height="110px" 
			onselectedindexchanged="ProcessList_SelectedIndexChanged"
			Width="383px" onload="ProcessList_Load">
		</asp:ListBox>
        <br />
		<asp:Button ID="SelectProcess" runat="server" onclick="SelectProcess_Click" Text="Select" />
	 </p>
	 <p>
		<asp:Label ID="ProcessID" runat="server" Text="No selected process"></asp:Label>      
        <br />
		<asp:ListBox ID="InstanceList" runat="server" Height="107px" 
			style="margin-top: 0px" Width="470px"></asp:ListBox>
			<asp:Button ID="SelectInstance" runat="server" Text="Select" onclick="SelectInstance_Click" />

	</p>
	</form>
	<div><img src="modelvis.aspx" alt="spec preview" /></div>
	

</asp:Content>
