<%@ page language="java" contentType="text/html; charset=UTF-8"
  pageEncoding="UTF-8"%>
<%@page import="java.util.*"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title>Core BPEL - Online Transformer</title>
  <link rel="stylesheet" media="screen" type="text/css" title="Style"
        href="style.css" />
</head>
<body>
<div id="content"><jsp:include page="header.jsp" />
<%
  String[][] examples = {
        {"attributes.bpel",""},
        {"echo.bpel","echo.wsdl"},        
        {"invoke.bpel","invoke.wsdl"},
        {"onEvent.bpel","onEvent.wsdl"},
        {"pick.bpel","pick.wsdl"},
        {"receive.bpel","receive.wsdl"},
        {"receive2.bpel","receive2.wsdl"},
        {"repeatUntil.bpel","repeatUntil.wsdl"},
        {"reply.bpel","receive.wsdl"},
        {"sequence.1.bpel",""},
        {"sequence.2.bpel",""}                                
       };  
  
%>
  <div id="main">
  <h3>Examples</h3>
  <p>Here are some example WS-BPEL processes that may be transformed by clicking the corresponding "Transform" links. Each of them illustrate a particular aspect of Core BPEL, as hinted to in the file names.</p>
  <p>If you want to try the tool with your own WS-BPEL processes, go back to the <a href="./">main page</a>.</p>
  <table class="example">
      <tr>
      <th>WS-BPEL File</th>
      <th>WSDL File</th>
      <th>Transform</th>
      </tr>
      <%
    for(String[] e:examples){
      %>
      <tr>
      <td><a href="./examples/<%out.print(e[0]); %>"><%out.print(e[0]); %></a></td>
      <td><%if(e[1]!="") %>
          <a href="./examples/<%out.print(e[1]); %>"><%out.print(e[1]); %></a>
      </td>
      <td><a href="./TransformServlet?example=<%out.print(e[0]); %>">Transform</a></td>
      </tr>
<% }  %>    
  </table>    
  
  </div>



<jsp:include page="footer.jsp" />
</div>
</body>
</html>