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

  <div id="main"><%
    if (request.getAttribute("UploadedFiles")!=null){%> 
    <div class="file_list">
    <p>The following files were uploaded:</p>
    <ul><%
        try{
          List<String> fileNames = (List<String>) request.getAttribute("UploadedFiles");
          for(String fileName:fileNames){%>
            <li><%out.write(fileName);%></li><%
          }
        } catch (ClassCastException e) {
           //ignore the attribute 
        }
        
%>
    </ul>
    </div><%
    } %>
  <div class="result">
  
    <div class="source">
      <p>This is the source BPEL:</p>
<%
        out.write(request.getAttribute("source").toString());
%>
    </div>
  
    <div class="target">
      <p>This is the transformed Core BPEL:</p>
<%
        out.write(request.getAttribute("result").toString());
%>
    </div>

  </div>
  </div>


<jsp:include page="footer.jsp" />
</div>
</body>
</html>