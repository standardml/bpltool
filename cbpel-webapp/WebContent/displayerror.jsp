<%@page import="javax.xml.transform.TransformerException"%>
<%@page import="errors.InternalErrorException"%>
<%@ page language="java" contentType="text/html; charset=UTF-8"
  pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title>Core BPEL - Online Transformer</title>
  <link rel="stylesheet" media="screen" type="text/css" title="Style"
        href="style.css" />   
</head>
<body>
<div id="content">
  
  <jsp:include page="header.jsp" />

  <div id="main">
  <%
  if(request.getAttribute("exception")!=null) {
    Object exceptionAttribute = request.getAttribute("exception");
    if (request.getAttribute("exception") instanceof InternalErrorException) {
      try {
     	InternalErrorException e = (InternalErrorException) exceptionAttribute;
     	%><p class="error">An internal error as occurred : <%out.write(e.getShortMessage());%>.</p><%
      } catch (ClassCastException e){
    	%><p class="error">An internal error as occurred !</p><% 
      }
    } else if (request.getAttribute("exception") instanceof TransformerException) {
      try {
        TransformerException e = (TransformerException) request.getAttribute("exception");
        %><p class="error">An error as occurred during the Transformation !</p>
        <p class="error">Here is the trace of the error : </p>
        <p class="error"><%out.write(e.getLocalizedMessage());%></p>
        <%
      } catch (ClassCastException e){
      %><p class="error">An internal error as occurred !</p><% 
      }
    } else {
      %><p class="error">Another error as occurred !</p><%
    }
  } else {
	%><p class="error">An internal error as occurred !</p><%
  }%>
  <br/>
  <p>Please return to <a href="./">home page</a> to perform a new transformation.</p>
  
  </div>

  <jsp:include page="footer.jsp" />

  
</div>
</body>
</html>