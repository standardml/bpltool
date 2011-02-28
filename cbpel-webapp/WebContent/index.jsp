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
<div id="content"><jsp:include page="header.jsp" />

  <div id="main">
    <h3>Welcome…</h3>
    <p>… to the Online version of the Core BPEL Transformer. To learn more about Core BPEL and the Transformer tool, including java and XSLT source code, 
    please visit <a href="http://www.itu.dk/research/CosmoBiz/wiki/pmwiki.php?n=Main.CoreBPEL" target="_blank">this page</a>.</p> 
    <p>The Core BPEL Transformer turns any <em>executable</em> WS-BPEL process into an equivalent Core BPEL process. 
    A number of executable WS-BPEL processes are available <a href="./examples.jsp">here</a>; you can use these to experiment with the Transformer and Core BPEL.</p>
    
    <br/>
    <h3>Transformation</h3>
    <p>Choose a WS-BPEL file to transform. Any WSDL and XSD files referenced by the process must also be provided; the references must be relative and assume that all the files are in the same directory.</p>
<%
	boolean requiredFieldError = request.getAttribute("BpelReq")==Boolean.TRUE;
    if(requiredFieldError){
%>
    <p class="error">Error : You must choose at least a valid WS-BPEL file.</p>
<% 
    }
%>
    <div class="data_input">
      <form action="TransformServlet" method="post" enctype="multipart/form-data">
        <p class="file_input_label"><label<%
    	if (requiredFieldError) {
    		out.write(" class=\"error_required\"");
    	}        
        %>>Pick a WS-BPEL file :</label></p>
        <p class="file_input"><input name="bpel" type="file" /></p>
        
        <div id="opt_field_zone">        
<%
        //fileNumber should stay null if javascript is enabled
        int fileNumber = 0;//((Integer) request.getAttribute("FileNumber")).intValue();
        if(request.getParameter("filenb")!=null){
          try {
            fileNumber = Integer.parseInt(request.getParameter("filenb"));
          } catch (NumberFormatException e){
            // wrong parameter : don't add fields
          }
        }
        for(int i=0;i<fileNumber;i++){
%>
        <p class="file_input_label"><label>Pick an <em>optional</em> WSDL or XSD file :</label></p>
        <p class="file_input"><input name="opt_file<%out.print(i);%>" type="file" /></p>
<%      }
%>
        <p class="add_link" id=p_add_link><a id=a_add_link href="./index.jsp?filenb=<%out.print(fileNumber+1);%>">Add another file (WSDL or XSD)…</a></p>
        </div>
        
        <p><label>Validate files </label><input name="validate" type="checkbox" checked="checked" /></p>
        <p class="file_input"><input type="submit" name="bsubmit"/> <input type="reset" /></p>
      </form>
    </div>
  </div>

<jsp:include page="footer.jsp" />
</div>
<script type="text/javascript" src="addfield.js"></script>    
</body>
</html>