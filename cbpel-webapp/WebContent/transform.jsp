<?xml version="1.0" encoding="UTF-8" ?>
<%@page import="java.io.FileInputStream"%>
<%@page import="com.beepell.deployment.transform.Transform"%>
<%@page import="java.io.File"%>
<%@page import="org.apache.jasper.tagplugins.jstl.core.ForEach"%>
<%@page import="org.apache.tomcat.util.http.fileupload.FileItem"%>
<%@page import="java.util.List"%>
<%@page import="org.apache.tomcat.util.http.fileupload.DiskFileUpload"%>
<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <title>Insert title here</title>
  </head>
  <body>
  <p>The following files were uploaded:</p>
  <ul>
    <%
    
      // 1. Get the file area parameter from web.xml
      ServletContext context = getServletContext() ;
      String realPath = context.getRealPath("/schemas");
      
      String fileArea = context.getInitParameter("fileArea");
      
      // 2. Append with session id
      String sessionId = session.getId();
      if (!fileArea.endsWith("/")) {
    	  fileArea = fileArea + "/" + sessionId + "/";
      } else {
    	  fileArea = fileArea + sessionId + "/";
      }
      
      // 3. Create session file area
      File fileAreaFile = new File(fileArea);
      fileAreaFile.mkdirs();
      
      // 4. Write files to session file area
      String bpelFileName = null;
      DiskFileUpload upload = new DiskFileUpload();
      List<FileItem> files = (List<FileItem>) upload.parseRequest(request);
      for (FileItem file : files) {
    	  if (file.getName() != null && !file.getName().isEmpty()) {
              file.write(new File(fileArea + file.getName()));
    	      %><li><%= file.getName() %></li><%
    	      if (file.getFieldName().equals("bpel")) {
    	    	  bpelFileName = file.getName();
    	      }
    	  }
      }
      
      // 5. Transform the file
      File source = new File(fileArea + bpelFileName);
      String coreBpelFileName = bpelFileName.endsWith(".bpel") ? bpelFileName.substring(0,bpelFileName.length() - 5) : bpelFileName;
      coreBpelFileName += ".cbpel";
      File target = new File(fileArea + coreBpelFileName);
      
      Transform transform = new Transform(getServletContext().getRealPath("/schemas"), getServletContext().getRealPath("/xslt"));
      transform.transform(source, target);
      
      // 6. Write the file back
      %><p>This is the transformed Core BPEL:</p><textarea style="width: 800px; height:600px"><% 
      FileInputStream stream = new FileInputStream(target);
      int c = 0;
      while (true) {
    	  c = stream.read();
    	  if (c == -1)
    		  break;
    	  else
    		  out.write(c);
      }
      stream.close();
      %></textarea><% 
      
      // 7. Delete session file area
      for (String file : fileAreaFile.list()) {
    	  new File(fileArea + file).delete();
      }
      fileAreaFile.delete();
      
    
    %>
    </ul>
  </body>
</html>