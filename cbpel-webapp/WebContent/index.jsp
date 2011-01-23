<%@ page language="java" contentType="text/html; charset=UTF-8"
    pageEncoding="UTF-8"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <title>Core BPEL - Online Transformer</title>
  </head>
  <body>
    <form action="transform.jsp" method="post" enctype="multipart/form-data">
      <table>
        <tr>
          <td><label>bpel</label></td><td><input name="bpel" type="file" /></td>
        </tr><tr>
          <td><label>xsd</label></td><td><input name="xsd" type="file" /></td>
        </tr><tr>
          <td><label>wsdl</label></td><td><input name="wsdl" type="file" /></td>
        </tr><tr>
          <td><label>Validate files</label></td><td><input name="validate" type="checkbox" checked="checked" /></td>
        </tr>
        <tr>
          <td colspan="2"><input type="submit" /></td>
        </tr>
        
      </table>
      
    </form>  
  </body>
</html>