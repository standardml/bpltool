

import java.io.*;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.tomcat.util.http.fileupload.DiskFileUpload;
import org.apache.tomcat.util.http.fileupload.FileItem;
import org.apache.tomcat.util.http.fileupload.FileUploadException;
import org.xml.sax.SAXException;

import xhtmltool.XmlToXhtmlConverter;

import com.beepell.deployment.transform.Transform;

/**
 * Servlet implementation class Transform
 */
public class TransformServlet extends HttpServlet {
	private static final long serialVersionUID = 1L;
	private XmlToXhtmlConverter xmlConverter;
       
    /**
     * @see HttpServlet#HttpServlet()
     */
    public TransformServlet() {
        super();        
        // TODO Auto-generated constructor stub
    }
    
	public static String xmlToXhtml(InputStream in, String realPath)
	{
		System.out.println(com.sun.org.apache.xalan.internal.Version.getVersion());
		TransformerFactory tf = TransformerFactory.newInstance();

		try {
			InputStream xslt = new FileInputStream(realPath);
			Transformer t = tf.newTransformer(new StreamSource(xslt));
			t.setParameter("indent-elements", "yes");
			
			ByteArrayOutputStream s = new ByteArrayOutputStream();
			
			t.transform(new StreamSource(in), new StreamResult(s));
			byte[] out = s.toByteArray();

			return new String(out);
		}
		catch (FileNotFoundException e) {
			return "An internal error as occured : File Not Found"; 
		} catch (TransformerException e) {
			String error = "An internal error as occured : Transformer Exception";
			e.printStackTrace();
			error += "<br/>xalan version : "+com.sun.org.apache.xalan.internal.Version.getDevelopmentVersionNum()+"<br/>";
			error += "<br/>path : "+realPath+"<br/>";
			error += e.getMessage()+"<br/>";
		    Writer result = new StringWriter();
		    PrintWriter printWriter = new PrintWriter(result);
		    e.printStackTrace(printWriter);
			error += result.toString()+"<br/>";
			return error;
		}

	}

	/**
	 * Check if the request paramaters are ok,
	 * ie if there is a given bpel file.
	 * @param items the list of paramaters
	 * @return true if the params are ok.
	 */
    private static boolean checkParamaters(List<FileItem> items){
	    for (FileItem item : items) {
	    	if(item.getFieldName().equals("bpel")){
	    		String fileName = item.getName();
	    		return fileName!=null && !fileName.isEmpty();
	    	}
	    }
	    return false;
    }
    
    private String getFileArea(HttpSession session) {
    	
        // 1. Get the file area parameter from web.xml     
        String fileArea = getServletContext().getInitParameter("fileArea");
        
        // 2. Append with session id
        String sessionId = session.getId();
        if (!fileArea.endsWith("/")) {
      	  fileArea = fileArea + "/" + sessionId + "/";
        } else {
      	  fileArea = fileArea + sessionId + "/";
        }
        
        return fileArea;
    }
    
    private String writeFileToSessionFileArea(List<FileItem> items, String fileArea, HttpServletRequest request){

	    String bpelFileName = null;
        List<String> uploadedFileNames = new LinkedList<String>();
        
        // Write files to session file area
	    for (FileItem item : items) {
	    	if (item.isFormField()) {
	    		
	    	}else{
	    		String fileName = item.getName();
	    		if(fileName != null && !fileName.isEmpty()){
	    			uploadedFileNames.add(fileName);
	    			File target = new File(fileArea + fileName);
	    			try {
	    				item.write(target);
	    			} catch (Exception e) {
	    				e.printStackTrace();
	    				//TODO handle exception properly
	    			}
	    		}  
	    		if (item.getFieldName().equals("bpel"))
	    			bpelFileName = item.getName();	    	      
	    	}
	    }
	    
	    //Add the list of uploaded file to the request
	    request.setAttribute("UploadedFiles", uploadedFileNames);
	    return bpelFileName;
    }
    
    
    private File transform(String fileArea, String bpelFileName) {
	    
	    // 5. Transform the file
	    File source = new File(fileArea + bpelFileName);
	    String coreBpelFileName = bpelFileName.endsWith(".bpel") ? bpelFileName.substring(0,bpelFileName.length() - 5) : bpelFileName;
	    coreBpelFileName += ".cbpel";
	    File target = new File(fileArea + coreBpelFileName);
	    
	    Transform transform;
		try {
			transform = new Transform(getServletContext().getRealPath("/schemas"), getServletContext().getRealPath("/xslt"));
			transform.transform(source, target);
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return target;
    }

    
    private void performRequest(HttpServletRequest request, List<FileItem> items){
	    
    	// 1. Create session file area
    	String fileArea = getFileArea(request.getSession());
        File fileAreaFile = new File(fileArea);
        fileAreaFile.mkdirs();
    	
        // 2. Write files to session file area
        String bpelFileName = writeFileToSessionFileArea(items, fileArea, request);
        
        // 3. Transform the file
        File target = transform(fileArea, bpelFileName);
        
        // 4. Convert the source and result to xhtml
        try {
        	if(xmlConverter == null)
        		xmlConverter = new XmlToXhtmlConverter(getServletContext().getRealPath("/xmlverbatim/xmlverbatim.xsl"));
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (TransformerConfigurationException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
        try {
			FileInputStream streamResult = new FileInputStream(target);
			String result = xmlConverter.XmlToXhtml(streamResult);
			request.setAttribute("result", result);
			FileInputStream streamSource = new FileInputStream(fileArea + bpelFileName);
			String source = xmlConverter.XmlToXhtml(streamSource);
			request.setAttribute("source", source);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		// 5. Delete session file area
	    for (String file : fileAreaFile.list()) {
	    	new File(fileArea + file).delete();
	    }
	    fileAreaFile.delete();
    }
    
    
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		if(request.getParameter("example")!=null){
			String bpelFileName = request.getParameter("example");
			String fileArea = getServletContext().getRealPath("examples") + "/";
	        File target = transform(fileArea, bpelFileName);
	        try {
	        	if(xmlConverter == null)
	        		xmlConverter = new XmlToXhtmlConverter(getServletContext().getRealPath("/xmlverbatim/xmlverbatim.xsl"));
			} catch (FileNotFoundException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			} catch (TransformerConfigurationException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
	        try {
				FileInputStream streamResult = new FileInputStream(target);
				String result = xmlConverter.XmlToXhtml(streamResult);
				request.setAttribute("result", result);
				FileInputStream streamSource = new FileInputStream(fileArea + bpelFileName);
				String source = xmlConverter.XmlToXhtml(streamSource);
				request.setAttribute("source", source);
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (TransformerException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			target.delete();
			getServletConfig().getServletContext().getRequestDispatcher("/displayresult.jsp").forward(request,response);
		} else {
			//redirection to home page
			getServletConfig().getServletContext().getRequestDispatcher("/index.jsp").forward(request,response);
		}
	}
	
	
	

	/**
	 * @see HttpServlet#doPost(HttpServletRequest request, HttpServletResponse response)
	 */
	@SuppressWarnings("unchecked")
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

	    DiskFileUpload upload = new DiskFileUpload();
	    List<FileItem> items;
		try {
			items = (List<FileItem>) upload.parseRequest(request);
		    if(!checkParamaters(items)){
		    	request.setAttribute("BpelReq", true);
				getServletConfig().getServletContext().getRequestDispatcher("/index.jsp").forward(request,response);
		    } else {
		    	performRequest(request, items);
				getServletConfig().getServletContext().getRequestDispatcher("/displayresult.jsp").forward(request,response);
		    }
		} catch (FileUploadException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}


	    


			
	}

}
