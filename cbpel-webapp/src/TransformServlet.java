

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.xml.transform.TransformerException;

import org.apache.tomcat.util.http.fileupload.DiskFileUpload;
import org.apache.tomcat.util.http.fileupload.FileItem;
import org.apache.tomcat.util.http.fileupload.FileUploadException;
import org.xml.sax.SAXException;

import xhtmltool.XmlToXhtmlConverter;

import com.beepell.deployment.transform.Transform;

import errors.InternalErrorException;

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
    
    private String writeFileToSessionFileArea(List<FileItem> items, String fileArea, HttpServletRequest request) 
    	throws InternalErrorException{

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
	    				throw new InternalErrorException(e,"Unable to write the file "+fileName+" to disk.");
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
    
    
    private File transform(String fileArea, String bpelFileName) throws TransformerException, InternalErrorException {
	    
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
			throw new InternalErrorException(e,"SAXException");		
		}
		return target;
    }
    
    private void xhtmlConversion(String sourceFile, File target, HttpServletRequest request) throws InternalErrorException{
    	
    	//init xmlConverter if necessary
    	if(xmlConverter == null)
       		xmlConverter = new XmlToXhtmlConverter(getServletContext().getRealPath("/xmlverbatim/xmlverbatim.xsl"));
		try {
			FileInputStream streamResult = new FileInputStream(target);
			String result = xmlConverter.XmlToXhtml(streamResult);
			request.setAttribute("result", result);
			FileInputStream streamSource = new FileInputStream(sourceFile);
			String source = xmlConverter.XmlToXhtml(streamSource);
			request.setAttribute("source", source);
		} catch (FileNotFoundException e) {
			throw new InternalErrorException(e, "File not found");
		} catch (TransformerException e) {
			throw new InternalErrorException(e,
					"Conversion of source or result to XHTML failed");
		}
    }

    
    private void performRequest(HttpServletRequest request, List<FileItem> items) throws InternalErrorException, TransformerException{
	    
    	// 1. Create session file area
    	String fileArea = getFileArea(request.getSession());
        File fileAreaFile = new File(fileArea);
        fileAreaFile.mkdirs();
    	
        // 2. Write files to session file area
        String bpelFileName = writeFileToSessionFileArea(items, fileArea, request);
        
        // 3. Transform the file
        File target = transform(fileArea, bpelFileName);
        
        // 4. Convert the source and result to xhtml
        xhtmlConversion(fileArea + bpelFileName, target, request);
		
		// 5. Delete session file area
	    for (String file : fileAreaFile.list()) {
	    	new File(fileArea + file).delete();
	    }
	    fileAreaFile.delete();
    }
    
    private void performExampleRequest(HttpServletRequest request) throws InternalErrorException, TransformerException {
		String bpelFileName = request.getParameter("example");
		String fileArea = getServletContext().getRealPath("examples") + "/";
        File target = transform(fileArea, bpelFileName);

       	if(xmlConverter == null)
       		xmlConverter = new XmlToXhtmlConverter(getServletContext().getRealPath("/xmlverbatim/xmlverbatim.xsl"));

       	/* Convert source and target to xhtml, and save it in request attributes */
       	xhtmlConversion(fileArea + bpelFileName, target, request);
       	
		target.delete();
    }
    
    
	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse response)
	 */
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		if(request.getParameter("example")!=null){
			try {
				performExampleRequest(request);
				getServletConfig().getServletContext().getRequestDispatcher("/displayresult.jsp").forward(request,response);
			} catch (InternalErrorException e) {
				request.setAttribute("exception", e);
				getServletConfig().getServletContext().getRequestDispatcher("/displayerror.jsp").forward(request,response);
			} catch (TransformerException e) {
				request.setAttribute("exception", e);
				getServletConfig().getServletContext().getRequestDispatcher("/displayerror.jsp").forward(request,response);
			}			
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
			request.setAttribute("exception", new InternalErrorException(e, "File upload exception"));
			getServletConfig().getServletContext().getRequestDispatcher("/displayerror.jsp").forward(request,response);
		} catch (InternalErrorException e) {
			request.setAttribute("exception", e);
			getServletConfig().getServletContext().getRequestDispatcher("/displayerror.jsp").forward(request,response);
		} catch (TransformerException e) {
			request.setAttribute("exception", e);
			getServletConfig().getServletContext().getRequestDispatcher("/displayerror.jsp").forward(request,response);
		}
			
	}

}
