package xhtmltool;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;

import javax.xml.transform.*;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
public class XmlToXhtmlConverter {
	
	private Transformer transformer;
	
	public XmlToXhtmlConverter(String xsl_path) throws FileNotFoundException, TransformerConfigurationException{
		InputStream xslt = new FileInputStream(xsl_path);
		TransformerFactory tf = TransformerFactory.newInstance();
		transformer = tf.newTransformer(new StreamSource(xslt));
		transformer.setParameter("indent-elements", "yes");
	}
	
	public String XmlToXhtml(InputStream in) throws TransformerException
	{
		ByteArrayOutputStream s = new ByteArrayOutputStream();
		transformer.transform(new StreamSource(in), new StreamResult(s));
		byte[] out = s.toByteArray();
		return new String(out);	
	}
	
	
}
