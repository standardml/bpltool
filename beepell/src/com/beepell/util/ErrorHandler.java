package com.beepell.util;

import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * A simple print stack trace and rethrow error handler.
 * @author Tim Hallwyl
 *
 */
public class ErrorHandler implements org.xml.sax.ErrorHandler {

    public void error(SAXParseException exception) throws SAXException {
        exception.printStackTrace();
        throw new SAXException(exception);        
    }

    public void fatalError(SAXParseException exception) throws SAXException {
        exception.printStackTrace();
        throw new SAXException(exception);        
    }

    public void warning(SAXParseException exception) throws SAXException {
        exception.printStackTrace();
        throw new SAXException(exception);        
    }

}
