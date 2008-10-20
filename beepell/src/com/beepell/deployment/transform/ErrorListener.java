package com.beepell.deployment.transform;

import java.util.logging.Logger;

import javax.xml.transform.TransformerException;

import com.beepell.Settings;

/**
 * @author Tim Hallwyl
 *
 */
public class ErrorListener implements javax.xml.transform.ErrorListener {

    private static final Logger log = Settings.getLogger();
    
    /* (non-Javadoc)
     * @see javax.xml.transform.ErrorListener#error(javax.xml.transform.TransformerException)
     */
    @Override
    public void error(TransformerException exception) throws TransformerException {
        log.warning(exception.getLocalizedMessage());
        //throw exception;
    }

    /* (non-Javadoc)
     * @see javax.xml.transform.ErrorListener#fatalError(javax.xml.transform.TransformerException)
     */
    @Override
    public void fatalError(TransformerException exception) throws TransformerException {
        log.warning(exception.getLocalizedMessage());
        //throw exception;
    }

    /* (non-Javadoc)
     * @see javax.xml.transform.ErrorListener#warning(javax.xml.transform.TransformerException)
     */
    @Override
    public void warning(TransformerException exception) throws TransformerException {
        log.info(exception.getLocalizedMessage());
        //throw exception;
    }

}
