package com.beepell.deployment.analysis;

/**
 * Interface of error handler to handle violations of static analysis
 * requirements.
 * 
 * @author Tim Hallwyl
 */
public interface ErrorHandler {

    /**
     * An error is issued when static analysis cannot be performed, for example
     * because the document is invalid.
     * 
     * @param exception
     * @throws Exception
     */
    public void error(Exception exception) throws Exception;

    /**
     * A violation is issued when static analysis rejects the process
     * description, according to the requirments of the standard specification.
     * 
     * @param exception
     * @throws Exception
     */
    public void violation(Exception exception) throws Exception;

    /**
     * A warning is issued when static analysis finds non violating problems.
     * This may be possible runtime issues or theoretical issues.
     * 
     * @param exception
     * @throws Exception
     */
    public void warning(Exception exception) throws Exception;

}
