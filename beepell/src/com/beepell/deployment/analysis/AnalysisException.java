package com.beepell.deployment.analysis;

/**
 * Exception used be Static Analysis.
 * 
 * @author Tim Hallwyl
 */
public class AnalysisException extends Exception {

    private static final long serialVersionUID = 1L;
    private int rule = -1;

    /**
     * Default constructor. 
     */
    public AnalysisException() {
        super();
    }

    /**
     * Creates an exception with message and cause.
     * @param message
     * @param cause
     */
    public AnalysisException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Creates an exception with a message.
     * @param message
     */
    public AnalysisException(String message) {
        super(message);
    }

    /**
     * Creates an exception with a cause.
     * @param cause
     */
    public AnalysisException(Throwable cause) {
        super(cause);
    }

    /**
     * Creates an exception with a SA rule number.
     * 
     * @param rule Static analysis rule number
     */
    public AnalysisException(int rule) {
        super();
    }

    /**
     * Creates an exception with a SA rule number, message and cause.
     * 
     * @param rule Static analysis rule number
     * @param message
     * @param cause
     */
    public AnalysisException(int rule, String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Creates an exception with a SA rule number and message.
     * 
     * @param rule Static analysis rule number
     * @param message
     */
    public AnalysisException(int rule, String message) {
        super(message);
    }


    /**
     * Creates an exception with a SA rule number and cause.
     * 
     * @param rule Static analysis rule number
     * @param cause
     */
    public AnalysisException(int rule, Throwable cause) {
        super(cause);
    }

    /**
     * The the SA rule number that issued this exception.
     * 
     * @return -1 if this exception is not issued on basis of a SA rule.
     */
    public int getRule() {
        return rule;
    }

}
