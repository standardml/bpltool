package errors;

public class InternalErrorException extends Exception {
	
	String shortMessage;
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public InternalErrorException(Exception e){
		super(e);
		this.shortMessage = "";
	}
	
	public InternalErrorException(Exception e, String shortMessage){
		super(e);
		this.shortMessage = shortMessage;		
	}
	
	public String getShortMessage(){
		return shortMessage;
	}
}
