package dk.itu.bpl.dix.reactivexml;

public class RewriteException extends Exception {
  public RewriteException(Throwable cause) {
    super(cause);
  }

  public RewriteException(String msg) {
    super(msg);
  }

  public RewriteException(String msg, Throwable cause) {
    super(msg, cause);
  }
}
