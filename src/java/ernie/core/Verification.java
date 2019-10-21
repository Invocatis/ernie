package ernie.core.verify;

public class Verification {

  public enum Status {
    SUCCESS("SUCCESS"), ERROR("ERROR"), WARNING("WARNING");

    private String name;

    Status(String name) {
      this.name = name;
    }

    @Override
    public String toString() {
      return this.name;
    }
  }

  private Status status;
  private String message;

  public Verification(Status status, String message) {
    this.status = status;
    this.message = message;
  }

  public Status getStatus() {
    return status;
  }

  public String getMessage() {
    return message;
  }
}
