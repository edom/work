package id.web.edom.rmi;

/**
 * <p>Thrown when the receiver detects
 * a violation of the protocol specification.</p>
 *
 * @author erik
 */
public class ProtocolException extends RmiException
{
    private static final long serialVersionUID = 1L;

    public ProtocolException ()
    {
        super();
    }

    public ProtocolException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public ProtocolException (String message)
    {
        super(message);
    }

    public ProtocolException (Throwable cause)
    {
        super(cause);
    }
}
