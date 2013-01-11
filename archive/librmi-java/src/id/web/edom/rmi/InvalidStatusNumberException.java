package id.web.edom.rmi;

/**
 * <p>Thrown when the receiver receives a frame
 * with invalid status number.</p>
 *
 * @author erik
 */
public class InvalidStatusNumberException extends ProtocolException
{
    private static final long serialVersionUID = 1L;
    public InvalidStatusNumberException (int status)
    {
        super(String.valueOf(status));
    }
}
