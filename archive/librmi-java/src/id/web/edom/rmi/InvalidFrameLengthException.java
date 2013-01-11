package id.web.edom.rmi;

/**
 * <p>Thrown when the receiver receives a frame
 * that is smaller than the smallest possible frame.</p>
 *
 * @author erik
 */
public class InvalidFrameLengthException extends ProtocolException
{
    private static final long serialVersionUID = 1L;

    public InvalidFrameLengthException (int length)
    {
        super(String.valueOf(length));
    }
}
