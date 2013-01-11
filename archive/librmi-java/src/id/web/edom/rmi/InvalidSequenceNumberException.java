package id.web.edom.rmi;

/**
 * <p>Thrown when the receiver receives a frame
 * with invalid sequence number.
 * For example, this is thrown
 * when the sequence number is out of range.</p>
 *
 * @author erik
 */
public class InvalidSequenceNumberException extends ProtocolException
{
    private static final long serialVersionUID = 1L;

    public InvalidSequenceNumberException (int s)
    {
        super(String.valueOf(s));
    }
}
