package id.web.edom.rmi;

/**
 * <p>Thrown when an endpoint
 * receives a return frame
 * with invalid sequence number.</p>
 *
 * @author erik
 */
public class UnexpectedReturnFrameException extends InvalidSequenceNumberException
{
    private static final long serialVersionUID = 1L;

    public UnexpectedReturnFrameException (int s)
    {
        super(s);
    }
}
