package id.web.edom.rmi;

/**
 * <p>Thrown when the receiver receives a frame
 * with invalid method number.
 * Typically the object number is valid
 * but the object number is valid.</p>
 *
 * @author erik
 */
public class InvalidMethodNumberException extends RmiException
{
    private static final long serialVersionUID = 1L;

    public InvalidMethodNumberException (int number)
    {
        super(String.valueOf(number));
    }
}
