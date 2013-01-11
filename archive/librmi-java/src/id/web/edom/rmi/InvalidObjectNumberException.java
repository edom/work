package id.web.edom.rmi;

/**
 * <p>Thrown when the receiver receives a frame
 * with invalid object number.
 * For example, an object number can be invalid
 * because it is not mapped to any object.</p>
 *
 * @author erik
 */
public class InvalidObjectNumberException extends RmiException
{
    private static final long serialVersionUID = 1L;

    public InvalidObjectNumberException (int number)
    {
        super(String.valueOf(number));
    }

}
